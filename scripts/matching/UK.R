rm(list = ls())

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

script_dir <- local({
  source_files <- vapply(sys.frames(), function(frame) frame$ofile %||% NA_character_, character(1))
  source_files <- stats::na.omit(source_files)
  if (length(source_files) > 0) {
    return(dirname(normalizePath(source_files[length(source_files)], winslash = "/", mustWork = TRUE)))
  }

  file.path("scripts", "matching")
})
source(file.path(dirname(script_dir), "_setup.R"))

set.seed(1111)

load_packages(c(
  "dplyr", "stringdist", "sf", "tibble", "MatchIt", "fuzzyjoin"
))

eutl_installations_raw <- read.csv(repo_path("General EU-ETS data", "EUTL Data (ETS-Installations).csv"),
                 header = TRUE, sep = ";", encoding = "UTF-8")

ets_compliance <- read.csv(repo_path("General EU-ETS data", "compliance.csv"),
                       header = TRUE, sep = ",", encoding = "UTF-8")

pts <- read.csv(repo_path("PRTR UK", "pointsources.csv"),
                header = TRUE, sep = ";", encoding = "UTF-8")

newukdata <- read.csv(repo_path("PRTR UK", "newukdata.csv"), sep = ";")

# Keep GB installations with positive verified ETS emissions.
eutl_real <- ets_compliance %>%
  filter(substr(installation_id, 1, 2) == "GB") %>%
  group_by(installation_id) %>%
  summarise(
    ever_nonzero = any(!is.na(verified) & verified > 0),
    sum_verified = sum(verified, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(ever_nonzero & sum_verified > 1000)

real_eutl_ids <- eutl_real$installation_id

# Prepare source data.
eutl_installations <- eutl_installations_raw %>%
  filter(substr(id, 1, 2) == "GB") %>%
  filter(id %in% real_eutl_ids)

full_prtr <- full_join(pts, newukdata, by = intersect(names(pts), names(newukdata)))

unique_prtr_plants <- full_prtr %>%
  select(PlantID, Easting, Northing, Site) %>%
  distinct() %>%
  mutate(PlantID = as.character(PlantID))


eutl_clean <- eutl_installations %>%
  mutate(
    latitude  = coalesce(latitudeGoogle,  latitudeEutl),
    longitude = coalesce(longitudeGoogle, longitudeEutl)
  ) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  mutate(id = as.character(id))


# Spatial objects.
eutl_sf <- st_as_sf(eutl_clean, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)

CRS_PRTR <- 27700 # British National Grid.
prtr_sf <- st_as_sf(unique_prtr_plants,
                    coords = c("Easting","Northing"),
                    crs = CRS_PRTR,
                    remove = FALSE)
prtr_sf_transformed <- st_transform(prtr_sf, crs = st_crs(eutl_sf))

# Emissions time series for PRTR and EUTL.
prtr_emis <- full_prtr %>%
  mutate(PlantID = as.character(PlantID)) %>%
  group_by(PlantID, Year) %>%
  summarise(
    prtr_emis = sum(Emission, na.rm = TRUE),
    .groups = "drop"
  )

eutl_emis <- ets_compliance %>%
  filter(substr(installation_id, 1, 2) == "GB") %>%
  filter(installation_id %in% real_eutl_ids) %>%
  mutate(EUTL_ID = as.character(installation_id),
         Year    = year) %>%
  group_by(EUTL_ID, Year) %>%
  summarise(
    eutl_emis = sum(verified, na.rm = TRUE),
    .groups = "drop"
  )

# Matching helpers.
`%||%` <- function(a,b) if (is.null(a) || (length(a)==1 && is.na(a))) b else a

name_clean <- function(x) {
  x <- tolower(x %||% "")
  x <- gsub("&", " and ", x)
  x <- gsub("[^a-z0-9 ]+", " ", x)
  x <- gsub("\\b(ltd|limited|plc|llp|inc|co|company|uk|gb|power station|power stn|power|station|site|works|plant|factory|unit|units|industrial estate|estate|site)\\b", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  x
}

token_set <- function(x) unique(unlist(strsplit(x, " +")))
jaccard_tokens <- function(a, b) {
  A <- token_set(a); B <- token_set(b)
  if (length(A)==0 && length(B)==0) return(1)
  if (length(A)==0 || length(B)==0)  return(0)
  inter <- length(intersect(A,B)); uni <- length(union(A,B))
  if (uni==0) return(0) else inter/uni
}

qgram_cosine <- function(a, b, q = 3) {
  qa <- stringdist::qgrams(a, q = q)
  qb <- stringdist::qgrams(b, q = q)
  allq <- union(names(qa), names(qb))
  va <- as.numeric(qa[allq]); va[is.na(va)] <- 0
  vb <- as.numeric(qb[allq]); vb[is.na(vb)] <- 0
  den <- sqrt(sum(va*va)) * sqrt(sum(vb*vb))
  if (den == 0) return(0)
  sum(va*vb)/den
}

to_3857 <- function(lon, lat) {
  pts <- st_as_sf(tibble(lon = lon, lat = lat), coords = c("lon","lat"), crs = 4326)
  as.matrix(st_coordinates(st_transform(pts, 3857)))
}

# Greedy one-to-one assignment.
assign_one_to_one <- function(df, score_col = "match_score") {
  df <- df %>%
    arrange(dplyr::desc(.data[[score_col]]))
  
  used_e <- new.env(hash = TRUE, parent = emptyenv())
  used_p <- new.env(hash = TRUE, parent = emptyenv())
  keep   <- logical(nrow(df))
  
  for (i in seq_len(nrow(df))) {
    e <- as.character(df$EUTL_ID[i])
    p <- as.character(df$PlantID[i])
    if (!exists(e, used_e, inherits = FALSE) &&
        !exists(p, used_p, inherits = FALSE)) {
      keep[i] <- TRUE
      assign(e, TRUE, envir = used_e)
      assign(p, TRUE, envir = used_p)
    }
  }
  df[keep, , drop = FALSE]
}

# Name-matching source tables.
prtr_names <- prtr_sf_transformed %>%
  st_drop_geometry() %>%
  transmute(
    PlantID = as.character(PlantID),
    PRTR_Site_Name_raw = Site %||% "",
    PRTR_Site_Name = name_clean(Site %||% "")
  ) %>% distinct()

eutl_names <- eutl_sf %>%
  st_drop_geometry() %>%
  transmute(
    EUTL_ID = as.character(id),
    EUTL_Name_raw = name %||% "",
    EUTL_Name = name_clean(name %||% ""),
    EUTL_lon = longitude, EUTL_lat = latitude
  ) %>% distinct()

# Candidate blocking.
prtr_tokens <- prtr_names %>%
  mutate(first = substr(PRTR_Site_Name, 1, 1)) %>%
  mutate(tokens = strsplit(PRTR_Site_Name, " "))

eutl_tokens <- eutl_names %>%
  mutate(first = substr(EUTL_Name, 1, 1)) %>%
  mutate(tokens = strsplit(EUTL_Name, " "))

cand_block <- prtr_tokens %>%
  select(PlantID, PRTR_Site_Name, PRTR_Site_Name_raw, first, tokens) %>%
  fuzzyjoin::stringdist_inner_join(
    eutl_tokens %>% select(EUTL_ID, EUTL_Name, EUTL_Name_raw, first, tokens),
    by = c("first" = "first"),
    method = "jw", max_dist = 0.5, distance_col = "first_dist"
  ) %>%
  select(PlantID, PRTR_Site_Name, PRTR_Site_Name_raw,
         EUTL_ID, EUTL_Name, EUTL_Name_raw)

# Name scores.
cand_scored <- cand_block %>%
  rowwise() %>%
  mutate(
    jw  = 1 - stringdist(PRTR_Site_Name, EUTL_Name, method = "jw"),
    jac = jaccard_tokens(PRTR_Site_Name, EUTL_Name),
    qco = qgram_cosine(PRTR_Site_Name, EUTL_Name, q = 3),
    name_score = 0.6*jw + 0.25*jac + 0.15*qco
  ) %>% ungroup()

# Distances.
prtr_xy <- prtr_sf_transformed %>%
  mutate(PlantID = as.character(PlantID),
         PRTR_lon = st_coordinates(prtr_sf_transformed)[,1],
         PRTR_lat = st_coordinates(prtr_sf_transformed)[,2]) %>%
  select(PlantID, PRTR_lon, PRTR_lat)

eutl_xy <- eutl_sf %>%
  mutate(EUTL_ID = as.character(id),
         EUTL_lon = st_coordinates(eutl_sf)[,1],
         EUTL_lat = st_coordinates(eutl_sf)[,2]) %>%
  select(EUTL_ID, EUTL_lon, EUTL_lat)

cand_scored <- cand_scored %>%
  left_join(prtr_xy, by = "PlantID") %>%
  left_join(eutl_xy, by = "EUTL_ID")

coords_prtr <- to_3857(cand_scored$PRTR_lon, cand_scored$PRTR_lat)
coords_eutl <- to_3857(cand_scored$EUTL_lon, cand_scored$EUTL_lat)
cand_scored$Distance_m <- sqrt((coords_prtr[,1]-coords_eutl[,1])^2 +
                                 (coords_prtr[,2]-coords_eutl[,2])^2)

cand_scored <- cand_scored %>%
  mutate(
    dist_bonus = pmax(0, 1 - pmin(Distance_m, 1500)/1500) * 0.10,
    base_score = pmin(1, name_score + dist_bonus)
  )

# Emissions safeguards.
emis_pairs <- cand_scored %>%
  distinct(PlantID, EUTL_ID) %>%
  left_join(prtr_emis, by = "PlantID") %>%
  left_join(eutl_emis, by = c("EUTL_ID","Year")) %>%
  group_by(PlantID, EUTL_ID) %>%
  summarise(
    overlap_years = sum(!is.na(prtr_emis) & !is.na(eutl_emis)),
    emis_cor = {
      x <- log1p(prtr_emis)
      y <- log1p(eutl_emis)
      if (overlap_years >= 3 &&
          sd(x, na.rm = TRUE) > 0 &&
          sd(y, na.rm = TRUE) > 0) {
        cor(x, y, use = "complete.obs")
      } else NA_real_
    },
    total_prtr = sum(prtr_emis, na.rm = TRUE),
    total_eutl = sum(eutl_emis, na.rm = TRUE),
    emis_ratio_total = ifelse(
      total_prtr > 0 & total_eutl > 0,
      total_prtr / total_eutl,
      NA_real_
    ),
    log10_diff = ifelse(
      total_prtr > 0 & total_eutl > 0,
      abs(log10(total_prtr) - log10(total_eutl)),
      NA_real_
    ),
    .groups = "drop"
  )

cand_scored <- cand_scored %>%
  left_join(emis_pairs, by = c("PlantID","EUTL_ID")) %>%
  mutate(
    emis_bonus_raw = case_when(
      # Overlapping years with correlated emissions.
      !is.na(emis_cor) & overlap_years >= 3 & emis_cor >= 0.85 ~ 0.10,
      !is.na(emis_cor) & overlap_years >= 3 & emis_cor >= 0.60 ~ 0.05,
      !is.na(emis_cor) & overlap_years >= 3 & emis_cor <= 0    ~ -0.15,
      
      # Limited overlap, but totals in the same order of magnitude.
      (overlap_years < 3 | is.na(emis_cor)) &
        !is.na(log10_diff) & log10_diff <= 0.3 ~ 0.05,
      (overlap_years < 3 | is.na(emis_cor)) &
        !is.na(log10_diff) & log10_diff <= 0.5 ~ 0.02,
      
      # Totals far apart.
      (overlap_years < 3 | is.na(emis_cor)) &
        !is.na(log10_diff) & log10_diff >= 1   ~ -0.10,
      
      TRUE ~ 0
    ),
    emis_bonus = if_else(name_score >= 0.95 & emis_bonus_raw < 0,
                         0, emis_bonus_raw),
    emis_bonus = pmax(pmin(emis_bonus, 0.10), -0.20),
    match_score = pmin(1, base_score + emis_bonus)
  )

# Keep the best row for each PRTR-EUTL candidate pair.
cand_scored <- cand_scored %>%
  group_by(PlantID, EUTL_ID) %>%
  slice_max(match_score, n = 1, with_ties = FALSE) %>%
  ungroup()

# Greedy one-to-one matching.
matched_eutl <- assign_one_to_one(
  cand_scored %>%
    select(PlantID, EUTL_ID,
           PRTR_Site_Name_raw, EUTL_Name_raw,
           Distance_m, name_score, match_score,
           overlap_years, emis_cor, emis_ratio_total, log10_diff),
  score_col = "match_score"
)

# Final algorithmic matches.
matched_best <- matched_eutl %>%
  filter(match_score >= 0.5) %>%
  distinct(EUTL_ID, .keep_all = TRUE) %>%
  transmute(
    PRTR_PlantID   = PlantID,
    PRTR_Site_Name = PRTR_Site_Name_raw,
    EUTL_ID,
    EUTL_Name      = EUTL_Name_raw,
    Distance_m,
    name_score,
    match_score,
    overlap_years,
    emis_cor,
    emis_ratio_total,
    log10_diff
  )


# Apply manual one-to-one overrides.
manual_matches_raw <- read.csv(
  repo_path("PRTR UK", "EUTL_PRTR_Manual_Check_final_1to1.csv"),
  stringsAsFactors = FALSE, sep = ";"
) |>
  filter(manual == 1) |>
  mutate(PRTR_PlantID = as.character(PRTR_PlantID))

if (nrow(manual_matches_raw) > 0) {
  matched_best <- matched_best %>%
    filter(!EUTL_ID %in% manual_matches_raw$EUTL_ID) %>%
    bind_rows(manual_matches_raw)
}

# ETS entry classification.
ets_status_raw <- ets_compliance %>%
  filter(substr(installation_id, 1, 2) == "GB") %>%
  filter(installation_id %in% real_eutl_ids) %>%
  group_by(installation_id) %>%
  summarise(
    is_active = any(!is.na(verified) & year >= 2005),
    has_data_pre_2013 = any(!is.na(verified) & year < 2013),
    .groups = "drop"
  ) %>%
  filter(is_active)

ets_classification <- ets_status_raw %>%
  mutate(
    ETS_Start_Year = if_else(has_data_pre_2013, 2005L, 2013L)
  ) %>%
  select(EUTL_ID = installation_id, ETS_Start_Year) %>%
  mutate(EUTL_ID = as.character(EUTL_ID))

final_ets_classification <- ets_classification %>%
  left_join(eutl_installations %>% select(id, name), by = c("EUTL_ID" = "id")) %>%
  rename(EUTL_Name = name)

# Attach ETS information to the PRTR panel.
full_prtr_with_ets <- full_prtr %>%
  mutate(PlantID = as.character(PlantID)) %>%
  left_join(
    matched_best %>% select(PRTR_PlantID, EUTL_Name),
    by = c("PlantID" = "PRTR_PlantID")
  ) %>%
  mutate(ETS = if_else(!is.na(EUTL_Name), 1L, 0L))

final_correspondence_table <- matched_best %>%
  left_join(final_ets_classification %>% select(EUTL_ID, ETS_Start_Year, EUTL_Name),
            by = c("EUTL_ID","EUTL_Name")) %>%
  select(PRTR_PlantID, EUTL_ID, EUTL_Name, ETS_Start_Year)

full_prtr_final <- full_prtr_with_ets %>%
  left_join(
    final_correspondence_table %>% select(PRTR_PlantID, ETS_Start_Year, EUTL_ID),
    by = c("PlantID" = "PRTR_PlantID")
  )


original_ets_plants <- full_prtr_final %>% filter(ETS_Start_Year == 2005)
non_ets_plants <- full_prtr_final %>% filter(ETS == 0 | is.na(ETS))

gb_data <- bind_rows(original_ets_plants, non_ets_plants) %>%
  rename(year = Year, ets_emissions = Emission, ets = ETS) %>%
  filter(year >= 2000) %>%
  mutate(
    EUTL_ID = as.character(EUTL_ID),
    PlantID = as.character(PlantID),
    identifiant = as.character(coalesce(EUTL_ID, PlantID))
  )

large_emitter_ets_plants <- gb_data |> group_by(PlantID, ets) |> summarise(ets_emissions = mean(ets_emissions)) |> mutate(ets = ifelse(ets_emissions >= 300000, 1, ets)) |>
  filter(ets == 1)

sector_map <- read.csv(repo_path("PRTR UK", "sector_correspondence_uk_prtr_to_panel.csv")) |> rename(Sector = UK_PRTR_Sector,
                                                                                                                      sector_group = Full_panel_sector)


gb_data <- gb_data |> left_join(sector_map)

eutl_match_to_check <- gb_data |> group_by(EUTL_Name) |> filter(year <= 2007) |> select(EUTL_Name) |> distinct()

accepted_pre_treatment_matches <- matched_best |> filter(EUTL_Name %in% eutl_match_to_check$EUTL_Name) |> filter(name_score >= 0.5 & Distance_m <= 10000 |
                                                                                           name_score >= 0.5 & emis_cor >= 0.75 |
                                                                                           emis_cor >= 0.75 & Distance_m <= 10000 |
                                                                                           manual == 1)


gb_data <- gb_data |> mutate(ets = ifelse((EUTL_Name %in% accepted_pre_treatment_matches$EUTL_Name), 1, ets)) |> mutate(ets = ifelse(PlantID %in% large_emitter_ets_plants$PlantID, 1, ets))


gb_data_0507 <- gb_data |>
  filter(year >= 2003, year <= 2007) |>
  mutate(
    exact_match_sector = if (Sys.getenv("ETS_UK_EXACT_MATCH_LEVEL", unset = "original_sector") == "sector_group") {
      dplyr::coalesce(as.character(sector_group), "missing_sector_group")
    } else {
      dplyr::coalesce(as.character(Sector), "missing_original_sector")
    }
  ) |>
  group_by(PlantID, ets, exact_match_sector) |>
  summarise(avg = mean(ets_emissions), .groups = "drop")

match_model <- run_matchit_variant(ets ~ avg, gb_data_0507, default_discard = "both")



matched_treated_units <- match.data(match_model, group = "treat") |> select(-dplyr::any_of(c("avg", "exact_match_sector")))

matched_control_units <- match.data(match_model, group = "control") |> select(-dplyr::any_of(c("avg", "exact_match_sector")))

matched_units <- bind_rows(matched_treated_units, matched_control_units)


did_data <- gb_data %>%
  inner_join(matched_units, by = c("PlantID", "ets")) %>%
  filter(!is.na(ets_emissions)) %>%
  mutate(
    treat = ifelse(ets == 1 & year >= 2008, 1, 0),
    did   = ets * treat)


did_data_gb <- did_data

message(
  "United Kingdom matched sample: ", dplyr::n_distinct(did_data_gb$identifiant),
  " installations; ", nrow(did_data_gb), " plant-year rows."
)

