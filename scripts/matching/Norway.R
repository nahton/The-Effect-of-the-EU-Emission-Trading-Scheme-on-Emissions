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

load_packages(c(
  "dplyr", "tidyr", "haven", "MatchIt"
))

prior_paper_panel_long <- read_all_panel_long()


prtr_no <- read.csv(repo_path("PRTR Norway", "PRTR Norway.csv"),
                    header = TRUE,
                    sep = ";",
                    fileEncoding = "Windows-1252") |>
  rename(facility = Anleggsnavn,
         id = AnleggId,
         orgid = "Org.nr.",
         emissions = ets_emissions) |>
  select(year, id, orgid, facility, emissions) |>
  filter(year >= 2000 & year < 2024) |>
  mutate(emissions = emissions*1000,
         id = paste("NO_", id, sep = ""))

eutl_installations <- read.csv(repo_path("General EU-ETS data", "EUTL Data (ETS-Installations).csv"),
                 header = TRUE,
                 sep = ";",
                 encoding = "UTF-8")

ets_compliance <- read.csv(repo_path("General EU-ETS data", "compliance.csv"),
                       header = TRUE,
                       sep = ",",
                       encoding = "UTF-8")

# ETS installations with no verified emissions before Phase 3.
pre_2013_NA_entities <- ets_compliance %>%
  filter(year < 2013, substr(installation_id,1,2) == "NO") %>%
  group_by(installation_id) %>%
  summarize(all_NA_pre_2013 = all(is.na(verified))) %>%
  filter(all_NA_pre_2013) %>%
  ungroup()

# Installations that begin reporting from 2013 onward.
post_2012_entities <- ets_compliance %>%
  filter(year >= 2013, installation_id %in% pre_2013_NA_entities$installation_id) %>%
  group_by(installation_id) %>%
  summarize(any_data_post_2012 = any(!is.na(verified))) %>%
  filter(any_data_post_2012) %>%
  ungroup() |>
  rename(id = installation_id) |>
  inner_join(eutl_installations)

# Installations with verified emissions from the original ETS period.
from_2005_entities <- ets_compliance %>%
  filter(year >= 2005, substr(installation_id,1,2) == "NO") %>%
  group_by(installation_id) %>%
  summarize(any_data_from_2005 = any(!is.na(verified))) %>%
  filter(any_data_from_2005) %>%
  ungroup() |>
  rename(id = installation_id) |>
  inner_join(eutl_installations)

list_2005_ets_no <- setdiff(unique(from_2005_entities$name), unique(post_2012_entities$name))
list_ets_no <- c(unique(post_2012_entities$name))


matched <- read.csv(repo_path("PRTR Norway", "Matched_Facilities.csv"),
                    sep = ";",
                    header = TRUE) |>
  mutate(manual_reject = .data$X == "x") |>
  filter(!.data$manual_reject) |>
  filter(.data$PRTR_Name != "NA") |>
  unique()

prtr_no <- left_join(prtr_no, matched, by = c("facility" = "PRTR_Name"))

prtr_no <- prtr_no |> mutate(ets = ifelse(is.na(ETS_Name), 0, 1)) |> filter(ETS_Name %in% list_2005_ets_no | is.na(ETS_Name))


no_data <- prtr_no |> select(year, id, facility, ets, emissions, ETS_Name) |> unique() |>
  mutate(country = substr(id, 1,2),
         identifiant = gsub("_", "", id)) |>
  left_join(prior_paper_panel_long) |>
  mutate(nace2dig_unfilled = nace2dig) |>
  group_by(identifiant) %>%
  fill(sector, .direction = "downup") |>
  mutate(sector = as.factor(ifelse(is.na(sector), 999, sector))) |>
  fill(nace2dig, .direction = "downup") |>
  mutate(nace2dig_exact_matching = as.factor(ifelse(is.na(nace2dig), 999, nace2dig))) |>
  mutate(nace2dig = if_else(is.na(nace2dig_unfilled), NA_character_, as.character(nace2dig))) |>
  mutate(
    nace2 = as.integer(nace2dig),
    sector_group = case_when(
      nace2 %in% c(35, 38)            ~ "Power & heat (combustion)",
      nace2 %in% c(19, 20)            ~ "Refineries & chemicals",
      nace2 == 23                     ~ "Cement & minerals",
      nace2 == 24                     ~ "Metals",
      nace2 %in% c(16, 17)            ~ "Pulp, paper & wood",
      nace2 %in% c(10, 11)            ~ "Food & beverages",
      TRUE                            ~ "Other manufacturing"
    )
  ) |>
  select(-nace2dig_unfilled)
  

ets_info_plant <- no_data %>%
  group_by(id) %>%
  summarise(
    ets_new_plant = dplyr::case_when(
      any(ETS == 1, na.rm = TRUE) ~ 1L,
      all(ETS == 0, na.rm = TRUE) ~ 0L,
      TRUE ~ NA_integer_
    ),
    has_new_info = any(!is.na(ETS))
  )

no_data <- no_data %>%
  left_join(ets_info_plant, by = "id") %>%
  mutate(
    ets = case_when(
      has_new_info & !is.na(ets_new_plant) ~ ets_new_plant,
      TRUE ~ ets
    )
  )

no_data_0507 <- no_data |>
  filter(year >= 2003, year <= 2007) |>
  mutate(
    exact_match_sector = if (Sys.getenv("ETS_EXACT_MATCH_LEVEL", unset = "paper") == "sector_group") {
      as.character(sector_group)
    } else if (Sys.getenv("ETS_EXACT_MATCH_LEVEL", unset = "paper") == "prior_sector") {
      as.character(sector)
    } else {
      as.character(nace2dig_exact_matching)
    }
  ) |>
  group_by(id, ets, exact_match_sector) |>
  summarise(avg = mean(emissions), .groups = "drop")

match_model <- run_matchit_variant(ets ~ avg, no_data_0507, default_discard = "none")

matched_treated_units <- match.data(match_model, group = "treat") |> select(-dplyr::any_of(c("avg", "exact_match_sector")))
matched_control_units <- match.data(match_model, group = "control") |> select(-dplyr::any_of(c("avg", "exact_match_sector")))
matched_units <- bind_rows(matched_treated_units, matched_control_units)

did_data <- matched_units |>
  group_by(id, ets) |>
  inner_join(no_data) |>
  mutate(
    treat = ifelse(ets == 1 & year >= 2008, 1, 0),
    did = ets * treat
  )

did_data_no <- if ("subclass" %in% names(did_data)) {
  mutate(did_data, subclass = paste0("NO", subclass))
} else {
  mutate(did_data, subclass = NA_character_)
}

message(
  "Norway matched sample: ", dplyr::n_distinct(did_data_no$identifiant),
  " installations; ", nrow(did_data_no), " plant-year rows."
)

