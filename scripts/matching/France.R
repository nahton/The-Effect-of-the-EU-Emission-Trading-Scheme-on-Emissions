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

base_folder <- repo_path("PRTR France")
file_name <- "emissions.csv"

full_data <- data.frame()

for (year in 2003:2023) {
  file_path <- file.path(base_folder, as.character(year), file_name)
  
  if (file.exists(file_path)) {
    yearly_data <- read.csv(file_path, row.names=NULL, sep = ";")
    yearly_data$year <- year
    full_data <- bind_rows(full_data, yearly_data)
  } else {
    cat(sprintf("File not found for year %d\n", year))
  }
}



biomass <- full_data %>%
  filter(polluant %in% c("Dioxyde de carbone (CO2) d'origine biomasse")) %>%
  mutate(biomass = quantite) %>%
  mutate(biomass = ifelse(biomass > 0,biomass, 0)) %>%
  select(!c(polluant, quantite))

filtered_data <- full_data %>%
  filter(polluant %in% c("Dioxyde de carbone (CO2) total (d'origine biomasse et non biomasse)")) %>%
  mutate(total_emiss = quantite) %>%
  select(!c(polluant, quantite)) %>%
  left_join(biomass) %>%
  mutate(biomass = ifelse(is.na(biomass),0,biomass)) %>%
  mutate(ets_emissions = total_emiss - biomass) |>
  filter(!(identifiant %in% c("6502414", "5801770", "6400277"))) |>
  mutate(identifiant = as.character(identifiant)) |>
  rename(year = year)


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
  filter(year < 2013, substr(installation_id,1,2) == "FR") %>%
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
  filter(year >= 2005, substr(installation_id,1,2) == "FR") %>%
  group_by(installation_id) %>%
  summarize(any_data_from_2005 = any(!is.na(verified))) %>%
  filter(any_data_from_2005) %>%
  ungroup() |>
  rename(id = installation_id) |>
  inner_join(eutl_installations)


list_2005_ets_fr <- setdiff(unique(from_2005_entities$permitID), unique(post_2012_entities$permitID))
list_ets_fr <- c(unique(post_2012_entities$permitID))


filtered_data_ets <- filtered_data |> filter(year >= 2003, year <= 2023 & identifiant %in% c(list_2005_ets_fr)) |>
  mutate(ets = 1)


filtered_data_non_ets <- filtered_data |> filter(year >= 2003, year <= 2023 & !(identifiant %in% c(list_2005_ets_fr, list_ets_fr))) |> mutate(ets = 0)




fr_data <- bind_rows(filtered_data_ets, filtered_data_non_ets) |> mutate(ets_emissions = ets_emissions)






consistent <- fr_data %>% 
  filter(year >= 2003, year <= 2023)

entity_counts <- consistent %>% 
  group_by(identifiant) %>% 
  summarize(YearCount = n_distinct(year))

consistent_entities <- entity_counts %>% 
  filter(YearCount >= 0)

consistent_entity_list <- consistent_entities$identifiant



fr_data <- fr_data |> filter(identifiant %in% consistent_entity_list) |> na.omit() |>
  mutate(identifiant = paste0("FR", identifiant),
         country = substr(identifiant, 1,2)) |>
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
  select(-nace2dig_unfilled) |>
  mutate(ets_emissions = ets_emissions/1000) 
  
  

ets_info_plant <- fr_data %>%
  group_by(identifiant) %>%
  summarise(
    ets_new_plant = dplyr::case_when(
      any(ETS == 1, na.rm = TRUE) ~ 1L,
      all(ETS == 0, na.rm = TRUE) ~ 0L,
      TRUE ~ NA_integer_
    ),
    has_new_info = any(!is.na(ETS))
  )

fr_data <- fr_data %>%
  left_join(ets_info_plant, by = "identifiant") %>%
  mutate(
    ets = case_when(
      has_new_info & !is.na(ets_new_plant) ~ ets_new_plant,
      TRUE ~ ets
    )
  )

fr_data_0507 <- fr_data |>
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
  group_by(identifiant, ets, exact_match_sector) |>
  summarise(avg = mean(ets_emissions), .groups = "drop")

match_model <- run_matchit_variant(ets ~ avg, fr_data_0507, default_discard = "both")

matched_treated_units <- match.data(match_model, group = "treat") |> select(-dplyr::any_of(c("avg", "exact_match_sector")))
matched_control_units <- match.data(match_model, group = "control") |> select(-dplyr::any_of(c("avg", "exact_match_sector")))
matched_units <- bind_rows(matched_treated_units, matched_control_units)

did_data_fr <- matched_units |>
  group_by(identifiant, ets) |>
  inner_join(fr_data) |>
  mutate(
    treat = ifelse(ets == 1 & year >= 2008, 1, 0),
    did = ets * treat
  ) |>
  distinct()

did_data_fr <- mutate(did_data_fr, subclass = "FR")

message(
  "France matched sample: ", dplyr::n_distinct(did_data_fr$identifiant),
  " installations; ", nrow(did_data_fr), " plant-year rows."
)

