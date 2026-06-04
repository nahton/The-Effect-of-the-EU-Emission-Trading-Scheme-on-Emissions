`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

script_dir <- local({
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(script_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = FALSE)))
  }

  source_files <- vapply(sys.frames(), function(frame) frame$ofile %||% NA_character_, character(1))
  source_files <- stats::na.omit(source_files)
  if (length(source_files) > 0) {
    return(dirname(normalizePath(source_files[length(source_files)], winslash = "/", mustWork = TRUE)))
  }

  "scripts"
})
source(file.path(script_dir, "_setup.R"))

load_packages(c(
  "broom", "dplyr", "fixest", "purrr", "readr", "stringr", "tibble", "tidyr"
))

save_table <- function(data, name) {
  readr::write_csv(data, output_path("tables", paste0(name, ".csv")))
}

format_term <- function(model, term) {
  broom::tidy(model) |>
    dplyr::filter(.data$term == .env$term) |>
    dplyr::transmute(
      estimate = .data$estimate,
      std_error = .data$std.error,
      statistic = .data$statistic,
      p_value = .data$p.value,
      observations = stats::nobs(model)
    )
}

did_data_total <- read_total_data() |>
  dplyr::mutate(
    sector_group = as.factor(.data$sector_group),
    country = as.factor(.data$country)
  )

# Table 5, matched observations by harmonized sector and treatment status.
table_5 <- did_data_total |>
  dplyr::mutate(ets_group = dplyr::if_else(.data$ets == 1, "Treated", "Control")) |>
  dplyr::count(.data$sector_group, .data$ets_group, name = "observations") |>
  tidyr::pivot_wider(
    names_from = "ets_group",
    values_from = "observations",
    values_fill = 0
  ) |>
  dplyr::mutate(
    Total = .data$Control + .data$Treated,
    percent_treated = 100 * .data$Treated / .data$Total
  ) |>
  dplyr::arrange(dplyr::desc(.data$Total))
save_table(table_5, "table_5_sector_distribution")

# Table 6, continuous heterogeneity by average pre-treatment emissions.
pre_emissions <- did_data_total |>
  dplyr::filter(.data$year >= 2003, .data$year <= 2007) |>
  dplyr::group_by(.data$identifiant) |>
  dplyr::summarise(
    avg_pre_emissions = mean(.data$ets_emissions, na.rm = TRUE),
    .groups = "drop"
  )

did_size <- did_data_total |>
  dplyr::left_join(pre_emissions, by = "identifiant") |>
  dplyr::filter(!is.na(.data$avg_pre_emissions), .data$avg_pre_emissions > 0) |>
  dplyr::mutate(
    ets_emissions_50k = .data$ets_emissions / 50000,
    log10_pre_emissions_50k = log10(.data$avg_pre_emissions / 50000)
  )

size_two_way <- fixest::fepois(
  ets_emissions_50k ~ did + did:log10_pre_emissions_50k | identifiant + year,
  data = did_size,
  weights = ~weights,
  vcov = "hetero"
)
size_sector_year <- fixest::fepois(
  ets_emissions_50k ~ did + did:log10_pre_emissions_50k | identifiant + sector_group^year,
  data = did_size,
  weights = ~weights,
  vcov = "hetero"
)
size_country_year <- fixest::fepois(
  ets_emissions_50k ~ did + did:log10_pre_emissions_50k | identifiant + country^year,
  data = did_size,
  weights = ~weights,
  vcov = "hetero"
)

table_6 <- purrr::imap_dfr(
  list(
    "Two-way FE" = size_two_way,
    "Sector x Year FE" = size_sector_year,
    "Country x Year FE" = size_country_year
  ),
  function(model, specification) {
    dplyr::bind_rows(
      format_term(model, "did") |>
        dplyr::mutate(term_label = "ETS effect at 50,000 tCO2"),
      format_term(model, "did:log10_pre_emissions_50k") |>
        dplyr::mutate(term_label = "ETS x log10(pre-treatment emissions / 50,000)")
    ) |>
      dplyr::mutate(
        specification = .env$specification,
        installations = length(fixest::fixef(model)$identifiant),
        .before = 1
      )
  }
)
save_table(table_6, "table_6_size_heterogeneity_continuous")

message("Wrote appendix outputs to: ", output_path("tables"))
