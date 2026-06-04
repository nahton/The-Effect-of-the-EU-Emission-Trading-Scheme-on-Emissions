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
  "broom", "dplyr", "fixest", "ggplot2", "purrr", "readr", "scales",
  "stringr", "tibble", "tidyr"
))

format_estimate <- function(model, term = "did") {
  broom::tidy(model) |>
    dplyr::filter(.data$term == term) |>
    dplyr::transmute(
      estimate = .data$estimate,
      std_error = .data$std.error,
      statistic = .data$statistic,
      p_value = .data$p.value,
      observations = stats::nobs(model)
    )
}

model_installations <- function(model) {
  length(fixest::fixef(model)$identifiant)
}

save_table <- function(data, name) {
  readr::write_csv(data, output_path("tables", paste0(name, ".csv")))
}

did_data_total <- read_total_data()

# Table 1, matched sample.
table_1_matched <- did_data_total |>
  dplyr::group_by(country, ets) |>
  dplyr::summarise(
    installations = dplyr::n_distinct(identifiant),
    observations = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    country = dplyr::recode(
      country,
      FR = "France",
      GB = "United Kingdom",
      NL = "Netherlands",
      NO = "Norway"
    ),
    ets = dplyr::recode(as.character(ets), `0` = "Non-ETS", `1` = "ETS")
  )
save_table(table_1_matched, "table_1_matched_sample")

# Table 2, baseline PPML specifications.
ppml_year <- fixest::fepois(
  ets_emissions ~ did | identifiant + year,
  data = did_data_total,
  weights = ~weights,
  vcov = "hetero"
)
ppml_sector_year <- fixest::fepois(
  ets_emissions ~ did | identifiant + sector_year,
  data = did_data_total,
  weights = ~weights,
  vcov = "hetero"
)
ppml_country_year <- fixest::fepois(
  ets_emissions ~ did | identifiant + country_year,
  data = did_data_total,
  weights = ~weights,
  vcov = "hetero"
)

table_2 <- dplyr::bind_rows(
  format_estimate(ppml_year) |> dplyr::mutate(specification = "Year FE"),
  format_estimate(ppml_sector_year) |> dplyr::mutate(specification = "Sector x Year FE"),
  format_estimate(ppml_country_year) |> dplyr::mutate(specification = "Country x Year FE")
) |>
  dplyr::mutate(
    installations = purrr::map_int(
      list(ppml_year, ppml_sector_year, ppml_country_year),
      model_installations
    ),
    percent_effect = exp(estimate) - 1
  ) |>
  dplyr::select(specification, estimate, std_error, percent_effect, installations, observations)
save_table(table_2, "table_2_baseline_ppml")

capture.output(
  fixest::etable(
    ppml_year, ppml_sector_year, ppml_country_year,
    se = "hetero",
    dict = c(did = "ETS Effect (DiD)"),
    fitstat = ~ n + ll
  ),
  file = output_path("tables", "table_2_baseline_ppml.txt")
)

# Table 3, phase-specific effects.
phase_data <- did_data_total |>
  dplyr::mutate(
    phase = dplyr::case_when(
      year >= 2008 & year <= 2012 ~ "Phase 2 (2008-2012)",
      year >= 2013 & year <= 2020 ~ "Phase 3 (2013-2020)",
      year >= 2021 & year <= 2023 ~ "Phase 4 (2021-2023)",
      TRUE ~ "Pre-treatment"
    ),
    phase = factor(
      phase,
      levels = c(
        "Pre-treatment",
        "Phase 2 (2008-2012)",
        "Phase 3 (2013-2020)",
        "Phase 4 (2021-2023)"
      )
    )
  )

ppml_phase <- fixest::fepois(
  ets_emissions ~ phase * ets | identifiant + country_year,
  data = phase_data,
  weights = ~weights,
  vcov = "hetero"
)
table_3 <- broom::tidy(ppml_phase) |>
  dplyr::filter(stringr::str_detect(term, "^phase.*:ets$")) |>
  dplyr::mutate(
    phase = stringr::str_remove(term, "^phase"),
    phase = stringr::str_remove(phase, ":ets$"),
    percent_effect = exp(estimate) - 1
  ) |>
  dplyr::select(phase, estimate, std_error = std.error, percent_effect, p_value = p.value)
save_table(table_3, "table_3_phase_effects")

# Table 8, sector heterogeneity.
ppml_sector <- fixest::fepois(
  ets_emissions ~ did:sector_group | identifiant + year,
  data = did_data_total,
  weights = ~weights,
  vcov = "hetero"
)
table_8 <- broom::tidy(ppml_sector) |>
  dplyr::filter(stringr::str_detect(term, "^did:sector_group")) |>
  dplyr::mutate(
    sector = stringr::str_remove(term, "^did:sector_group"),
    percent_effect = exp(estimate) - 1
  ) |>
  dplyr::select(sector, estimate, std_error = std.error, statistic, p_value = p.value, percent_effect)
save_table(table_8, "table_8_sector_effects")

# Tables 9 and 10, country-specific PPML and OLS estimates.
country_names <- c(FR = "France", NL = "Netherlands", NO = "Norway", GB = "United Kingdom")

country_ppml <- purrr::imap_dfr(country_names, function(label, code) {
  country_data <- dplyr::filter(did_data_total, country == code)
  model <- fixest::fepois(
    ets_emissions ~ did | identifiant + year,
    data = country_data,
    weights = ~weights,
    vcov = "hetero"
  )
  format_estimate(model) |>
    dplyr::mutate(country = label, installations = model_installations(model))
})
save_table(country_ppml, "table_9_country_ppml")

country_ols <- purrr::imap_dfr(country_names, function(label, code) {
  country_data <- dplyr::filter(did_data_total, country == code)
  model <- fixest::feols(
    log(ets_emissions) ~ did | identifiant + year,
    data = country_data,
    weights = ~weights,
    vcov = "hetero"
  )
  format_estimate(model) |>
    dplyr::mutate(country = label, installations = model_installations(model))
})
save_table(country_ols, "table_10_country_ols")

# Figure 4, yearly treatment effects.
event_data <- did_data_total |>
  dplyr::mutate(
    ets = factor(ets, levels = c(0, 1)),
    year_factor = stats::relevel(factor(year), ref = "2007")
  )

log_yearly <- fixest::feols(
  log(ets_emissions) ~ year_factor * ets | identifiant,
  data = event_data,
  weights = ~weights
)

event_effects <- broom::tidy(log_yearly, conf.int = TRUE) |>
  dplyr::filter(stringr::str_detect(term, "^year_factor[0-9]{4}:ets1$")) |>
  dplyr::mutate(year = as.integer(stringr::str_extract(term, "[0-9]{4}"))) |>
  dplyr::bind_rows(tibble::tibble(year = 2007, estimate = 0, conf.low = 0, conf.high = 0)) |>
  dplyr::arrange(year) |>
  dplyr::mutate(
    pct = (exp(estimate) - 1) * 100,
    pct_lo = (exp(conf.low) - 1) * 100,
    pct_hi = (exp(conf.high) - 1) * 100
  ) |>
  dplyr::filter(year >= 2007)
save_table(event_effects, "figure_4_yearly_treatment_effects")

figure_4 <- ggplot2::ggplot(event_effects, ggplot2::aes(x = year, y = pct)) +
  ggplot2::geom_point(color = "purple", size = 3) +
  ggplot2::geom_line(color = "purple", linewidth = 1) +
  ggplot2::geom_hline(
    yintercept = seq(-50, 20, by = 10),
    linetype = "dashed",
    color = "grey80",
    linewidth = 0.3
  ) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = pct_lo, ymax = pct_hi), width = 0.2, color = "purple") +
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  ggplot2::geom_vline(xintercept = 2007.5, linetype = "dashed", color = "red") +
  ggplot2::annotate(
    "text", x = 2007.5, y = 15, label = "Treatment\n(2008)",
    angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
  ) +
  ggplot2::geom_vline(xintercept = 2012.5, linetype = "dashed", color = "red") +
  ggplot2::annotate(
    "text", x = 2012.5, y = 15, label = "Phase 3\n(2013)",
    angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
  ) +
  ggplot2::geom_vline(xintercept = 2020.5, linetype = "dashed", color = "red") +
  ggplot2::annotate(
    "text", x = 2020.5, y = 15, label = "Phase 4\n(2021)",
    angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
  ) +
  ggplot2::coord_cartesian(ylim = c(-70, 20)) +
  ggplot2::scale_x_continuous(breaks = seq(2008, 2023, by = 2)) +
  ggplot2::scale_y_continuous(breaks = seq(-70, 20, 10), labels = function(x) paste0(x, "%")) +
  ggplot2::labs(x = "Year", y = "% Change in Emissions") +
  ggplot2::theme_classic(base_size = 14) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

ggplot2::ggsave(
  output_path("plots", "Treatment_effect_over_time.png"),
  figure_4,
  width = 10,
  height = 7,
  dpi = 600
)

# Figures 3 and 5, predicted emissions for the matched panel.
coef_yearly <- broom::tidy(log_yearly, conf.int = TRUE)
beta <- coef_yearly |>
  dplyr::filter(stringr::str_detect(term, "^year_factor[0-9]{4}$")) |>
  dplyr::mutate(year = as.integer(stringr::str_extract(term, "[0-9]{4}"))) |>
  dplyr::select(year, beta = estimate)

gamma <- coef_yearly |>
  dplyr::filter(stringr::str_detect(term, "^year_factor[0-9]{4}:ets1$")) |>
  dplyr::mutate(year = as.integer(stringr::str_extract(term, "[0-9]{4}"))) |>
  dplyr::transmute(year, gamma = estimate) |>
  dplyr::bind_rows(tibble::tibble(year = 2007, gamma = 0))

base_emissions <- did_data_total |>
  dplyr::filter(.data$year == 2007) |>
  dplyr::group_by(.data$ets) |>
  dplyr::summarise(base_emissions = mean(.data$ets_emissions, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = "ets", values_from = "base_emissions")

predicted_levels <- dplyr::full_join(beta, gamma, by = "year") |>
  tidyr::complete(year = sort(unique(did_data_total$year)), fill = list(beta = 0, gamma = 0)) |>
  dplyr::arrange(year) |>
  dplyr::mutate(
    beta_base = beta[year == 2007][1],
    gamma_base = gamma[year == 2007][1],
    pred_non_ets = base_emissions$`0` * exp(beta - beta_base),
    pred_ets = base_emissions$`1` * exp(beta - beta_base + gamma - gamma_base)
  ) |>
  dplyr::select(year, pred_non_ets, pred_ets) |>
  tidyr::pivot_longer(
    c(pred_non_ets, pred_ets),
    names_to = "group",
    values_to = "emissions"
  ) |>
  dplyr::mutate(group = dplyr::recode(group, pred_non_ets = "Non-ETS", pred_ets = "ETS")) |>
  dplyr::filter(year >= 2004)
save_table(predicted_levels, "figure_3_predicted_emissions")

predicted_indexed <- predicted_levels |>
  dplyr::group_by(.data$group) |>
  dplyr::mutate(emissions = round(100 * .data$emissions / .data$emissions[.data$year == 2007][1], 2)) |>
  dplyr::ungroup()
save_table(predicted_indexed, "figure_5_predicted_emissions_indexed")
save_table(predicted_indexed, "figure_3_predicted_emissions_indexed")

predicted <- predicted_indexed

figure_5 <- ggplot2::ggplot(predicted, ggplot2::aes(x = year, y = emissions, color = group, linetype = group)) +
  ggplot2::geom_line(linewidth = 1.4) +
  ggplot2::geom_vline(xintercept = 2007.5, linetype = "dashed", color = "red") +
  ggplot2::annotate(
    "text", x = 2007.5, y = 50, label = "Treatment\n(2008)",
    angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
  ) +
  ggplot2::geom_vline(xintercept = 2012.5, linetype = "dashed", color = "red") +
  ggplot2::annotate(
    "text", x = 2012.5, y = 50, label = "Phase 3\n(2013)",
    angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
  ) +
  ggplot2::geom_vline(xintercept = 2020.5, linetype = "dashed", color = "red") +
  ggplot2::annotate(
    "text", x = 2020.5, y = 50, label = "Phase 4\n(2021)",
    angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
  ) +
  ggplot2::geom_hline(yintercept = pretty(predicted$emissions, n = 5), color = "grey90", linewidth = 0.3) +
  ggplot2::scale_x_continuous(limits = c(2004, 2023), breaks = 2004:2023) +
  ggplot2::scale_color_manual(values = c("Non-ETS" = "black", "ETS" = "blue"), name = "") +
  ggplot2::scale_linetype_manual(values = c("Non-ETS" = "solid", "ETS" = "dashed"), name = "") +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(x = "Year", y = "CO2 Emissions, 2007 = 100") +
  ggplot2::theme_classic(base_size = 14) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

ggplot2::ggsave(
  output_path("plots", "Predicted_emissions_2004_indexed.png"),
  figure_5,
  width = 12,
  height = 9,
  dpi = 600
)

ggplot2::ggsave(
  output_path("plots", "Predicted_emissions_indexed.png"),
  figure_5,
  width = 12,
  height = 9,
  dpi = 600
)

figure_3_levels <- ggplot2::ggplot(predicted_levels, ggplot2::aes(x = year, y = emissions, color = group, linetype = group)) +
  ggplot2::geom_line(linewidth = 1.4) +
  ggplot2::geom_vline(xintercept = 2007.5, linetype = "dashed", color = "red") +
  ggplot2::annotate(
    "text", x = 2007.5, y = 35000, label = "Treatment\n(2008)",
    angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
  ) +
  ggplot2::geom_vline(xintercept = 2012.5, linetype = "dashed", color = "red") +
  ggplot2::annotate(
    "text", x = 2012.5, y = 35000, label = "Phase 3\n(2013)",
    angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
  ) +
  ggplot2::geom_vline(xintercept = 2020.5, linetype = "dashed", color = "red") +
  ggplot2::annotate(
    "text", x = 2020.5, y = 35000, label = "Phase 4\n(2021)",
    angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
  ) +
  ggplot2::geom_hline(yintercept = pretty(predicted_levels$emissions, n = 5), color = "grey90", linewidth = 0.3) +
  ggplot2::scale_x_continuous(limits = c(2004, 2023), breaks = 2004:2023) +
  ggplot2::scale_color_manual(values = c("Non-ETS" = "black", "ETS" = "blue"), name = "") +
  ggplot2::scale_linetype_manual(values = c("Non-ETS" = "solid", "ETS" = "dashed"), name = "") +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::labs(x = "Year", y = "CO2 Emissions (Tonnes)") +
  ggplot2::theme_classic(base_size = 14) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

ggplot2::ggsave(
  output_path("plots", "Predicted_emissions_2004.png"),
  figure_3_levels,
  width = 12,
  height = 9,
  dpi = 600
)

ggplot2::ggsave(
  output_path("plots", "Predicted_emissions.png"),
  figure_3_levels,
  width = 12,
  height = 9,
  dpi = 600
)

# Figures 8-11, country-level predicted emissions.
build_predicted_emissions <- function(data, base_year = 2007) {
  country_event_data <- data |>
    dplyr::mutate(
      ets = factor(.data$ets, levels = c(0, 1)),
      year_factor = stats::relevel(factor(.data$year), ref = as.character(base_year))
    )

  model <- fixest::feols(
    log(ets_emissions) ~ year_factor * ets | identifiant,
    data = country_event_data,
    weights = ~weights
  )

  coef_yearly <- broom::tidy(model, conf.int = TRUE)

  beta <- coef_yearly |>
    dplyr::filter(stringr::str_detect(.data$term, "^year_factor[0-9]{4}$")) |>
    dplyr::mutate(year = as.integer(stringr::str_extract(.data$term, "[0-9]{4}"))) |>
    dplyr::select(year, beta = estimate)

  gamma <- coef_yearly |>
    dplyr::filter(stringr::str_detect(.data$term, "^year_factor[0-9]{4}:ets1$")) |>
    dplyr::mutate(year = as.integer(stringr::str_extract(.data$term, "[0-9]{4}"))) |>
    dplyr::transmute(year = year, gamma = estimate)

  base_emissions <- country_event_data |>
    dplyr::filter(.data$year == base_year) |>
    dplyr::group_by(.data$ets) |>
    dplyr::summarise(base_emissions = mean(.data$ets_emissions, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "ets", values_from = "base_emissions")

  dplyr::full_join(beta, gamma, by = "year") |>
    tidyr::complete(
      year = 2004:2023,
      fill = list(beta = 0, gamma = 0)
    ) |>
    dplyr::arrange(.data$year) |>
    dplyr::mutate(
      beta_base = .data$beta[.data$year == base_year][1],
      gamma_base = .data$gamma[.data$year == base_year][1],
      pred_non_ets = base_emissions$`0` * exp(.data$beta - .data$beta_base),
      pred_ets = base_emissions$`1` * exp(.data$beta - .data$beta_base + .data$gamma - .data$gamma_base)
    ) |>
    dplyr::select(year, pred_non_ets, pred_ets) |>
    tidyr::pivot_longer(
      c(pred_non_ets, pred_ets),
      names_to = "group",
      values_to = "emissions"
    ) |>
    dplyr::mutate(group = dplyr::recode(.data$group, pred_non_ets = "Non-ETS", pred_ets = "ETS")) |>
    dplyr::filter(.data$year >= 2004)
}

plot_predicted_emissions <- function(predicted_data, annotation_y = NULL, y_label = "CO2 Emissions (tonnes)") {
  y_max <- max(predicted_data$emissions, na.rm = TRUE)
  if (is.null(annotation_y)) {
    annotation_y <- 0.6 * y_max
  }
  y_grid <- pretty(predicted_data$emissions, n = 5)
  y_grid <- y_grid[y_grid >= 0 & y_grid <= y_max]

  ggplot2::ggplot(predicted_data, ggplot2::aes(x = .data$year, y = .data$emissions, color = .data$group, linetype = .data$group)) +
    ggplot2::geom_line(linewidth = 1.4) +
    ggplot2::coord_cartesian(ylim = c(0, y_max)) +
    ggplot2::geom_vline(xintercept = 2007.5, linetype = "dashed", color = "red") +
    ggplot2::annotate(
      "text", x = 2007.5, y = annotation_y, label = "Treatment\n(2008)",
      angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
    ) +
    ggplot2::geom_vline(xintercept = 2012.5, linetype = "dashed", color = "red") +
    ggplot2::annotate(
      "text", x = 2012.5, y = annotation_y, label = "Phase 3\n(2013)",
      angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
    ) +
    ggplot2::geom_vline(xintercept = 2020.5, linetype = "dashed", color = "red") +
    ggplot2::annotate(
      "text", x = 2020.5, y = annotation_y, label = "Phase 4\n(2021)",
      angle = 90, vjust = -0.2, hjust = 0.5, color = "red", size = 4
    ) +
    ggplot2::geom_hline(yintercept = y_grid, color = "grey90", linewidth = 0.3) +
    ggplot2::scale_x_continuous(limits = c(2004, 2023), breaks = 2004:2023) +
    ggplot2::scale_color_manual(values = c("Non-ETS" = "black", "ETS" = "blue"), name = "") +
    ggplot2::scale_linetype_manual(values = c("Non-ETS" = "solid", "ETS" = "dashed"), name = "") +
    ggplot2::scale_y_continuous(limits = c(0, y_max), labels = scales::comma) +
    ggplot2::labs(x = "Year", y = y_label) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

country_plot_files <- c(FR = "Predicted_FR", NL = "Predicted_NL", GB = "Predicted_GB", NO = "Predicted_NO")
country_annotation_y <- c(FR = 35000, NL = 30000, GB = 9000, NO = 35000)
country_predicted <- purrr::imap_dfr(country_plot_files, function(file_stub, country_code) {
  country_data <- dplyr::filter(did_data_total, .data$country == .env$country_code)
  build_predicted_emissions(country_data) |>
    dplyr::mutate(country = .env$country_names[[country_code]], country_code = .env$country_code, .before = 1)
})
save_table(country_predicted, "figures_8_to_11_country_predicted_emissions")

purrr::iwalk(country_plot_files, function(file_stub, country_code) {
  plot_data <- dplyr::filter(country_predicted, .data$country_code == .env$country_code)
  ggplot2::ggsave(
    output_path("plots", paste0(file_stub, ".png")),
    plot_predicted_emissions(plot_data, annotation_y = country_annotation_y[[country_code]]),
    width = 12,
    height = 9,
    dpi = 600
  )
})

# Appendix boxplot.
box_stats <- did_data_total |>
  dplyr::filter(!is.na(ets_emissions)) |>
  dplyr::mutate(group = dplyr::recode(as.character(ets), `0` = "Non-ETS installations", `1` = "ETS installations")) |>
  dplyr::group_by(group) |>
  dplyr::summarise(
    q1 = stats::quantile(ets_emissions, 0.25),
    q3 = stats::quantile(ets_emissions, 0.75),
    iqr = q3 - q1,
    lower_whisker = q1 - 1.5 * iqr,
    upper_whisker = q3 + 1.5 * iqr,
    .groups = "drop"
  )

box_data <- did_data_total |>
  dplyr::filter(!is.na(ets_emissions)) |>
  dplyr::mutate(group = dplyr::recode(as.character(ets), `0` = "Non-ETS installations", `1` = "ETS installations")) |>
  dplyr::left_join(box_stats, by = "group") |>
  dplyr::filter(ets_emissions >= lower_whisker, ets_emissions <= upper_whisker)

y_max <- max(box_stats$upper_whisker) * 1.05

boxplot_figure <- ggplot2::ggplot(box_data, ggplot2::aes(x = group, y = ets_emissions)) +
  ggplot2::geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(add = c(0, 0))) +
  ggplot2::coord_cartesian(ylim = c(0, y_max)) +
  ggplot2::labs(x = NULL, y = "CO2 emissions in tonnes") +
  ggplot2::annotate("text", x = 1.5, y = y_max * 0.02, label = "excludes outside values", size = 3, color = "grey30") +
  ggplot2::theme_bw(base_size = 14)

ggplot2::ggsave(
  output_path("plots", "boxplot.png"),
  boxplot_figure,
  width = 10,
  height = 7,
  dpi = 600
)

message("Wrote reproduced tables and plots to: ", output_path())
