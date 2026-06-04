source(file.path("scripts", "_setup.R"))

load_packages(c("dplyr", "readr", "stringdist", "tidyr"))

read_match_input <- function(path, sep = ",") {
  raw_lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  cleaned_lines <- iconv(raw_lines, from = "UTF-8", to = "UTF-8", sub = "")
  temp_file <- tempfile(fileext = ".csv")
  writeLines(cleaned_lines, temp_file, useBytes = TRUE)
  read.csv(
    temp_file,
    sep = sep,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    row.names = NULL
  )
}

ets_facilities <- read_match_input(
  repo_path("PRTR Norway", "ETS_facilities_to_match.csv")
) |>
  dplyr::select(ETS_Name = 1) |>
  dplyr::mutate(ETS_Name = trimws(.data$ETS_Name)) |>
  dplyr::filter(!is.na(.data$ETS_Name), .data$ETS_Name != "")

all_facilities <- read_match_input(
  repo_path("PRTR Norway", "all_facilities_to_match.csv"),
  sep = ";"
) |>
  dplyr::select(PRTR_Name = 1, EUTL_Name = 2) |>
  dplyr::mutate(PRTR_Name = trimws(.data$PRTR_Name))

algorithmic_matches <- ets_facilities |>
  dplyr::rowwise() |>
  dplyr::mutate(
    Match = list(
      all_facilities |>
        dplyr::mutate(
          Similarity = stringdist::stringdist(.data$PRTR_Name, ETS_Name, method = "jw")
        ) |>
        dplyr::filter(.data$Similarity <= 0.2) |>
        dplyr::arrange(.data$Similarity) |>
        dplyr::slice(1)
    )
  ) |>
  tidyr::unnest_wider("Match") |>
  dplyr::ungroup() |>
  dplyr::mutate(Similarity = round(.data$Similarity, 9))

final_matches_raw <- read_match_input(
  repo_path("PRTR Norway", "Matched_Facilities.csv"),
  sep = ";"
)
names(final_matches_raw)[1:5] <- c("ETS_Name", "PRTR_Name", "EUTL_Name", "Similarity", "X")

final_matches <- final_matches_raw |>
  dplyr::mutate(
    Similarity = round(.data$Similarity, 9),
    X = dplyr::na_if(.data$X, "")
  ) |>
  dplyr::select(ETS_Name, PRTR_Name, EUTL_Name, Similarity, X)

comparison <- dplyr::full_join(
  algorithmic_matches |>
    dplyr::mutate(source_algorithm = TRUE),
  final_matches |>
    dplyr::mutate(source_final = TRUE),
  by = c("ETS_Name", "PRTR_Name", "EUTL_Name", "Similarity")
) |>
  dplyr::mutate(
    source_algorithm = dplyr::coalesce(.data$source_algorithm, FALSE),
    source_final = dplyr::coalesce(.data$source_final, FALSE),
    manual_rejected = .data$X == "x"
  )

summary_table <- tibble::tibble(
  metric = c(
    "ets_facilities_to_match",
    "all_prtr_facilities_to_match",
    "algorithmic_candidate_rows",
    "final_candidate_rows",
    "manual_rejections_marked_x",
    "algorithmic_rows_missing_from_final",
    "final_rows_not_recreated_algorithmically"
  ),
  value = c(
    nrow(ets_facilities),
    nrow(all_facilities),
    nrow(algorithmic_matches),
    nrow(final_matches),
    sum(final_matches$X == "x", na.rm = TRUE),
    sum(comparison$source_algorithm & !comparison$source_final),
    sum(!comparison$source_algorithm & comparison$source_final)
  )
)

readr::write_csv(
  algorithmic_matches,
  output_path("data", "norway_algorithmic_matched_facilities.csv")
)
readr::write_csv(
  summary_table,
  output_path("tables", "norway_prtr_eutl_matching_audit_summary.csv")
)
readr::write_csv(
  final_matches |> dplyr::filter(.data$X == "x"),
  output_path("tables", "norway_manual_rejections.csv")
)
readr::write_csv(
  comparison |> dplyr::filter(.data$source_algorithm & !.data$source_final),
  output_path("tables", "norway_algorithmic_rows_missing_from_final.csv")
)
readr::write_csv(
  comparison |> dplyr::filter(!.data$source_algorithm & .data$source_final),
  output_path("tables", "norway_final_rows_not_recreated_algorithmically.csv")
)

print(summary_table)

if (any(summary_table$value[summary_table$metric %in% c(
  "algorithmic_rows_missing_from_final",
  "final_rows_not_recreated_algorithmically"
)] != 0)) {
  message("The finalized Norwegian match file differs from the simple algorithmic candidates; this records the manual review layer.")
}
