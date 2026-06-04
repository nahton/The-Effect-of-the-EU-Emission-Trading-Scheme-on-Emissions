find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    markers <- file.path(current, c("total_data.csv", "General EU-ETS data"))
    if (all(file.exists(markers))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find repository root from: ", start, call. = FALSE)
    }
    current <- parent
  }
}

setup_source_dir <- function() {
  frames <- sys.frames()
  for (frame in rev(frames)) {
    if (!is.null(frame$ofile)) {
      return(dirname(normalizePath(frame$ofile, winslash = "/", mustWork = TRUE)))
    }
  }
  NA_character_
}

script_file_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(script_arg) == 0) {
    return(NA_character_)
  }
  dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = FALSE))
}

repo_root_candidates <- unique(stats::na.omit(c(
  Sys.getenv("ETS_REPO_ROOT", unset = NA_character_),
  getwd(),
  setup_source_dir(),
  script_file_dir()
)))

repo_root <- NULL
for (candidate in repo_root_candidates) {
  repo_root <- tryCatch(find_repo_root(candidate), error = function(error) NULL)
  if (!is.null(repo_root)) {
    break
  }
}

if (is.null(repo_root)) {
  stop(
    "Could not find repository root. Either run from inside the ETS repository, ",
    "source scripts/_setup.R from the repository, or set ETS_REPO_ROOT.",
    call. = FALSE
  )
}

repo_path <- function(...) {
  file.path(repo_root, ...)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

output_path <- function(...) {
  path <- repo_path("outputs", ...)
  ensure_dir(dirname(path))
  path
}

load_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing required R packages: ",
      paste(missing, collapse = ", "),
      ". Install them before rerunning the reproduction scripts.",
      call. = FALSE
    )
  }

  invisible(lapply(packages, library, character.only = TRUE))
}

read_total_data <- function() {
  read.csv(repo_path("total_data.csv")) |>
    dplyr::filter(year >= 2003) |>
    dplyr::mutate(
      country_year = interaction(country, year, sep = "_"),
      sector_year = interaction(sector_group, year, sep = "_")
    )
}

read_all_panel <- function() {
  haven::read_dta(repo_path("General EU-ETS data", "ALL.dta")) |>
    dplyr::select(-dplyr::any_of("ETS2013"))
}

read_all_panel_long <- function() {
  read_all_panel() |>
    tidyr::pivot_longer(
      cols = tidyselect::matches("\\d{4}$"),
      names_to = c(".value", "year"),
      names_pattern = "([a-zA-Z_]+)(\\d{4})",
      names_repair = "unique"
    ) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::select(identifiant, sector, year, nonbio, ETS, nace2dig, nace3dig, zipcode)
}

matching_variant <- function() {
  variant <- Sys.getenv("ETS_MATCHING_VARIANT", unset = "baseline")
  allowed <- c("baseline", "full", "exact_sector")
  if (!variant %in% allowed) {
    stop(
      "Unknown ETS_MATCHING_VARIANT: ", variant,
      ". Use one of: ", paste(allowed, collapse = ", "),
      call. = FALSE
    )
  }
  variant
}

matching_replace <- function() {
  tolower(Sys.getenv("ETS_MATCHING_REPLACE", unset = "false")) %in% c("true", "1", "yes")
}

matching_ratio <- function(default_ratio = 3L) {
  as.integer(Sys.getenv("ETS_MATCHING_RATIO", unset = as.character(default_ratio)))
}

run_matchit_variant <- function(formula, data, default_discard = "both") {
  variant <- matching_variant()

  if (variant == "full") {
    return(MatchIt::matchit(
      formula,
      data = data,
      method = "full",
      distance = "glm",
      link = "logit",
      distance.options = list(),
      estimand = "ATT",
      exact = NULL,
      mahvars = NULL,
      antiexact = NULL,
      discard = default_discard,
      reestimate = FALSE,
      s.weights = NULL,
      replace = matching_replace(),
      m.order = NULL,
      caliper = 0.2,
      ratio = NULL,
      min.controls = NULL,
      max.controls = NULL,
      verbose = FALSE
    ))
  }

  MatchIt::matchit(
    formula,
    data = data,
    method = "nearest",
    distance = "glm",
    link = "logit",
    distance.options = list(),
    estimand = "ATT",
    exact = if (variant == "exact_sector") ~ exact_match_sector else NULL,
    mahvars = NULL,
    antiexact = NULL,
    discard = default_discard,
    reestimate = FALSE,
    s.weights = NULL,
    replace = matching_replace(),
    m.order = NULL,
    caliper = 0.2,
    ratio = matching_ratio(3L),
    min.controls = NULL,
    max.controls = NULL,
    verbose = FALSE
  )
}
