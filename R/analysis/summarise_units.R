#' Stage 1 units of one dataset: dataset by wave by country
#'
#' A multi-country survey or a multi-wave cohort is several units. Each row
#' says how big the unit is, whom it covers and which survey design variables
#' it carries, on the primary rows.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param settings From `analysis_settings()`.
#' @return A tibble with one row per wave and country, or NULL. A country of
#'   `Unknown` is no country.
summarise_units <- function(harmonised, info, settings = analysis_settings()) {
  rows <- included_rows(harmonised, info, primary_only = TRUE)
  if (is.null(rows)) {
    return(NULL)
  }
  column <- function(name) {
    if (name %in% names(rows)) rows[[name]] else rep(NA, nrow(rows))
  }
  age <- as.numeric(column("age_years"))
  dt <- data.table::data.table(
    wave = as.character(rows$wave),
    country = unit_country(rows),
    participant_id = as.character(rows$participant_id),
    age = age,
    year = as.numeric(column("data_year")),
    weight = as.numeric(column("survey_weight")),
    strata = as.character(column("survey_strata")),
    cluster = as.character(column("survey_cluster")),
    family = as.character(column("family_id")),
    arm = as.character(column("rct_arm"))
  )
  limit <- settings$age_limit
  units <- dt[,
    list(
      rows = .N,
      participants = data.table::uniqueN(participant_id),
      age_missing = sum(is.na(age)),
      age_eligible = sum(age < limit, na.rm = TRUE),
      age_min = bp_stat(age, min),
      age_median = bp_stat(age, stats::median),
      age_max = bp_stat(age, max),
      year_min = bp_stat(year, min),
      year_median = bp_stat(year, stats::median),
      year_max = bp_stat(year, max),
      weight_rows = sum(!is.na(weight)),
      weight_zero = sum(weight == 0, na.rm = TRUE),
      weight_mean = bp_stat(weight, mean),
      strata_rows = sum(!is.na(strata)),
      strata_n = data.table::uniqueN(strata, na.rm = TRUE),
      cluster_rows = sum(!is.na(cluster)),
      cluster_n = data.table::uniqueN(cluster, na.rm = TRUE),
      family_rows = sum(!is.na(family)),
      trial_rows = sum(is_trial_arm(arm))
    ),
    by = c("wave", "country")
  ]
  out <- tibble::as_tibble(units)
  out$dataset_id <- as.character(info$dataset_id)
  dplyr::relocate(out, "dataset_id")
}

# Rows allocated to a trial arm. Observational rows have no arm; the mock
# dataset labels them instead.
is_trial_arm <- function(arm) {
  arm <- as.character(arm)
  !is.na(arm) & !arm %in% c("Not a randomised trial", "Unknown")
}

# A summary of the non-missing values, NA when there are none.
bp_stat <- function(x, fn) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else as.numeric(fn(x))
}
