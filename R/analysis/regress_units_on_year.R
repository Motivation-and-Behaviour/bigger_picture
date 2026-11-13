#' Meta-regression of unit-level estimates on collection year
#'
#' Asks whether an estimate (a stage 1 slope, or a descriptive statistic)
#' differs with the year the data were collected, with year as a proxy for
#' what children's screen use consists of (ANALYSIS_PLAN.md, Step 12).
#'
#' Year is split in two. The between-dataset part is a dataset's mean year:
#' it compares studies run in different periods, which also differ in
#' country, age and instrument, so it is adjusted for the units' median age
#' and, where they vary, the response format and who reported. The
#' within-series part is a unit's year minus the mean year of its series: a
#' series is a set of a dataset's units that measure the same thing in the
#' same country, school stage, respondent and response format, with no
#' recorded change to the item between them (`assign_series()`). The within
#' part compares like with like and is the firmer evidence. Median age is in
#' the model throughout, so ageing within a school stage is adjusted for.
#'
#' The model is the stage 2 model with these moderators: REML, units nested
#' in datasets, robust inference clustered on dataset. Slopes are per decade.
#' It runs only with enough datasets over enough years.
#'
#' @param units A tibble with one row per unit: `dataset_id`, `unit`, `yi`
#'   and `sei` (the estimate and its standard error), `year`, `series`,
#'   `age_median`, `measure_type` and `responder`.
#' @param settings From `analysis_settings()`.
#' @return A one-row tibble: counts, the year span, the between and within
#'   slopes per decade with robust intervals and degrees of freedom, the
#'   number of datasets with a series spanning more than one year, which is
#'   what the within slope rests on, a status for each slope (`estimated`, or
#'   `too few studies to say` under four robust degrees of freedom), and
#'   `status`.
regress_units_on_year <- function(units, settings = analysis_settings()) {
  units <- units[
    !is.na(units$yi) & !is.na(units$sei) & units$sei > 0 & !is.na(units$year),
  ]
  k_datasets <- dplyr::n_distinct(units$dataset_id)
  span <- if (nrow(units) == 0) NA_real_ else diff(range(units$year))
  result <- function(status, ...) {
    values <- list(
      k_datasets = k_datasets,
      k_units = nrow(units),
      first_year = if (nrow(units) == 0) NA_real_ else min(units$year),
      last_year = if (nrow(units) == 0) NA_real_ else max(units$year),
      between_per_decade = NA_real_,
      between_ci_low = NA_real_,
      between_ci_high = NA_real_,
      between_df = NA_real_,
      within_per_decade = NA_real_,
      within_ci_low = NA_real_,
      within_ci_high = NA_real_,
      within_df = NA_real_,
      k_within = 0L,
      between_status = NA_character_,
      within_status = NA_character_,
      adjusted_for = NA_character_,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    tibble::as_tibble(values)
  }
  if (
    k_datasets < settings$year_min_datasets ||
      is.na(span) ||
      span < settings$year_min_span
  ) {
    return(result("too few datasets or years"))
  }

  dataset_year <- stats::ave(units$year, units$dataset_id, FUN = mean)
  units$decade_between <- (dataset_year - 2010) / 10
  series_year <- stats::ave(units$year, units$series, FUN = mean)
  units$decade_within <- (units$year - series_year) / 10
  varies_within <- tapply(
    abs(units$decade_within) > 0.01,
    units$dataset_id,
    any
  )
  k_within <- sum(varies_within)

  terms <- "decade_between"
  adjusted <- character()
  if (k_within > 0) {
    terms <- c(terms, "decade_within")
  }
  if (!anyNA(units$age_median) && stats::sd(units$age_median) > 0) {
    units$age_c <- units$age_median - mean(units$age_median)
    terms <- c(terms, "age_c")
    adjusted <- c(adjusted, "median age")
  }
  varying <- function(values) {
    counts <- table(values)
    !anyNA(values) && length(counts) > 1 && min(counts) >= 3
  }
  if (varying(units$measure_type)) {
    terms <- c(terms, "measure_type")
    adjusted <- c(adjusted, "response format")
  }
  units$self_report <- ifelse(units$responder %in% "Self", "self", "other")
  if (!anyNA(units$responder) && varying(units$self_report)) {
    terms <- c(terms, "self_report")
    adjusted <- c(adjusted, "who reported")
  }

  units$vi <- units$sei^2
  nested <- nrow(units) > k_datasets
  random <- if (nested) {
    stats::as.formula("~ 1 | dataset_id / unit")
  } else {
    stats::as.formula("~ 1 | dataset_id")
  }
  fit <- tryCatch(
    metafor::rma.mv(
      yi,
      vi,
      mods = stats::as.formula(paste("~", paste(terms, collapse = " + "))),
      random = random,
      data = units,
      method = "REML",
      test = "t"
    ),
    error = function(e) conditionMessage(e)
  )
  if (is.character(fit)) {
    return(result(paste("model failed:", fit)))
  }
  robust <- tryCatch(
    as.data.frame(
      clubSandwich::conf_int(fit, vcov = "CR2", cluster = units$dataset_id)
    ),
    error = function(e) conditionMessage(e)
  )
  if (is.character(robust)) {
    return(result(paste("robust inference failed:", robust)))
  }
  rownames(robust) <- rownames(fit$b)
  pick <- function(term, column) {
    if (term %in% rownames(robust)) robust[term, column] else NA_real_
  }
  result(
    "fitted",
    between_per_decade = pick("decade_between", "beta"),
    between_ci_low = pick("decade_between", "CI_L"),
    between_ci_high = pick("decade_between", "CI_U"),
    between_df = pick("decade_between", "df"),
    within_per_decade = pick("decade_within", "beta"),
    within_ci_low = pick("decade_within", "CI_L"),
    within_ci_high = pick("decade_within", "CI_U"),
    within_df = pick("decade_within", "df"),
    k_within = as.integer(k_within),
    between_status = few_to_say(pick("decade_between", "df")),
    within_status = few_to_say(pick("decade_within", "df")),
    adjusted_for = paste(adjusted, collapse = ", ")
  )
}

# The stage 2 rule for a robust estimate: under four degrees of freedom there
# are too few studies to say.
few_to_say <- function(df) {
  if (is.na(df)) {
    return(NA_character_)
  }
  if (df < 4) "too few studies to say" else "estimated"
}
