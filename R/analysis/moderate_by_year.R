#' Whether associations differ by collection year, cell by cell
#'
#' Applies `regress_units_on_year()` to the stage 1 slopes of each exposure
#' and outcome cell. It reuses the stage 1 estimates, so it adds no stage 1
#' fitting. The test is exploratory: the preregistration lists year of data
#' collection as a moderator to collect but registers no hypothesis about it.
#'
#' Year stands in for content, which is not measured. A slope can also
#' differ because hours shifted along a curved relationship, because an item
#' changed meaning, or because different countries and ages were studied in
#' different periods, so a result reads "the link was stronger in more recent
#' data" and never that content has got worse.
#'
#' @param stage1 Stage 1 estimates, as from `fit_stage1()`.
#' @param units Per-unit descriptions, as the `units` table of
#'   `describe_dataset()`: year, country, median age, response format and
#'   who reported.
#' @param exposure_registry The exposure registry.
#' @param breaks The table from `read_series_breaks()`.
#' @param settings From `analysis_settings()`.
#' @return One row per exposure, outcome and effect measure.
moderate_by_year <- function(
  stage1,
  units,
  exposure_registry,
  breaks,
  settings = analysis_settings()
) {
  fitted <- stage1[stage1$status %in% "fitted", ]
  if (nrow(fitted) == 0) {
    return(tibble::tibble())
  }
  fitted <- dplyr::left_join(
    fitted[c(
      "dataset_id",
      "unit",
      "exposure",
      "outcome",
      "effect_measure",
      "estimate",
      "se"
    )],
    units[c(
      "unit",
      "country",
      "year",
      "age_median",
      "measure_type",
      "responder"
    )],
    by = "unit"
  )
  exposure_of <- exposure_registry$exposure[match(
    fitted$exposure,
    exposure_registry$variable_name
  )]
  fitted$series <- NA_character_
  for (exposure in unique(exposure_of)) {
    rows <- exposure_of %in% exposure
    fitted$series[rows] <- assign_series(
      fitted[rows, ],
      exposure,
      breaks,
      settings
    )
  }
  fitted$yi <- fitted$estimate
  fitted$sei <- fitted$se
  key <- paste(
    fitted$exposure,
    fitted$outcome,
    fitted$effect_measure,
    sep = "\r"
  )
  out <- lapply(split(fitted, key), function(cell) {
    row <- regress_units_on_year(cell, settings)
    row$exposure <- cell$exposure[1]
    row$outcome <- cell$outcome[1]
    row$effect_measure <- cell$effect_measure[1]
    row
  })
  dplyr::relocate(
    dplyr::bind_rows(out),
    "exposure",
    "outcome",
    "effect_measure"
  )
}
