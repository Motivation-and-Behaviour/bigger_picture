#' Group a dataset's units into series that measure the same thing
#'
#' A trend over years is only meaningful along a series of like units. Two
#' units of a dataset belong to one series when they are in the same country,
#' were reported by the same kind of respondent in the same response format,
#' cover the same school stage, and fall on the same side of every recorded
#' change to the item (`series_breaks.csv`). A panel followed from age 5 to
#' 15 therefore breaks into a series per school stage, and a survey of
#' fifteen year olds repeated for twenty years is one series.
#'
#' @param units A tibble with `dataset_id`, `country`, `responder`,
#'   `measure_type`, `age_median` and `year`.
#' @param exposure The exposure the units' values are of, for the breaks.
#' @param breaks The table from `read_series_breaks()`.
#' @param settings From `analysis_settings()`.
#' @return A character vector, one series label per unit.
assign_series <- function(
  units,
  exposure,
  breaks,
  settings = analysis_settings()
) {
  stage <- as.character(cut(
    units$age_median,
    breaks = settings$school_stage_breaks,
    labels = settings$school_stage_labels,
    right = FALSE
  ))
  segment <- rep(0L, nrow(units))
  applies <- breaks[breaks$exposure %in% exposure, ]
  for (i in seq_len(nrow(applies))) {
    after <- units$dataset_id %in%
      applies$dataset_id[i] &
      !is.na(units$year) &
      units$year >= applies$from_year[i]
    segment[after] <- segment[after] + 1L
  }
  paste(
    units$dataset_id,
    dplyr::coalesce(as.character(units$country), ""),
    dplyr::coalesce(as.character(units$responder), ""),
    dplyr::coalesce(as.character(units$measure_type), ""),
    dplyr::coalesce(stage, ""),
    segment,
    sep = "|"
  )
}
