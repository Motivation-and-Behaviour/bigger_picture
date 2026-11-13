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
#' The derived total (`exposure` missing) is built from whichever items a
#' dataset has, so every recorded change in the dataset splits it.
#'
#' @param units A tibble with `dataset_id`, `wave`, `country`, `responder`,
#'   `measure_type`, `age_median` and `year`.
#' @param exposure The exposure the units' values are of, for the breaks; `NA`
#'   for the derived total.
#' @param breaks The table from `read_series_breaks()`.
#' @param settings From `analysis_settings()`.
#' @param day The day each unit's value covers (`weekday`, `weekend` or
#'   `usual`), one value or one per unit, for breaks limited to a day.
#' @return A character vector, one series label per unit.
assign_series <- function(
  units,
  exposure,
  breaks,
  settings = analysis_settings(),
  day = NA_character_
) {
  stage <- as.character(cut(
    units$age_median,
    breaks = settings$school_stage_breaks,
    labels = settings$school_stage_labels,
    right = FALSE
  ))
  stopifnot(
    "wave" %in% names(units),
    length(day) %in% c(1L, nrow(units))
  )
  day <- rep_len(day, nrow(units))
  # One mark per recorded change: units sharing every mark share the item.
  segment <- rep("", nrow(units))
  applies <- breaks[is.na(exposure) | breaks$exposure %in% exposure, ]
  for (i in seq_len(nrow(applies))) {
    own <- units$dataset_id %in% applies$dataset_id[i]
    if (!any(own)) {
      next
    }
    after <- own &
      (is.na(applies$day[i]) | is.na(day) | day %in% applies$day[i]) &
      series_break_applies(applies[i, ], units)
    segment[own] <- paste0(segment[own], as.integer(after[own]))
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
