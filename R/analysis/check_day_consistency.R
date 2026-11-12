#' Check usual-day screen time against its weekday and weekend rows
#'
#' The harmonisation guide fills the unsuffixed (usual-day) row as
#' (5 x weekday + 2 x weekend) / 7 when a wave maps both. So wherever a row
#' has all three, the usual-day value should equal that. The comparison is
#' made row by row: the three rows often cover different participants, and
#' comparing their means would mislead.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param exposures The exposure registry.
#' @param tolerance Largest difference, in hours, treated as rounding.
#' @return A tibble with one row per wave, measure and exposure that has all
#'   three rows on some participants: `n` such rows, `n_differs`, and the
#'   mean usual-day and expected values. NULL when the dataset is not
#'   included or has no such exposure.
check_day_consistency <- function(
  harmonised,
  info,
  exposures,
  tolerance = 0.02
) {
  rows <- included_rows(harmonised, info)
  if (is.null(rows)) {
    return(NULL)
  }
  days <- exposures[
    exposures$unit %in% "hours" &
      exposures$day %in% c("usual", "weekday", "weekend") &
      exposures$variable_name %in% names(rows),
    c("variable_name", "exposure", "context", "day")
  ]
  sets <- tidyr::pivot_wider(
    days,
    id_cols = c("exposure", "context"),
    names_from = "day",
    values_from = "variable_name"
  )
  needed <- c("usual", "weekday", "weekend")
  if (!all(needed %in% names(sets))) {
    return(NULL)
  }
  sets <- sets[stats::complete.cases(sets[needed]), ]
  wave <- as.character(rows$wave)
  measure <- as.character(rows$st_measure_id)

  check_one <- function(i) {
    usual <- as.numeric(rows[[sets$usual[i]]])
    weekday <- as.numeric(rows[[sets$weekday[i]]])
    weekend <- as.numeric(rows[[sets$weekend[i]]])
    all_three <- !is.na(usual) & !is.na(weekday) & !is.na(weekend)
    if (!any(all_three)) {
      return(NULL)
    }
    expected <- (5 * weekday + 2 * weekend) / 7
    out <- dplyr::summarise(
      tibble::tibble(
        wave = wave[all_three],
        st_measure_id = measure[all_three],
        usual = usual[all_three],
        expected = expected[all_three]
      ),
      n = dplyr::n(),
      n_differs = sum(abs(.data$usual - .data$expected) > tolerance),
      mean_usual = mean(.data$usual),
      mean_expected = mean(.data$expected),
      .by = c("wave", "st_measure_id")
    )
    out$variable_name <- sets$usual[i]
    out
  }

  out <- dplyr::bind_rows(lapply(seq_len(nrow(sets)), check_one))
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$dataset_id <- as.character(info$dataset_id)
  dplyr::relocate(out, "dataset_id", "variable_name")
}
