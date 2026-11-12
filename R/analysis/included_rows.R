#' One dataset's harmonised rows, or NULL when the dataset is not included
#'
#' The analysis reads each dataset's harmonised table directly: it is the same
#' data as that dataset's rows of `analysis_data`, and a target that reads one
#' dataset reruns only when that dataset changes.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param primary_only Keep only the primary screen-time measure's rows, one
#'   per participant and wave.
#' @return A tibble, or NULL.
included_rows <- function(harmonised, info, primary_only = FALSE) {
  if (!isTRUE(info$included) || is.null(harmonised) || nrow(harmonised) == 0) {
    return(NULL)
  }
  if (primary_only && "st_measure_id" %in% names(harmonised)) {
    harmonised <- harmonised[harmonised$st_measure_id %in% "primary", ]
  }
  harmonised
}
