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
    primary <- harmonised$st_measure_id %in% "primary"
    # Most datasets have only primary rows: taking them all would copy the
    # whole table for nothing.
    if (!all(primary)) {
      harmonised <- harmonised[primary, ]
    }
  }
  harmonised
}

#' Drop the columns a dataset never fills
#'
#' Every harmonised table carries all of the dataschema's columns, and most
#' are empty for any one dataset: the largest dataset takes about 10 GB with
#' them. Dropping them first means the copies made later (taking the primary
#' rows, the eligible rows) are of what the dataset actually holds. The key
#' columns are kept even when empty, as `wave` is in a single-wave dataset.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @return The tibble without its all-missing columns.
drop_empty_columns <- function(harmonised) {
  if (is.null(harmonised)) {
    return(NULL)
  }
  keys <- c(
    "dataset_id",
    "dataset_name",
    "participant_id",
    "wave",
    "st_measure_id",
    "age_years"
  )
  filled <- vapply(harmonised, function(x) !all(is.na(x)), logical(1))
  harmonised[filled | names(harmonised) %in% keys]
}
