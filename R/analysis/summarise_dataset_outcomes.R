#' The outcome of every dataset sought, with the pipeline's own count
#'
#' Joins the hand-entered counts with what the pipeline holds, for the top of
#' the flow diagram. A blank count stays blank, and the total is given only
#' when every outcome has a count. `included` should equal the number of
#' datasets in the analysis: `agrees` says whether it does.
#'
#' @param outcomes The table from `read_dataset_outcomes()`.
#' @param manifest The data manifest.
#' @return A list: `table` (one row per outcome, with `in_analysis` on the
#'   included row), `sought` (the total, or NA until every count is entered),
#'   `complete` and `agrees` (NA until `included` is entered).
summarise_dataset_outcomes <- function(outcomes, manifest) {
  in_analysis <- nrow(manifest)
  outcomes$in_analysis <- ifelse(
    outcomes$outcome == "included",
    in_analysis,
    NA_real_
  )
  complete <- !anyNA(outcomes$datasets)
  entered <- outcomes$datasets[outcomes$outcome == "included"]
  list(
    table = outcomes,
    sought = if (complete) sum(outcomes$datasets) else NA_real_,
    complete = complete,
    agrees = if (is.na(entered)) NA else entered == in_analysis
  )
}
