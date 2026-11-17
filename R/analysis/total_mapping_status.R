#' The harmonisation status of a dataset's total screen-time item
#'
#' A `partial` mapping of `st_total` says the item covers only part of
#' screen use, which the preparation step needs to know before it treats
#' the item as a total (DR-0013).
#'
#' @param variables The dataset's harmonisation mapping (`variables.csv`).
#' @return The status of the primary-measure `st_total` row, or NA where
#'   there is none.
total_mapping_status <- function(variables) {
  primary <- if ("measure" %in% names(variables)) {
    is.na(variables$measure) | variables$measure %in% c("", "primary")
  } else {
    rep(TRUE, nrow(variables))
  }
  status <- variables$status[
    variables$target_variable %in% "st_total" & primary
  ]
  if (length(status) == 0) NA_character_ else as.character(status[1])
}
