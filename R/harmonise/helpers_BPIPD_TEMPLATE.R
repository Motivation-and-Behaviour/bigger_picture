#' Dataset-specific helper template for table-driven harmonisation
#'
#' Prefer expressing mappings directly in `variables.csv`.
#' Use helper functions only for logic that is reused or too noisy to write
#' inline. Any helper referenced in a mapping should still have a readable
#' `algorithm_summary` in that CSV row.
bp_helper_BPIPD_TEMPLATE <- function(x) {
  x
}
