#' Sporadic and systematic missingness per dataset and variable
#'
#' Missingness is classified by where it sits, not by how it is encoded
#' (ANALYSIS_PLAN.md, Step 4): a missing or `Unknown` cell is sporadic when its
#' unit (wave by country) has real values on that variable, and systematic
#' when the unit has none, because the harmonisation guide also writes
#' `Unknown` in waves where an item was not asked. Counted on the primary
#' rows, before the eligibility rules.
#'
#' @param unit_counts Combined output of `summarise_unit_counts()`.
#' @param unit_summary Combined output of `summarise_units()`.
#' @return A tibble with one row per dataset and variable that has any value:
#'   `rows`, `n_value`, `n_sporadic`, `n_systematic` and
#'   `units_with_value` out of `units`.
summarise_missingness <- function(unit_counts, unit_summary) {
  units <- unit_summary[c("dataset_id", "wave", "country", "rows")]
  variables <- dplyr::distinct(
    unit_counts,
    .data$dataset_id,
    .data$variable_name
  )
  grid <- dplyr::inner_join(
    variables,
    units,
    by = "dataset_id",
    relationship = "many-to-many"
  )
  grid <- dplyr::left_join(
    grid,
    unit_counts,
    by = c("dataset_id", "variable_name", "wave", "country"),
    na_matches = "na"
  )
  grid$n_value[is.na(grid$n_value)] <- 0L
  dplyr::summarise(
    grid,
    # `rows` is summed last: a summary named like its input replaces it for
    # the expressions that follow.
    n_sporadic = sum((.data$rows - .data$n_value)[.data$n_value > 0]),
    n_systematic = sum(.data$rows[.data$n_value == 0]),
    units_with_value = sum(.data$n_value > 0),
    units = dplyr::n(),
    n_value = sum(.data$n_value),
    rows = sum(.data$rows),
    .by = c("dataset_id", "variable_name")
  )
}
