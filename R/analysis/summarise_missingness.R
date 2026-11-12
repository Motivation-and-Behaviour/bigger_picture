#' Sporadic and systematic missingness per dataset and variable
#'
#' Missingness is classified by where it sits, not by how it is encoded
#' (ANALYSIS_PLAN.md, Step 4): a missing or `Unknown` cell is sporadic when its
#' wave has real values on that variable, and systematic when the wave has
#' none, because the harmonisation guide also writes `Unknown` in waves where
#' an item was not asked. Counted on the primary rows, by dataset and wave.
#'
#' @param numeric_summary Combined output of `summarise_numeric()`.
#' @param level_summary Combined output of `summarise_levels()`.
#' @param grain Combined output of `summarise_grain()`.
#' @return A tibble with one row per dataset and variable that has any value:
#'   `rows`, `n_value`, `n_sporadic`, `n_systematic` and
#'   `waves_with_value` out of `waves`.
summarise_missingness <- function(numeric_summary, level_summary, grain) {
  primary <- function(tbl) tbl[tbl$st_measure_id %in% "primary", ]
  wave_rows <- primary(grain)[c("dataset_id", "wave", "rows")]

  levels <- primary(level_summary)
  levels <- levels[!levels$level %in% "Unknown", ]
  values <- dplyr::bind_rows(
    primary(numeric_summary)[c("dataset_id", "variable_name", "wave", "n")],
    dplyr::summarise(
      levels,
      n = sum(.data$n),
      .by = c("dataset_id", "variable_name", "wave")
    )
  )

  variables <- dplyr::distinct(values, .data$dataset_id, .data$variable_name)
  grid <- dplyr::inner_join(
    variables,
    wave_rows,
    by = "dataset_id",
    relationship = "many-to-many"
  )
  grid <- dplyr::left_join(
    grid,
    values,
    by = c("dataset_id", "variable_name", "wave"),
    na_matches = "na"
  )
  grid$n[is.na(grid$n)] <- 0L
  dplyr::summarise(
    grid,
    # `rows` is summed last: a summary named like its input replaces it for
    # the expressions that follow.
    n_value = sum(.data$n),
    n_sporadic = sum((.data$rows - .data$n)[.data$n > 0]),
    n_systematic = sum(.data$rows[.data$n == 0]),
    waves_with_value = sum(.data$n > 0),
    waves = dplyr::n(),
    rows = sum(.data$rows),
    .by = c("dataset_id", "variable_name")
  )
}
