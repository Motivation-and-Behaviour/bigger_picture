#' Coverage of every dataschema variable in every included dataset
#'
#' A value counts when it is neither missing nor `Unknown`, which the
#' harmonisation guide uses for "asked but not known" and for waves where an
#' item was not asked. Screen-time variables are counted on the primary
#' measure, so the denominator is one row per participant and wave.
#'
#' @param numeric_summary Combined output of `summarise_numeric()`.
#' @param level_summary Combined output of `summarise_levels()`.
#' @param grain Combined output of `summarise_grain()`.
#' @param dataschema The dataschema.
#' @return A tibble with one row per dataset and variable that has any value:
#'   `rows` (primary rows in the dataset), `n_value`, `n_unknown`, `share`,
#'   `waves` (waves with a value) and the variable's `domain`.
summarise_coverage <- function(
  numeric_summary,
  level_summary,
  grain,
  dataschema
) {
  primary <- function(tbl) tbl[tbl$st_measure_id %in% "primary", ]
  denominators <- dplyr::summarise(
    primary(grain),
    rows = sum(.data$rows),
    .by = "dataset_id"
  )
  from_numeric <- dplyr::summarise(
    primary(numeric_summary),
    n_value = sum(.data$n),
    n_unknown = 0L,
    waves = dplyr::n_distinct(.data$wave),
    .by = c("dataset_id", "variable_name")
  )
  levels <- primary(level_summary)
  known <- !levels$level %in% "Unknown"
  from_levels <- dplyr::summarise(
    levels,
    n_value = sum(.data$n[known[dplyr::cur_group_rows()]]),
    n_unknown = sum(.data$n[!known[dplyr::cur_group_rows()]]),
    waves = dplyr::n_distinct(.data$wave[known[dplyr::cur_group_rows()]]),
    .by = c("dataset_id", "variable_name")
  )
  coverage <- dplyr::bind_rows(from_numeric, from_levels)
  coverage <- dplyr::left_join(coverage, denominators, by = "dataset_id")
  coverage$share <- coverage$n_value / coverage$rows
  coverage <- dplyr::left_join(
    coverage,
    dataschema[c("variable_name", "domain", "data_type")],
    by = "variable_name"
  )
  coverage[order(as.numeric(coverage$dataset_id), coverage$variable_name), ]
}
