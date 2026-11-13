#' Pool any per-unit table so that each dataset counts once
#'
#' The rule of the descriptives, in one place: a dataset's units are averaged
#' into one value per dataset, weighted by their rows, and the pooled figure
#' is the median of the datasets' values with the middle half of datasets. A
#' figure needs the minimum number of datasets.
#'
#' @param table A per-unit table with `dataset_id` and `n`.
#' @param values Names of the columns to pool.
#' @param by Names of the columns that define a row of the pooled table.
#' @param settings From `analysis_settings()`.
#' @return A tibble with `by`, `datasets`, `rows`, and for each value its
#'   `<value>_mid`, `<value>_low` and `<value>_high`.
pool_unit_table <- function(table, values, by, settings = analysis_settings()) {
  per_dataset <- dplyr::summarise(
    table,
    dplyr::across(
      dplyr::all_of(values),
      function(x) stats::weighted.mean(x, .data$n)
    ),
    rows = sum(.data$n),
    .by = dplyr::all_of(c("dataset_id", by))
  )
  quartile <- function(x, p) stats::quantile(x, p, names = FALSE)
  pooled <- dplyr::summarise(
    per_dataset,
    datasets = dplyr::n(),
    rows = sum(.data$rows),
    dplyr::across(
      dplyr::all_of(values),
      list(
        mid = stats::median,
        low = function(x) quartile(x, 0.25),
        high = function(x) quartile(x, 0.75)
      )
    ),
    .by = dplyr::all_of(by)
  )
  pooled[pooled$datasets >= settings$min_datasets, ]
}
