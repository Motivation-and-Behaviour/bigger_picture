#' Pool per-unit descriptive statistics so that each dataset counts once
#'
#' A dataset's units are first averaged, weighted by their rows, into one
#' value per dataset. The pooled figure is then the median of the datasets'
#' values, with the range the middle half of datasets span, so the largest
#' surveys do not swamp the picture (ANALYSIS_PLAN.md, Step 6). A result
#' needs the minimum number of datasets; below that no figure is given.
#'
#' Total screen time is never pooled across the ways it was built: a
#' dedicated item, and sums of two, three or four content classes, are
#' separate rows.
#'
#' @param screen Combined `screen` tables from `describe_dataset()`.
#' @param units Combined `units` tables from `describe_dataset()`.
#' @param settings From `analysis_settings()`.
#' @return A tibble with one row per variable, way of measuring it, grouping
#'   and group: the number of datasets and rows, and for each statistic the
#'   median across datasets and the 25th and 75th percentiles.
pool_descriptives <- function(screen, units, settings = analysis_settings()) {
  statistics <- c(
    "median",
    "p25",
    "p75",
    "p90",
    "share_zero",
    "share_2plus",
    "share_4plus"
  )
  screen <- dplyr::left_join(
    screen,
    units[c("unit", "total_source", "total_coverage")],
    by = "unit"
  )
  screen$built <- ifelse(
    screen$variable_name == "total_screen",
    ifelse(
      screen$total_source %in% "dedicated",
      "dedicated item",
      paste("sum of", screen$total_coverage, "content classes")
    ),
    "as asked"
  )
  keys <- c("variable_name", "built", "grouping", "group")
  per_dataset <- dplyr::summarise(
    screen,
    dplyr::across(
      dplyr::all_of(statistics),
      function(x) stats::weighted.mean(x, .data$n)
    ),
    rows = sum(.data$n),
    .by = dplyr::all_of(c("dataset_id", keys))
  )
  spread <- function(x, p) stats::quantile(x, p, names = FALSE)
  pooled <- dplyr::summarise(
    per_dataset,
    datasets = dplyr::n(),
    rows = sum(.data$rows),
    dplyr::across(
      dplyr::all_of(statistics),
      list(
        mid = stats::median,
        low = function(x) spread(x, 0.25),
        high = function(x) spread(x, 0.75)
      )
    ),
    .by = dplyr::all_of(keys)
  )
  pooled[pooled$datasets >= settings$min_datasets, ]
}
