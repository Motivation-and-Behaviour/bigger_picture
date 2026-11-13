#' Screen use by collection year, across all datasets
#'
#' Each unit's mean hours (or a share) against its collection year, in the
#' meta-regression of `regress_units_on_year()`: the between-dataset trend is
#' adjusted for the units' ages and how screen use was asked, and the
#' within-series trend follows like units of one dataset through its years.
#' Comparing periods across datasets mixes real change with which datasets
#' exist in each period, so the between part is read with that caution.
#'
#' Total screen time is not included: it is built differently from dataset to
#' dataset, and its hours are not comparable across them (DR-0013).
#'
#' @param screen Combined `screen` tables from `describe_dataset()`.
#' @param units Combined `units` tables from `describe_dataset()`.
#' @param exposure_registry The exposure registry.
#' @param breaks The table from `read_series_breaks()`.
#' @param statistic `mean` or a share such as `share_2plus`.
#' @param settings From `analysis_settings()`.
#' @return A list: `trends` (one row per variable) and `periods` (one row
#'   per variable and five-year period with the minimum number of datasets:
#'   the median dataset's value and the middle half of datasets, each dataset
#'   counted once).
trend_across_datasets <- function(
  screen,
  units,
  exposure_registry,
  breaks,
  statistic = "mean",
  settings = analysis_settings()
) {
  overall <- screen[
    screen$grouping %in% "all" & !screen$variable_name %in% "total_screen",
  ]
  overall$yi <- overall[[statistic]]
  overall$sei <- if (statistic == "mean") {
    overall$se_mean
  } else {
    sqrt(pmax(overall$yi * (1 - overall$yi), 1e-4) / overall$n)
  }
  overall <- dplyr::left_join(
    overall[c("dataset_id", "unit", "variable_name", "n", "yi", "sei")],
    units[c(
      "unit",
      "wave",
      "country",
      "year",
      "age_median",
      "measure_type",
      "responder"
    )],
    by = "unit"
  )
  overall <- overall[!is.na(overall$year), ]
  exposure_of <- exposure_registry$exposure[match(
    overall$variable_name,
    exposure_registry$variable_name
  )]
  day_of <- exposure_registry$day[match(
    overall$variable_name,
    exposure_registry$variable_name
  )]
  overall$series <- NA_character_
  for (exposure in unique(exposure_of)) {
    rows <- exposure_of %in% exposure
    overall$series[rows] <- assign_series(
      overall[rows, ],
      exposure,
      breaks,
      settings,
      day_of[rows]
    )
  }

  trends <- lapply(split(overall, overall$variable_name), function(cell) {
    row <- regress_units_on_year(cell, settings)
    row$variable_name <- cell$variable_name[1]
    row
  })
  trends <- dplyr::relocate(dplyr::bind_rows(trends), "variable_name")

  # For the figure: each dataset once per five-year period.
  overall$period <- 5 * floor(overall$year / 5)
  per_dataset <- dplyr::summarise(
    overall,
    value = stats::weighted.mean(.data$yi, .data$n),
    rows = sum(.data$n),
    .by = c("variable_name", "period", "dataset_id")
  )
  periods <- dplyr::summarise(
    per_dataset,
    datasets = dplyr::n(),
    rows = sum(.data$rows),
    mid = stats::median(.data$value),
    low = stats::quantile(.data$value, 0.25, names = FALSE),
    high = stats::quantile(.data$value, 0.75, names = FALSE),
    .by = c("variable_name", "period")
  )
  periods <- periods[periods$datasets >= settings$min_datasets, ]
  list(trends = trends, periods = periods, statistic = statistic)
}
