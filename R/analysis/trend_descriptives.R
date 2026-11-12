#' Change over time in screen use, within repeated surveys
#'
#' Comparing periods across datasets mixes real change with which datasets
#' exist in each period, so the trend is estimated inside each repeated
#' survey and then pooled (ANALYSIS_PLAN.md, Step 6). A dataset contributes
#' when it does not follow the same participants (in a panel the year moves
#' with the children's age) and has units in three or more collection years.
#' Within a dataset the trend is the slope of a unit statistic on year, per
#' decade, with a country effect where the dataset has several countries and
#' units weighted by their rows.
#'
#' @param screen Combined `screen` tables from `describe_dataset()`.
#' @param statistic The unit statistic: `median`, `mean` or a share.
#' @param settings From `analysis_settings()`.
#' @return A list: `series` (one row per dataset, variable and year, the
#'   statistic averaged over the year's units), `slopes` (one row per dataset
#'   and variable) and `pooled` (one row per variable with three or more
#'   datasets: the mean change per decade across datasets, with its
#'   interval).
trend_descriptives <- function(
  screen,
  statistic = "median",
  settings = analysis_settings()
) {
  overall <- screen[
    screen$grouping %in%
      "all" &
      !screen$repeats_participants %in% TRUE &
      !is.na(screen$year),
  ]
  overall$value <- overall[[statistic]]
  overall$year <- round(overall$year)
  keys <- c("dataset_id", "variable_name")

  series <- dplyr::summarise(
    overall,
    value = stats::weighted.mean(.data$value, .data$n),
    rows = sum(.data$n),
    units = dplyr::n(),
    .by = dplyr::all_of(c(keys, "year"))
  )
  enough <- dplyr::summarise(
    series,
    years = dplyr::n_distinct(.data$year),
    .by = dplyr::all_of(keys)
  )
  enough <- enough[enough$years >= 3, ]
  series <- dplyr::semi_join(series, enough, by = keys)
  overall <- dplyr::semi_join(overall, enough, by = keys)

  slope_one <- function(cell) {
    several_countries <- dplyr::n_distinct(cell$country) > 1
    formula <- if (several_countries) {
      stats::as.formula("value ~ decade + country")
    } else {
      stats::as.formula("value ~ decade")
    }
    cell$decade <- (cell$year - 2010) / 10
    fit <- tryCatch(
      stats::lm(formula, data = cell, weights = cell$n),
      error = function(e) NULL
    )
    if (is.null(fit) || fit$df.residual < 1) {
      return(NULL)
    }
    table <- stats::coef(summary(fit))
    if (!"decade" %in% rownames(table)) {
      return(NULL)
    }
    tibble::tibble(
      dataset_id = cell$dataset_id[1],
      variable_name = cell$variable_name[1],
      first_year = min(cell$year),
      last_year = max(cell$year),
      years = dplyr::n_distinct(cell$year),
      units = nrow(cell),
      slope_per_decade = table["decade", "Estimate"],
      se = table["decade", "Std. Error"]
    )
  }
  cells <- split(overall, paste(overall$dataset_id, overall$variable_name))
  slopes <- dplyr::bind_rows(lapply(cells, slope_one))

  pool_one <- function(cell) {
    cell <- cell[!is.na(cell$se) & cell$se > 0, ]
    if (nrow(cell) < settings$min_datasets) {
      return(NULL)
    }
    fit <- tryCatch(
      metafor::rma(
        yi = cell$slope_per_decade,
        sei = cell$se,
        method = "REML",
        test = "knha"
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(NULL)
    }
    tibble::tibble(
      variable_name = cell$variable_name[1],
      datasets = nrow(cell),
      first_year = min(cell$first_year),
      last_year = max(cell$last_year),
      change_per_decade = as.numeric(fit$b[1]),
      ci_low = fit$ci.lb,
      ci_high = fit$ci.ub,
      tau = sqrt(fit$tau2)
    )
  }
  pooled <- if (nrow(slopes) == 0) {
    NULL
  } else {
    dplyr::bind_rows(lapply(split(slopes, slopes$variable_name), pool_one))
  }
  list(series = series, slopes = slopes, pooled = pooled, statistic = statistic)
}
