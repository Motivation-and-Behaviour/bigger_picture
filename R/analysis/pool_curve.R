#' Stage 2 for curves: pool one cell's curve across datasets
#'
#' A multivariate random-effects meta-analysis of the units' curve terms,
#' fitted by REML with each unit's own covariance of its terms, each term
#' varying between datasets and, where a dataset has several units, between
#' its units, with inference robust to the dependence of a dataset's units.
#' The pooled curve is read at each hour of `curve_grid` as the difference
#' from the low-use reference, in SD of the outcome.
#'
#' A unit supports an hour where it has the minimum number of rows within
#' `curve_window` hours of it. The curve is interpretable at an hour inside
#' the boundary knots (beyond them it is a straight line continued) that at
#' least the minimum number of datasets support, counting only datasets that
#' also support the reference (ANALYSIS_PLAN.md, Step 11 task 7).
#'
#' The threshold duration is read two ways (Step 11 task 1; DR-0016), each
#' the lowest interpretable hour above the reference that meets its rule:
#'
#' - conservative (`threshold_hours`): the whole interval lies beyond the
#'   smallest effect of interest from the reference, in either direction,
#'   with at least the minimum robust degrees of freedom. It is rarely wrong and
#'   usually late, and depends on how precisely the curve is estimated: the
#'   same curve gives a later threshold where datasets are fewer or differ
#'   more.
#' - central (`threshold_central_hours`): the estimate itself is beyond the
#'   smallest effect of interest and the interval excludes no difference,
#'   with the minimum degrees of freedom, at that hour and at the next hour
#'   read. It is close to the true crossing on average, and can be reported
#'   for a curve that levels off just short of the smallest effect.
#'
#' `threshold_holds` and `threshold_central_holds` say whether the curve's
#' estimate stays beyond the smallest effect, on the same side, at every
#' later interpretable hour; where FALSE the curve comes back. The
#' `_interval_holds` columns say whether the interval goes on meeting the
#' rule as well (wholly beyond for the conservative reading, excluding no
#' difference for the central one), whatever the degrees of freedom there:
#' an estimate that stays beyond with an interval that widens has not come
#' back.
#'
#' @param curves Rows of `fit_stage1_curves()` for one exposure and outcome.
#' @param settings From `analysis_settings()`.
#' @param higher_is `better` or `worse`, the outcome row's direction, so
#'   the threshold's direction can be given as better or worse; NA gives it
#'   as higher or lower.
#' @return A list: `curve`, one row per hour with the `estimate` (difference
#'   from the reference), robust `se`, `df` and 95% interval, the datasets
#'   supporting the hour and `interpretable`; and `summary`, one row with
#'   the counts, the units left out, each threshold with its direction and
#'   whether it holds, the hours the curve is interpretable over, the
#'   random-effect levels used, and `status`.
pool_curve <- function(
  curves,
  settings = default_analysis_settings(),
  higher_is = NA_character_
) {
  grid <- settings$curve_grid
  reference <- settings$curve_reference
  fitted <- curves[curves$status %in% "fitted", ]
  not_fitted <- curves[!curves$status %in% "fitted", ]
  k_datasets <- dplyr::n_distinct(fitted$dataset_id)

  # The datasets with a unit supporting each hour and the reference.
  enough <- settings$min_unit_rows
  near <- matrix(
    vapply(
      fitted$rows_near,
      function(rows) rows >= enough,
      logical(length(grid))
    ),
    nrow = length(grid)
  )
  at_reference <- abs(grid - reference) < 1e-9
  supports_reference <- if (any(at_reference) && nrow(fitted) > 0) {
    near[which(at_reference)[1], ]
  } else {
    rep(TRUE, nrow(fitted))
  }
  k_support <- vapply(
    seq_along(grid),
    function(h) {
      dplyr::n_distinct(fitted$dataset_id[near[h, ] & supports_reference])
    },
    integer(1)
  )
  inside <- grid >= settings$curve_boundary[1] &
    grid <= settings$curve_boundary[2]
  curve <- tibble::tibble(
    hours = grid,
    estimate = NA_real_,
    se = NA_real_,
    se_floored = NA,
    df = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    k_datasets_support = k_support,
    interpretable = inside & k_support >= settings$min_datasets
  )
  finish <- function(status, ...) {
    values <- list(
      k_datasets = k_datasets,
      k_units = nrow(fitted),
      k_units_not_fitted = nrow(not_fitted),
      n = sum(fitted$n),
      reference_hours = reference,
      interpretable_from = NA_real_,
      interpretable_to = NA_real_,
      threshold_hours = NA_real_,
      threshold_direction = NA_character_,
      threshold_holds = NA,
      threshold_interval_holds = NA,
      threshold_central_hours = NA_real_,
      threshold_central_direction = NA_character_,
      threshold_central_holds = NA,
      threshold_central_interval_holds = NA,
      random_levels = NA_character_,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    list(curve = curve, summary = tibble::as_tibble(values))
  }
  # A curve from fewer datasets than this depends on how their differences
  # are modelled, so none is drawn.
  if (k_datasets < settings$curve_min_datasets) {
    return(finish("insufficient data"))
  }
  if (!curve$interpretable[which(at_reference)[1]]) {
    return(finish("too few datasets support the reference"))
  }

  terms <- names(fitted$coef[[1]])
  data <- data.frame(
    yi = unlist(fitted$coef, use.names = FALSE),
    term = factor(rep(terms, nrow(fitted)), levels = terms),
    dataset_id = rep(fitted$dataset_id, each = length(terms)),
    unit = rep(fitted$unit, each = length(terms))
  )
  sampling <- as.matrix(Matrix::bdiag(fitted$vcov))
  # Terms vary between datasets and, where there are more units than
  # datasets, between a dataset's units; the simpler model is the fallback.
  structures <- list(
    `dataset, unit` = list(~ term | dataset_id, ~ term | unit),
    dataset = list(~ term | dataset_id)
  )
  if (nrow(fitted) == k_datasets) {
    structures <- structures["dataset"]
  }
  fit <- "no model tried"
  levels_used <- NA_character_
  for (levels in names(structures)) {
    for (optimiser in c("nlminb", "BFGS")) {
      random <- structures[[levels]]
      fit <- tryCatch(
        metafor::rma.mv(
          yi,
          sampling,
          mods = ~ 0 + term,
          random = random,
          struct = rep("DIAG", length(random)),
          data = data,
          method = "REML",
          test = "t",
          control = list(optimizer = optimiser)
        ),
        error = function(e) conditionMessage(e)
      )
      if (!is.character(fit)) {
        break
      }
    }
    if (!is.character(fit)) {
      levels_used <- levels
      break
    }
  }
  if (is.character(fit)) {
    return(finish(paste("model failed:", fit)))
  }
  # Each hour against the reference: a contrast of the curve terms.
  contrasts <- sweep(
    curve_basis(grid, settings),
    2,
    curve_basis(reference, settings)[1, ]
  )
  robust <- tryCatch(
    clubSandwich::linear_contrast(
      fit,
      vcov = "CR2",
      cluster = data$dataset_id,
      contrasts = contrasts[!at_reference, , drop = FALSE]
    ),
    error = function(e) conditionMessage(e)
  )
  if (is.character(robust)) {
    return(finish(paste("robust inference failed:", robust)))
  }
  # The robust standard error is not allowed below the working model's:
  # with few datasets it is sometimes smaller than the units' own sampling
  # error allows, and intervals there cover the truth too seldom.
  own <- contrasts[!at_reference, , drop = FALSE]
  model_se <- sqrt(pmax(diag(own %*% fit$vb %*% t(own)), 0))
  floored <- model_se > robust$SE
  se <- pmax(robust$SE, model_se)
  half <- stats::qt(0.975, df = robust$df) * se
  curve$estimate[at_reference] <- 0
  curve$estimate[!at_reference] <- robust$Est
  curve$se[!at_reference] <- se
  curve$se_floored[!at_reference] <- floored
  curve$df[!at_reference] <- robust$df
  curve$ci_low[!at_reference] <- robust$Est - half
  curve$ci_high[!at_reference] <- robust$Est + half

  readable <- curve$interpretable & curve$hours > reference
  sure <- readable & !is.na(curve$df) & curve$df >= settings$min_robust_df
  beyond <- !is.na(curve$ci_low) &
    (curve$ci_low > settings$smallest_effect |
      curve$ci_high < -settings$smallest_effect)
  # The first hour that meets a rule, its direction, and what the curve
  # does at every later interpretable hour: whether its estimate stays
  # beyond the smallest effect on the same side (`holds`), and whether its
  # interval goes on meeting the rule too (`interval_holds`). An estimate
  # that stays beyond with an interval that widens has not come back.
  read_threshold <- function(meets, interval_meets) {
    found <- readable & meets
    none <- list(
      hours = NA_real_,
      direction = NA_character_,
      holds = NA,
      interval_holds = NA
    )
    if (!any(found)) {
      return(none)
    }
    hours <- min(curve$hours[found])
    start <- curve$estimate[match(hours, curve$hours)]
    rises <- start > 0
    direction <- if (is.na(higher_is)) {
      if (rises) "higher outcome" else "lower outcome"
    } else if (rises == (higher_is == "better")) {
      "better outcome"
    } else {
      "worse outcome"
    }
    later <- readable & curve$hours > hours
    stays <- sign(curve$estimate) == sign(start) &
      abs(curve$estimate) >= settings$smallest_effect
    list(
      hours = hours,
      direction = direction,
      holds = all(stays[later]),
      interval_holds = all((stays & interval_meets)[later])
    )
  }
  conservative <- read_threshold(sure & beyond, beyond)
  # The central reading: the estimate beyond the smallest effect with an
  # interval that excludes no difference and enough degrees of freedom, at
  # this hour and at the next one read, so that an estimate brushing the
  # line at one hour is not a threshold.
  clear <- !is.na(curve$ci_low) & (curve$ci_low > 0 | curve$ci_high < 0)
  past <- sure & clear & abs(curve$estimate) >= settings$smallest_effect
  in_order <- order(curve$hours)
  next_too <- rep(FALSE, nrow(curve))
  next_too[in_order] <- c(
    (past & readable)[in_order][-1],
    FALSE
  )
  central <- read_threshold(past & next_too, clear)
  finish(
    if (any(sure)) "pooled" else "too few studies to say",
    interpretable_from = min(curve$hours[curve$interpretable]),
    interpretable_to = max(curve$hours[curve$interpretable]),
    threshold_hours = conservative$hours,
    threshold_direction = conservative$direction,
    threshold_holds = conservative$holds,
    threshold_interval_holds = conservative$interval_holds,
    threshold_central_hours = central$hours,
    threshold_central_direction = central$direction,
    threshold_central_holds = central$holds,
    threshold_central_interval_holds = central$interval_holds,
    random_levels = levels_used
  )
}

#' Pooled curves for every cell in a table of stage 1 curves
#'
#' @param curves Rows of `fit_stage1_curves()` for any number of cells.
#' @param settings From `analysis_settings()`.
#' @param outcome_registry The outcome registry, for each row's direction;
#'   NULL gives threshold directions as higher or lower.
#' @return A list of two tibbles, each with `exposure` and `outcome`:
#'   `curves`, the pooled curve of each cell by hour, and `summaries`, one
#'   row per cell.
pool_curve_cells <- function(
  curves,
  settings = default_analysis_settings(),
  outcome_registry = NULL
) {
  if (is.null(curves) || nrow(curves) == 0) {
    return(list(curves = tibble::tibble(), summaries = tibble::tibble()))
  }
  direction_of <- function(outcome) {
    if (is.null(outcome_registry)) {
      return(NA_character_)
    }
    rows <- outcome_registry$rows
    direction <- rows$direction[match(outcome, rows$variable_name)]
    unname(c(higher_better = "better", higher_worse = "worse")[direction])
  }
  cells <- split(curves, paste(curves$exposure, curves$outcome, sep = "\r"))
  pooled <- lapply(cells, function(cell) {
    out <- pool_curve(cell, settings, direction_of(cell$outcome[1]))
    label <- function(table) {
      table$exposure <- rep(cell$exposure[1], nrow(table))
      table$outcome <- rep(cell$outcome[1], nrow(table))
      dplyr::relocate(table, "exposure", "outcome")
    }
    list(curve = label(out$curve), summary = label(out$summary))
  })
  list(
    curves = dplyr::bind_rows(lapply(pooled, function(cell) cell$curve)),
    summaries = dplyr::bind_rows(lapply(pooled, function(cell) cell$summary))
  )
}
