#' Stage 2 for curves: pool one cell's curve across datasets
#'
#' A multivariate random-effects meta-analysis of the units' curve terms,
#' fitted by REML with each unit's own covariance of its terms and each term
#' varying between datasets, with inference robust to the dependence of a
#' dataset's units. The pooled curve is read at each hour of `curve_grid`
#' as the difference from the low-use reference, in SD of the outcome.
#'
#' A unit supports an hour where it has the minimum number of rows at or
#' above it and at or below it, and the curve is interpretable at an hour
#' that at least the minimum number of datasets support
#' (ANALYSIS_PLAN.md, Step 11 task 7).
#'
#' The threshold duration is the lowest interpretable hour above the
#' reference whose interval lies wholly beyond the smallest effect of
#' interest from the reference, in either direction (Step 11 task 1).
#'
#' @param curves Rows of `fit_stage1_curves()` for one exposure and outcome.
#' @param settings From `analysis_settings()`.
#' @return A list: `curve`, one row per hour with the `estimate` (difference
#'   from the reference), robust `se`, `df` and 95% interval, the datasets
#'   supporting the hour and `interpretable`; and `summary`, one row with
#'   the counts, `threshold_hours` and its `threshold_direction`, the hours
#'   the curve is interpretable over, and `status`.
pool_curve <- function(curves, settings = analysis_settings()) {
  grid <- settings$curve_grid
  reference <- settings$curve_reference
  fitted <- curves[curves$status %in% "fitted", ]
  k_datasets <- dplyr::n_distinct(fitted$dataset_id)

  # The datasets with a unit supporting each hour.
  enough <- settings$min_unit_rows
  supports <- vapply(
    seq_len(nrow(fitted)),
    function(i) {
      fitted$rows_at_or_above[[i]] >= enough &
        fitted$rows_at_or_below[[i]] >= enough
    },
    logical(length(grid))
  )
  supports <- matrix(supports, nrow = length(grid))
  k_support <- vapply(
    seq_along(grid),
    function(h) dplyr::n_distinct(fitted$dataset_id[supports[h, ]]),
    integer(1)
  )
  curve <- tibble::tibble(
    hours = grid,
    estimate = NA_real_,
    se = NA_real_,
    df = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    k_datasets_support = k_support,
    interpretable = k_support >= settings$min_datasets
  )
  finish <- function(status, ...) {
    values <- list(
      k_datasets = k_datasets,
      k_units = nrow(fitted),
      n = sum(fitted$n),
      reference_hours = reference,
      interpretable_from = NA_real_,
      interpretable_to = NA_real_,
      threshold_hours = NA_real_,
      threshold_direction = NA_character_,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    list(curve = curve, summary = tibble::as_tibble(values))
  }
  if (k_datasets < settings$min_datasets) {
    return(finish("insufficient data"))
  }

  terms <- names(fitted$coef[[1]])
  data <- data.frame(
    yi = unlist(fitted$coef, use.names = FALSE),
    term = factor(rep(terms, nrow(fitted)), levels = terms),
    dataset_id = rep(fitted$dataset_id, each = length(terms)),
    unit = rep(fitted$unit, each = length(terms))
  )
  sampling <- as.matrix(Matrix::bdiag(fitted$vcov))
  fit <- NULL
  for (optimiser in c("nlminb", "BFGS")) {
    fit <- tryCatch(
      metafor::rma.mv(
        yi,
        sampling,
        mods = ~ 0 + term,
        random = ~ term | dataset_id,
        struct = "DIAG",
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
  if (is.character(fit)) {
    return(finish(paste("model failed:", fit)))
  }
  # Each hour against the reference: a contrast of the curve terms.
  contrasts <- sweep(
    curve_basis(grid, settings),
    2,
    curve_basis(reference, settings)[1, ]
  )
  at_reference <- rowSums(abs(contrasts)) < 1e-12
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
  curve$estimate[at_reference] <- 0
  curve$estimate[!at_reference] <- robust$Est
  curve$se[!at_reference] <- robust$SE
  curve$df[!at_reference] <- robust$df
  curve$ci_low[!at_reference] <- robust$CI_L
  curve$ci_high[!at_reference] <- robust$CI_U

  readable <- curve$interpretable & curve$hours > reference
  beyond <- readable &
    !is.na(curve$ci_low) &
    (curve$ci_low > settings$smallest_effect |
      curve$ci_high < -settings$smallest_effect)
  threshold <- if (any(beyond)) min(curve$hours[beyond]) else NA_real_
  direction <- if (is.na(threshold)) {
    NA_character_
  } else if (curve$estimate[match(threshold, curve$hours)] > 0) {
    "higher outcome"
  } else {
    "lower outcome"
  }
  few <- any(curve$df[curve$interpretable & !at_reference] < 4, na.rm = TRUE)
  finish(
    if (few) "too few studies to say" else "pooled",
    interpretable_from = if (any(curve$interpretable)) {
      min(curve$hours[curve$interpretable])
    } else {
      NA_real_
    },
    interpretable_to = if (any(curve$interpretable)) {
      max(curve$hours[curve$interpretable])
    } else {
      NA_real_
    },
    threshold_hours = threshold,
    threshold_direction = direction
  )
}

#' Pooled curves for every cell in a table of stage 1 curves
#'
#' @param curves Rows of `fit_stage1_curves()` for any number of cells.
#' @param settings From `analysis_settings()`.
#' @return A list of two tibbles, each with `exposure` and `outcome`:
#'   `curves`, the pooled curve of each cell by hour, and `summaries`, one
#'   row per cell.
pool_curve_cells <- function(curves, settings = analysis_settings()) {
  if (is.null(curves) || nrow(curves) == 0) {
    return(list(curves = tibble::tibble(), summaries = tibble::tibble()))
  }
  cells <- split(curves, paste(curves$exposure, curves$outcome, sep = "\r"))
  pooled <- lapply(cells, function(cell) {
    out <- pool_curve(cell, settings)
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
