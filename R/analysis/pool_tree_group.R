#' Pool the slopes of several outcome rows into one group of the tree
#'
#' The group is a specific outcome (L1) or a specific outcome by informant
#' (L2). The working model is a random-effects meta-analysis fitted by REML
#' with the rows' sampling covariance (`tree_covariance()`) and slopes
#' varying between datasets, between a dataset's units and, where a unit
#' gives several rows, between a unit's rows. Inference is robust to the
#' dependence of a dataset's slopes, as in `pool_stage2()`, whose rules on
#' the minimum number of datasets and on too few degrees of freedom apply.
#'
#' @param effects Rows from `tree_effects()` for one group.
#' @param correlations See `tree_covariance()`.
#' @param settings From `analysis_settings()`.
#' @return A list: `row`, a one-row tibble shaped like `pool_stage2()`'s with
#'   `k_rows` (outcome rows), `pairs_assumed` and `units_repaired` (see
#'   `tree_covariance()`) and `optimiser` added; `M`, the working model's
#'   covariance of the slopes in the order of `effects` (NULL when the group
#'   was not pooled); and `keys`, the unit and outcome row of each slope in
#'   that order.
pool_tree_group <- function(
  effects,
  correlations = NULL,
  settings = default_analysis_settings()
) {
  k_datasets <- dplyr::n_distinct(effects$dataset_id)
  keys <- paste(effects$unit, effects$outcome, sep = "\r")
  result <- function(status, ...) {
    values <- list(
      k_datasets = k_datasets,
      k_units = dplyr::n_distinct(effects$unit),
      k_rows = dplyr::n_distinct(effects$outcome),
      k_slopes = nrow(effects),
      estimate = NA_real_,
      se = NA_real_,
      df = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      tau_dataset = NA_real_,
      tau_unit = NA_real_,
      tau_row = NA_real_,
      pi_low = NA_real_,
      pi_high = NA_real_,
      pairs_assumed = NA_integer_,
      units_repaired = NA_integer_,
      optimiser = NA_character_,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    tibble::as_tibble(values)
  }
  if (k_datasets < settings$min_datasets) {
    return(list(row = result("insufficient data"), M = NULL, keys = keys))
  }

  working <- fit_tree_model(effects, correlations, settings)
  if (is.character(working)) {
    return(list(
      row = result(paste("model failed:", working)),
      M = NULL,
      keys = keys
    ))
  }
  fit <- working$fit
  data <- working$data
  levels <- working$levels
  sampling <- working$sampling
  optimiser <- working$optimiser
  robust <- tryCatch(
    clubSandwich::conf_int(fit, vcov = "CR2", cluster = data$dataset_id),
    error = function(e) conditionMessage(e)
  )
  if (is.character(robust)) {
    return(list(
      row = result(paste("robust inference failed:", robust)),
      M = NULL,
      keys = keys
    ))
  }

  tau <- stats::setNames(sqrt(fit$sigma2), levels)
  estimate <- as.numeric(fit$b[1])
  critical <- stats::qt(0.975, df = robust$df[1])
  half_width <- critical * sqrt(robust$SE[1]^2 + sum(fit$sigma2))
  row <- result(
    if (robust$df[1] < 4) "too few studies to say" else "pooled",
    estimate = estimate,
    se = robust$SE[1],
    df = robust$df[1],
    ci_low = robust$CI_L[1],
    ci_high = robust$CI_U[1],
    tau_dataset = unname(tau["dataset_id"]),
    tau_unit = unname(tau["unit"]),
    tau_row = unname(tau["slope"]),
    pi_low = estimate - half_width,
    pi_high = estimate + half_width,
    pairs_assumed = sampling$assumed,
    units_repaired = sampling$repaired,
    optimiser = optimiser
  )
  list(row = row, M = unclass(fit$M), keys = keys)
}
