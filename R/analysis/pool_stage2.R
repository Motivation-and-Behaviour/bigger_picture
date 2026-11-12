#' Stage 2: pool one cell's unit slopes across datasets
#'
#' A random-effects multilevel meta-analysis fitted by REML, with units
#' nested in datasets, and inference robust to the dependence of a dataset's
#' units (DR-0010). It reports the mean slope, how much the slope varies
#' between datasets, and a prediction interval for a new dataset. There is no
#' fallback to a simpler model: a fit that fails is reported as failed.
#'
#' A cell is pooled only from the minimum number of datasets. Where the
#' robust estimate has fewer than four degrees of freedom the cell is marked
#' `too few studies to say`, because an interval that wide cannot support
#' "no clear relationship" either.
#'
#' Standard model-based inference is reported alongside the robust interval,
#' and the prediction interval is approximate, because dataset sizes vary
#' widely.
#'
#' @param estimates Stage 1 rows for one exposure and outcome, with
#'   `dataset_id`, `unit`, `estimate` and `se`; rows that were not fitted are
#'   ignored.
#' @param settings From `analysis_settings()`.
#' @return A one-row tibble: counts of datasets and units, the pooled
#'   `estimate` with its robust `se`, `df` and 95% interval, the model-based
#'   interval, the between-dataset and between-unit SDs, the prediction
#'   interval and `status`.
pool_stage2 <- function(estimates, settings = analysis_settings()) {
  fitted <- estimates[
    estimates$status %in%
      "fitted" &
      !is.na(estimates$estimate) &
      !is.na(estimates$se),
  ]
  k_datasets <- dplyr::n_distinct(fitted$dataset_id)
  result <- function(status, ...) {
    values <- list(
      k_datasets = k_datasets,
      k_units = nrow(fitted),
      n = sum(fitted$n),
      estimate = NA_real_,
      se = NA_real_,
      df = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      model_ci_low = NA_real_,
      model_ci_high = NA_real_,
      tau_dataset = NA_real_,
      tau_unit = NA_real_,
      pi_low = NA_real_,
      pi_high = NA_real_,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    tibble::as_tibble(values)
  }
  if (k_datasets < settings$min_datasets) {
    return(result("insufficient data"))
  }

  data <- data.frame(
    yi = fitted$estimate,
    vi = fitted$se^2,
    dataset_id = fitted$dataset_id,
    unit = fitted$unit
  )
  # The unit level is only estimable when some dataset has several units.
  nested <- nrow(data) > k_datasets
  random <- if (nested) {
    stats::as.formula("~ 1 | dataset_id / unit")
  } else {
    stats::as.formula("~ 1 | dataset_id")
  }
  fit <- tryCatch(
    metafor::rma.mv(
      yi,
      vi,
      random = random,
      data = data,
      method = "REML",
      test = "t"
    ),
    error = function(e) conditionMessage(e)
  )
  if (is.character(fit)) {
    return(result(paste("model failed:", fit)))
  }
  robust <- tryCatch(
    clubSandwich::conf_int(fit, vcov = "CR2", cluster = data$dataset_id),
    error = function(e) conditionMessage(e)
  )
  if (is.character(robust)) {
    return(result(paste("robust inference failed:", robust)))
  }

  estimate <- as.numeric(fit$b[1])
  between <- sum(fit$sigma2)
  critical <- stats::qt(0.975, df = robust$df[1])
  half_width <- critical * sqrt(robust$SE[1]^2 + between)
  result(
    if (robust$df[1] < 4) "too few studies to say" else "pooled",
    estimate = estimate,
    se = robust$SE[1],
    df = robust$df[1],
    ci_low = robust$CI_L[1],
    ci_high = robust$CI_U[1],
    model_ci_low = as.numeric(fit$ci.lb[1]),
    model_ci_high = as.numeric(fit$ci.ub[1]),
    tau_dataset = sqrt(fit$sigma2[1]),
    tau_unit = if (nested) sqrt(fit$sigma2[2]) else NA_real_,
    pi_low = estimate - half_width,
    pi_high = estimate + half_width
  )
}
