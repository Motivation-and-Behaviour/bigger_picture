#' The working model for a set of aligned slopes
#'
#' A random-effects meta-analysis fitted by REML with the slopes' sampling
#' covariance (`tree_covariance()`) and slopes varying between datasets,
#' between a dataset's units and, where a unit gives several slopes, between
#' a unit's slopes. A level is included only when it has more members than
#' the one above it. The default optimiser is tried first and a second one
#' where it does not converge.
#'
#' @param effects Rows from `tree_effects()`.
#' @param correlations See `tree_covariance()`.
#' @param settings From `analysis_settings()`.
#' @param mods A one-sided formula of moderators among the columns of
#'   `effects`, or NULL for one mean.
#' @return A list of `fit`, `data` (the model's data, in the order of
#'   `effects`), `levels` (the random-effect levels used), `sampling` (from
#'   `tree_covariance()`) and `optimiser`; or the error message as a string.
fit_tree_model <- function(
  effects,
  correlations = NULL,
  settings = default_analysis_settings(),
  mods = NULL
) {
  sampling <- tree_covariance(effects, correlations, settings)
  data <- as.data.frame(effects)
  data$yi <- effects$estimate
  data$slope <- seq_len(nrow(effects))
  k_datasets <- dplyr::n_distinct(data$dataset_id)
  k_units <- dplyr::n_distinct(data$unit)
  levels <- c(
    "dataset_id",
    if (k_units > k_datasets) "unit",
    if (nrow(data) > k_units) "slope"
  )
  random <- stats::as.formula(paste("~ 1 |", paste(levels, collapse = " / ")))
  arguments <- list(
    yi = data$yi,
    V = sampling$V,
    random = random,
    data = data,
    method = "REML",
    test = "t"
  )
  if (!is.null(mods)) {
    arguments$mods <- mods
  }
  fit <- NULL
  for (optimiser in c("nlminb", "BFGS")) {
    arguments$control <- list(optimizer = optimiser)
    fit <- tryCatch(
      do.call(metafor::rma.mv, arguments),
      error = function(e) conditionMessage(e)
    )
    if (!is.character(fit)) {
      break
    }
  }
  if (is.character(fit)) {
    return(fit)
  }
  list(
    fit = fit,
    data = data,
    levels = levels,
    sampling = sampling,
    optimiser = optimiser
  )
}
