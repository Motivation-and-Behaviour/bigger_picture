#' Pool the longitudinal strand's slopes, model by model
#'
#' Each of the strand's models (`lagged`, `matched`, `within`) is pooled as
#' the cross-sectional slopes are: across datasets for each outcome row
#' (`pool_stage2_cells()`), and up the outcome tree (`pool_tree()`). A
#' dataset's pairs of waves take the place of its units, so its several
#' pairs are treated as dependent in the same way.
#'
#' The slopes of two outcome rows in one pair of waves are given the
#' default correlation: it is not estimated from the children's rows.
#'
#' @param estimates Rows of `fit_stage1_longitudinal()`.
#' @param cells A tibble of `exposure` and `outcome`, as for
#'   `pool_stage2_cells()`.
#' @param outcome_registry The outcome registry.
#' @param settings From `analysis_settings()`.
#' @return A list of two tibbles, each with `model_name`
#'   (`longitudinal_lagged` and so on): `tree`, the L0 to L2 rows, with the
#'   median `lag_years` of the pairs behind each family's row; and
#'   `cells`, the L3 rows.
pool_longitudinal <- function(
  estimates,
  cells,
  outcome_registry,
  settings = default_analysis_settings()
) {
  if (is.null(estimates) || nrow(estimates) == 0) {
    return(list(tree = tibble::tibble(), cells = tibble::tibble()))
  }
  models <- split(estimates, estimates$model)
  name <- function(model) paste0("longitudinal_", model)
  tree <- lapply(names(models), function(model) {
    rows <- models[[model]]
    out <- pool_tree(rows, outcome_registry, NULL, settings)
    if (nrow(out) == 0) {
      return(out)
    }
    fitted <- rows[rows$status %in% "fitted", ]
    fitted$family <- tree_place(fitted$outcome, outcome_registry)$family
    lags <- dplyr::summarise(
      fitted,
      lag_years = stats::median(.data$lag_years, na.rm = TRUE),
      .by = c("exposure", "family")
    )
    out <- dplyr::left_join(out, lags, by = c("exposure", "family"))
    out$lag_years[!out$level %in% "L0"] <- NA_real_
    out$model_name <- name(model)
    out
  })
  pooled <- lapply(names(models), function(model) {
    out <- pool_stage2_cells(models[[model]], cells, settings)
    out$model_name <- rep(name(model), nrow(out))
    out
  })
  list(tree = dplyr::bind_rows(tree), cells = dplyr::bind_rows(pooled))
}
