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
#'   typical gap between waves behind each family and specific outcome
#'   (`lag_years`, the median over datasets of each dataset's median, and
#'   the shortest and longest dataset); and
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
    # The typical gap behind each family and specific outcome: the median
    # over datasets of each dataset's median gap, with the shortest and
    # longest dataset, so that one large panel does not set it.
    fitted <- rows[rows$status %in% "fitted", ]
    place <- tree_place(fitted$outcome, outcome_registry)
    fitted$family <- place$family
    fitted$node <- place$node
    fitted <- fitted[is.na(place$left_out), ]
    gaps <- function(by) {
      per_dataset <- dplyr::summarise(
        fitted,
        gap = stats::median(.data$lag_years, na.rm = TRUE),
        .by = dplyr::all_of(c(by, "dataset_id"))
      )
      dplyr::summarise(
        per_dataset,
        lag_years = stats::median(.data$gap, na.rm = TRUE),
        lag_years_min = min(.data$gap, na.rm = TRUE),
        lag_years_max = max(.data$gap, na.rm = TRUE),
        .by = dplyr::all_of(by)
      )
    }
    family_gaps <- gaps(c("exposure", "family"))
    family_gaps$level <- rep("L0", nrow(family_gaps))
    node_gaps <- gaps(c("exposure", "family", "node"))
    node_gaps$level <- rep("L1", nrow(node_gaps))
    out <- dplyr::left_join(
      out,
      dplyr::bind_rows(family_gaps, node_gaps),
      by = c("exposure", "family", "node", "level")
    )
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
