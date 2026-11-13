#' Stage 2 moderators, for every exposure and specific outcome
#'
#' Applies `moderate_tree_group()` to each categorical moderator in
#' `bp_tree_moderators()` within each specific outcome (L1). It reuses the
#' stage 1 slopes, so it adds no stage 1 fitting. Estimates are aligned, in
#' SD per hour a day, positive where more screen use goes with a better
#' outcome.
#'
#' @param estimates Stage 1 rows for any number of exposures.
#' @param units Per-unit descriptions, as the `units` table of
#'   `describe_dataset()`.
#' @param outcome_registry The outcome registry.
#' @param correlations See `tree_covariance()`.
#' @param mapping_status,datasets See `tree_moderators()`.
#' @param settings From `analysis_settings()`.
#' @return One row per exposure, specific outcome, moderator and level, as
#'   `moderate_tree_group()` gives them. A moderator with one level or none
#'   in a specific outcome has its rows too, so what could not be compared
#'   is on record.
moderate_tree <- function(
  estimates,
  units,
  outcome_registry,
  correlations = NULL,
  mapping_status = NULL,
  datasets = NULL,
  settings = analysis_settings()
) {
  effects <- tree_effects(estimates, outcome_registry, settings)
  effects <- effects[effects$pools, ]
  if (nrow(effects) == 0) {
    return(tibble::tibble())
  }
  effects <- tree_moderators(effects, units, mapping_status, datasets)
  effects <- effects[
    order(effects$exposure, effects$family, effects$node, effects$unit),
  ]
  groups <- split(effects, paste(effects$exposure, effects$node))
  out <- lapply(groups, function(rows) {
    if ("exposure" %in% names(correlations)) {
      correlations <- correlations[
        correlations$exposure %in% rows$exposure[1],
      ]
    }
    dplyr::bind_rows(lapply(bp_tree_moderators(), function(moderator) {
      result <- moderate_tree_group(rows, moderator, correlations, settings)
      if (nrow(result) == 0) {
        return(NULL)
      }
      result$exposure <- rows$exposure[1]
      result$family <- rows$family[1]
      result$node <- rows$node[1]
      result$moderator <- moderator
      result
    }))
  })
  out <- dplyr::bind_rows(out)
  if (nrow(out) == 0) {
    return(tibble::tibble())
  }
  dplyr::relocate(out, "exposure", "family", "node", "moderator", "level")
}
