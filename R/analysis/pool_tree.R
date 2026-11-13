#' Pool one exposure's slopes up the outcome tree
#'
#' From the outcome rows (L3, pooled by `pool_stage2_cells()`) upward:
#' specific outcome by informant (L2), specific outcome (L1) and family
#' (L0). L2 and L1 pool rows with `pool_tree_group()`; L0 averages the
#' specific outcomes with `pool_tree_family()`. Estimates are in SD per hour
#' a day, positive where more screen use goes with a better outcome.
#'
#' @param estimates Stage 1 rows for any number of exposures.
#' @param outcome_registry The outcome registry.
#' @param correlations See `tree_covariance()`.
#' @param settings From `analysis_settings()`.
#' @return One row per exposure, level and group: `exposure`, `level`,
#'   `family`, `node`, `informant`, then the columns of `pool_tree_group()`
#'   or `pool_tree_family()`. An L2 group whose slopes are all log odds also
#'   gives `odds_ratio`, the odds of the better outcome per hour.
pool_tree <- function(
  estimates,
  outcome_registry,
  correlations = NULL,
  settings = analysis_settings()
) {
  effects <- tree_effects(estimates, outcome_registry, settings)
  if (nrow(effects) == 0) {
    return(tibble::tibble())
  }
  # One order throughout, so each node's covariance lines up with its rows.
  effects <- effects[
    order(effects$exposure, effects$family, effects$node, effects$unit),
  ]
  out <- list()
  add <- function(row, exposure, level, family, node, informant) {
    row$exposure <- exposure
    row$level <- level
    row$family <- family
    row$node <- node
    row$informant <- informant
    out[[length(out) + 1]] <<- row
  }
  for (one in split(effects, effects$exposure)) {
    exposure <- one$exposure[1]
    for (family_rows in split(one, one$family)) {
      family <- family_rows$family[1]
      groups <- list()
      for (node_rows in split(family_rows, family_rows$node)) {
        node <- node_rows$node[1]
        for (l2 in split(node_rows, node_rows$informant)) {
          row <- pool_tree_group(l2, correlations, settings)$row
          log_odds <- all(l2$effect_measure %in% "log_odds_per_hour")
          row$odds_ratio <- if (log_odds) {
            exp(row$estimate * settings$log_odds_per_sd)
          } else {
            NA_real_
          }
          add(row, exposure, "L2", family, node, l2$informant[1])
        }
        groups[[node]] <- pool_tree_group(node_rows, correlations, settings)
        add(groups[[node]]$row, exposure, "L1", family, node, NA_character_)
      }
      l0 <- pool_tree_family(family_rows, groups, settings)
      add(l0, exposure, "L0", family, NA_character_, NA_character_)
    }
  }
  dplyr::relocate(
    dplyr::bind_rows(out),
    "exposure",
    "level",
    "family",
    "node",
    "informant"
  )
}
