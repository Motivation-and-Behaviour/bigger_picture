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
#' @param correlations See `tree_covariance()`; with an `exposure` column,
#'   as `stage1_correlations()` gives, each exposure uses its own rows.
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
  label <- function(row, exposure, level, family, node, informant) {
    row$exposure <- exposure
    row$level <- level
    row$family <- family
    row$node <- node
    row$informant <- informant
    row
  }
  families <- split(effects, paste(effects$exposure, effects$family))
  out <- lapply(families, function(family_rows) {
    exposure <- family_rows$exposure[1]
    family <- family_rows$family[1]
    nodes <- split(family_rows, family_rows$node)
    if ("exposure" %in% names(correlations)) {
      correlations <- correlations[correlations$exposure %in% exposure, ]
    }
    groups <- lapply(nodes, pool_tree_group, correlations, settings)
    l2 <- lapply(nodes, function(node_rows) {
      by_informant <- split(node_rows, node_rows$informant)
      dplyr::bind_rows(lapply(by_informant, function(rows) {
        row <- pool_tree_group(rows, correlations, settings)$row
        log_odds <- all(rows$effect_measure %in% "log_odds_per_hour")
        row$odds_ratio <- if (log_odds) {
          exp(row$estimate * settings$log_odds_per_sd)
        } else {
          NA_real_
        }
        label(row, exposure, "L2", family, rows$node[1], rows$informant[1])
      }))
    })
    l1 <- lapply(names(groups), function(node) {
      label(groups[[node]]$row, exposure, "L1", family, node, NA_character_)
    })
    l0 <- label(
      pool_tree_family(family_rows, groups, settings),
      exposure,
      "L0",
      family,
      NA_character_,
      NA_character_
    )
    dplyr::bind_rows(c(unname(l2), l1, list(l0)))
  })
  dplyr::relocate(
    dplyr::bind_rows(out),
    "exposure",
    "level",
    "family",
    "node",
    "informant"
  )
}
