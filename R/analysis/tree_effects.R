#' Stage 1 slopes made ready for pooling up the outcome tree
#'
#' Keeps the fitted slopes of rows that pool (`synthesis` TRUE, in an L1
#' node, with a direction), and puts them on one scale and one direction so
#' rows of a specific outcome, and specific outcomes of a family, can be
#' averaged:
#'
#' - a slope in log odds is divided by pi / sqrt(3) to join the SD scale
#'   (ANALYSIS_PLAN.md, Step 7 task 3);
#' - a slope on a row where higher is worse changes sign, so a positive
#'   slope always means a better outcome with more screen use.
#'
#' @param estimates Stage 1 rows, from `fit_stage1()`.
#' @param outcome_registry The outcome registry.
#' @param settings From `analysis_settings()`.
#' @return The kept rows with `node`, `family`, `informant`, the aligned
#'   `estimate` and `se` in SD per hour, and `natural_estimate`,
#'   `natural_se` and `flipped` recording what was changed.
tree_effects <- function(
  estimates,
  outcome_registry,
  settings = analysis_settings()
) {
  rows <- outcome_registry$rows
  tree <- outcome_registry$tree
  at <- match(estimates$outcome, rows$variable_name)
  node <- rows$node[at]
  out <- estimates
  out$node <- node
  out$family <- tree$parent[match(node, tree$node)]
  out$informant <- rows$informant[at]
  direction <- rows$direction[at]
  keep <- out$status %in%
    "fitted" &
    !is.na(out$estimate) &
    !is.na(out$se) &
    rows$synthesis[at] %in% TRUE &
    tree$level[match(node, tree$node)] %in% "L1" &
    direction %in% c("higher_better", "higher_worse")
  out <- out[keep, ]
  direction <- direction[keep]

  scale <- ifelse(
    out$effect_measure %in% "log_odds_per_hour",
    settings$log_odds_per_sd,
    1
  )
  out$flipped <- direction == "higher_worse"
  out$natural_estimate <- out$estimate
  out$natural_se <- out$se
  out$estimate <- ifelse(out$flipped, -1, 1) * out$estimate / scale
  out$se <- out$se / scale
  out
}
