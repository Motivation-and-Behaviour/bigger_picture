#' Stage 1 slopes made ready for pooling up the outcome tree
#'
#' Marks which fitted slopes pool (`synthesis` TRUE, in an L1 node, with a
#' direction) and puts those on one scale and one direction, so rows of a
#' specific outcome, and specific outcomes of a family, can be averaged:
#'
#' - a slope in log odds is divided by pi / sqrt(3) to join the SD scale
#'   (ANALYSIS_PLAN.md, Step 7 task 3);
#' - a slope on a row where higher is worse changes sign, so a positive
#'   slope always means a better outcome with more screen use.
#'
#' @param estimates Stage 1 rows, from `fit_stage1()`.
#' @param outcome_registry The outcome registry.
#' @param settings From `analysis_settings()`.
#' @return The fitted rows with `node`, `family`, `informant` (`not
#'   recorded` where the registry has none), `pools` and, where a row does
#'   not pool, `left_out` saying why; the aligned `estimate` and `se` in SD
#'   per hour; and `natural_estimate`, `natural_se` and `flipped` recording
#'   what was changed. Unfitted slopes are not returned: stage 1 reports
#'   them.
tree_effects <- function(
  estimates,
  outcome_registry,
  settings = analysis_settings()
) {
  rows <- outcome_registry$rows
  tree <- outcome_registry$tree
  out <- estimates[
    estimates$status %in%
      "fitted" &
      !is.na(estimates$estimate) &
      !is.na(estimates$se),
  ]
  at <- match(out$outcome, rows$variable_name)
  out$node <- rows$node[at]
  out$family <- tree$parent[match(out$node, tree$node)]
  out$informant <- dplyr::coalesce(rows$informant[at], "not recorded")
  direction <- rows$direction[at]
  out$left_out <- dplyr::case_when(
    !rows$synthesis[at] %in% TRUE ~ "reported on its own",
    !tree$level[match(out$node, tree$node)] %in% "L1" ~ "no specific outcome",
    !direction %in% c("higher_better", "higher_worse") ~ "no direction",
    .default = NA_character_
  )
  out$pools <- is.na(out$left_out)

  scale <- ifelse(
    out$effect_measure %in% "log_odds_per_hour",
    settings$log_odds_per_sd,
    1
  )
  out$flipped <- direction %in% "higher_worse"
  out$natural_estimate <- out$estimate
  out$natural_se <- out$se
  out$estimate <- ifelse(out$flipped, -1, 1) * out$estimate / scale
  out$se <- out$se / scale
  out
}
