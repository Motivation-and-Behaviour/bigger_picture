#' Stage 1 slopes made ready for pooling up the outcome tree
#'
#' Marks which fitted slopes pool (`synthesis` TRUE, in an L1 node, with a
#' direction) and puts those on one scale and one direction, so rows of a
#' specific outcome, and specific outcomes of a family, can be averaged:
#'
#' - a slope in log odds is divided by pi / sqrt(3) to join the SD scale;
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
  settings = default_analysis_settings()
) {
  out <- estimates[
    estimates$status %in%
      "fitted" &
      !is.na(estimates$estimate) &
      !is.na(estimates$se),
  ]
  place <- tree_place(out$outcome, outcome_registry)
  out$node <- place$node
  out$family <- place$family
  out$informant <- place$informant
  direction <- place$direction
  out$left_out <- place$left_out
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

#' Where outcome rows sit in the tree, and whether they pool
#'
#' @param outcome Outcome row names.
#' @param outcome_registry The outcome registry.
#' @return A tibble, one row per element of `outcome`: `node`, `family`,
#'   `informant` (`not recorded` where the registry has none), `direction`
#'   and `left_out`, the reason the row does not pool above itself, or NA.
tree_place <- function(outcome, outcome_registry) {
  rows <- outcome_registry$rows
  tree <- outcome_registry$tree
  at <- match(outcome, rows$variable_name)
  node <- rows$node[at]
  direction <- rows$direction[at]
  tibble::tibble(
    node = node,
    family = tree$parent[match(node, tree$node)],
    informant = dplyr::coalesce(rows$informant[at], "not recorded"),
    direction = direction,
    left_out = dplyr::case_when(
      !rows$synthesis[at] %in% TRUE ~ "reported on its own",
      !tree$level[match(node, tree$node)] %in% "L1" ~ "no specific outcome",
      !direction %in% c("higher_better", "higher_worse") ~ "no direction",
      .default = NA_character_
    )
  )
}
