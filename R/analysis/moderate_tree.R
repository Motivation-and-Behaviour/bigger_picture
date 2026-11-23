#' Stage 2 moderators, for every exposure, specific outcome and family
#'
#' Applies `moderate_tree_group()` to each categorical moderator in
#' `bp_tree_moderators()` within each specific outcome (L1), and within
#' each family across its specific outcomes (L0), where the model has a
#' term for each specific outcome. It reuses the
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
#' @param levels `"L1"`, `"L0"` or both. The family level is not yet fit
#'   for use and is off by default: with a term for each specific outcome,
#'   only the datasets of specific outcomes that hold two or more of the
#'   compared levels inform the comparison, and the count of datasets per
#'   level does not yet allow for that, so a test can rest on one dataset
#'   and reject far too often.
#' @return One row per exposure, specific outcome or family (`tree_level`
#'   L1 or L0; `node` is NA for a family), moderator and level, as
#'   `moderate_tree_group()` gives them. A moderator with one level or none
#'   in a specific outcome has its rows too, so what could not be compared
#'   is on record. `same_as` names an earlier moderator that sorts the
#'   specific outcome's slopes into the same groups, so the two are one
#'   comparison under two names.
moderate_tree <- function(
  estimates,
  units,
  outcome_registry,
  correlations = NULL,
  mapping_status = NULL,
  datasets = NULL,
  settings = default_analysis_settings(),
  levels = "L1"
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
  # Each specific outcome, and each family across its specific outcomes.
  effects$tree_level <- "L1"
  whole <- effects
  whole$tree_level <- "L0"
  groups <- c(
    if ("L1" %in% levels) split(effects, paste(effects$exposure, effects$node)),
    if ("L0" %in% levels) {
      split(whole, paste(whole$exposure, whole$family, "L0"))
    }
  )
  out <- lapply(groups, function(rows) {
    family_level <- rows$tree_level[1] == "L0"
    if ("exposure" %in% names(correlations)) {
      correlations <- correlations[
        correlations$exposure %in% rows$exposure[1],
      ]
    }
    # Two labels that sort the slopes into the same groups are one test.
    grouping <- vapply(
      bp_tree_moderators(),
      function(moderator) {
        level <- as.character(rows[[moderator]])
        paste(match(level, unique(level)), collapse = " ")
      },
      character(1)
    )
    dplyr::bind_rows(lapply(bp_tree_moderators(), function(moderator) {
      result <- moderate_tree_group(
        rows,
        moderator,
        correlations,
        settings,
        adjust = if (family_level) "node"
      )
      result$exposure <- rows$exposure[1]
      result$family <- rows$family[1]
      result$node <- if (family_level) NA_character_ else rows$node[1]
      result$tree_level <- rows$tree_level[1]
      result$moderator <- moderator
      earlier <- names(grouping)[
        seq_along(grouping) < match(moderator, names(grouping)) &
          grouping == grouping[[moderator]]
      ]
      labelled <- any(!is.na(rows[[moderator]]))
      result$same_as <- if (labelled && length(earlier) > 0) {
        earlier[1]
      } else {
        NA_character_
      }
      result
    }))
  })
  out <- dplyr::bind_rows(out)
  if (nrow(out) == 0) {
    return(tibble::tibble())
  }
  dplyr::relocate(
    out,
    "exposure",
    "tree_level",
    "family",
    "node",
    "moderator",
    "level"
  )
}
