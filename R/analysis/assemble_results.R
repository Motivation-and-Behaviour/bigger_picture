#' One results table for a set of models, with p, q and equivalence tests
#'
#' Binds what each model pooled at every level of the outcome tree, labels
#' each row with its model's tier and class, and adds:
#'
#' - `p`, two-sided, from the robust standard error and its degrees of
#'   freedom;
#' - `q` and `k_total` from the tiered false discovery rate procedure
#'   (`adjust_tiered_fdr()`), counted along `fdr_path`;
#' - the effect on the contrast of 1 against 4 hours a day
#'   (`contrast_estimate` and its interval), for estimates in SD per hour;
#' - `p_equivalence`, the test that the estimate lies within the smallest
#'   effect of interest (`test_equivalence()`), for estimates in SD per
#'   hour;
#' - `rating` and `rating_label`, the evidence platform's impact band for
#'   the effect on that contrast (`rate_effect()`, DR-0020);
#' - the preregistered `hypothesis` and `prediction` for the row's exposure
#'   and family, and for a prediction of no association `q_equivalence`,
#'   the equivalence p-values adjusted among themselves with the row's
#'   `k_total`, and `equivalent`, TRUE where that is below 0.05;
#' - `prediction_supported`, for the family estimate of a primary model:
#'   whether its q-value is below 0.05 in the predicted direction, or, for
#'   a prediction of no association, whether it is `equivalent`.
#'
#' Estimates at L0 to L2 are in SD per hour and aligned, positive where
#' more screen use goes with a better outcome, so a predicted `negative`
#' association is a negative estimate in every family. L3 rows are in their
#' own direction and effect measure.
#'
#' `finding` says how a row may be described: `finding` at L0 and L1 of a
#' confirmatory model, otherwise the model's class, or `exploratory` for
#' L2 and L3; `too few studies to say`
#' wherever that is the row's status; `not estimated` for a row that
#' could not be pooled at all; and `specific outcomes disagree` for a
#' family kept from the headline because they do (DR-0014).
#'
#' A test that could not be run has no p-value and is not counted. One with
#' too few studies to say keeps its p-value and its place in the count, but
#' is not ranked and has no q-value, and is never described as a finding.
#'
#' @param tree Rows of `pool_tree()` for any number of models, with
#'   `model_name`.
#' @param cells Rows of `pool_stage2_cells()` for the same models, with
#'   `model_name`; the L3 level. NULL for none.
#' @param tests Rows of `moderator_tests()`: tests that come with their own
#'   p-value (`p_test`), such as whether a moderator's levels differ. They
#'   are counted and adjusted with the rest, and have no equivalence test.
#'   NULL for none.
#' @param seen The development cells (`read_development_cells()`, with
#'   `exposure` and `outcome`): the exposures and families fitted before
#'   the freeze, whose rows are
#'   marked `seen_before_freeze` whatever `frozen` is. NULL for none.
#' @param model_spec From `read_model_spec()`.
#' @param hypotheses From `read_hypotheses()`.
#' @param outcome_registry The outcome registry.
#' @param settings From `analysis_settings()`.
#' @param frozen Whether the specification is frozen; until it is, every
#'   row is marked as a development result.
#' @return A tibble, one row per model, level and group.
assemble_results <- function(
  tree,
  cells,
  model_spec,
  hypotheses,
  outcome_registry,
  settings = default_analysis_settings(),
  frozen = FALSE,
  tests = NULL,
  seen = NULL
) {
  keep <- c(
    "model_name",
    "exposure",
    "level",
    "family",
    "node",
    "informant",
    "outcome",
    "effect_measure",
    "k_datasets",
    "k_units",
    "estimate",
    "se",
    "df",
    "ci_low",
    "ci_high",
    "tau_dataset",
    "tau_unit",
    "pi_low",
    "pi_high",
    "headline",
    "warning",
    "note",
    "lag_years",
    "lag_years_min",
    "lag_years_max",
    "k_nodes",
    "k_nodes_thin",
    "nodes_differ_p",
    "nodes_left_out",
    "moderator",
    "term",
    "p_test",
    "status"
  )
  if (!is.null(tree) && nrow(tree) > 0) {
    tree$outcome <- NA_character_
    tree$effect_measure <- "sd_per_hour"
  }
  if (!is.null(cells) && nrow(cells) > 0) {
    place <- tree_place(cells$outcome, outcome_registry)
    rows <- outcome_registry$rows
    cells$level <- "L3"
    cells$node <- place$node
    cells$family <- rows$prereg_family[match(
      cells$outcome,
      rows$variable_name
    )]
    cells$informant <- place$informant
  }
  out <- dplyr::bind_rows(tree, cells, tests)
  if (nrow(out) == 0) {
    return(tibble::tibble())
  }
  for (column in setdiff(keep, names(out))) {
    out[[column]] <- NA
  }
  out <- out[keep]

  unknown <- setdiff(out$model_name, model_spec$model_name)
  if (length(unknown) > 0) {
    stop(
      "assemble_results(): not in the model list: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  at <- match(out$model_name, model_spec$model_name)
  out$tier <- model_spec$tier[at]
  out$analysis_class <- model_spec$analysis_class[at]
  # A level the model list does not give the model is not reported.
  listed <- mapply(
    function(level, levels) level %in% levels,
    out$level,
    model_spec$levels[at]
  )
  # A test is made for a level on purpose, so one at a level its model is
  # not listed for is a mistake in the list or the code, not a row to drop.
  stray <- !listed & !is.na(out$p_test)
  if (any(stray)) {
    stop(
      "assemble_results(): tests at a level the model list does not give ",
      "their model: ",
      paste(unique(out$model_name[stray]), collapse = ", "),
      call. = FALSE
    )
  }
  out <- out[listed, ]
  at <- at[listed]
  primary <- model_spec$analysis_class[at] %in%
    "confirmatory" &
    model_spec$engine[at] %in% "two_stage" &
    is.na(model_spec$variant[at])
  out$finding <- dplyr::case_when(
    out$status %in% "too few studies to say" ~ "too few studies to say",
    !out$status %in% "pooled" |
      (is.na(out$estimate) & is.na(out$p_test)) ~ "not estimated",
    out$level %in% "L0" & out$headline %in% FALSE ~
      "specific outcomes disagree",
    !out$analysis_class %in% "confirmatory" ~ out$analysis_class,
    out$level %in% c("L0", "L1") ~ "finding",
    .default = "exploratory"
  )
  out$development <- !frozen
  # The exposures and families fitted while the code was developed were
  # seen before the freeze, and stay marked after it (deviation D13).
  out$seen_before_freeze <- FALSE
  if (!is.null(seen) && nrow(seen) > 0) {
    rows <- outcome_registry$rows
    seen_family <- rows$prereg_family[match(seen$outcome, rows$variable_name)]
    out$seen_before_freeze <- paste(out$exposure, out$family) %in%
      paste(seen$exposure, seen_family)
  }

  usable <- !is.na(out$estimate) & !is.na(out$se) & !is.na(out$df) & out$se > 0
  out$p <- NA_real_
  out$p[usable] <- 2 *
    stats::pt(
      -abs(out$estimate[usable] / out$se[usable]),
      df = out$df[usable]
    )
  # A test that brings its own p-value keeps it.
  given <- !is.na(out$p_test)
  out$p[given] <- out$p_test[given]
  out$p_test <- NULL
  out$fdr_path <- tiered_fdr_path(
    out$tier,
    out$level,
    out$family,
    out$exposure,
    primary,
    out$model_name
  )
  out$q <- NA_real_
  out$k_total <- NA_integer_
  placed <- !is.na(out$fdr_path)
  # A row with too few studies to say counts towards K and is not ranked;
  # nor is a family whose specific outcomes disagree, whose average is not
  # read as one estimate.
  thin <- out$status %in%
    "too few studies to say" |
    out$finding %in% "specific outcomes disagree"
  adjusted <- adjust_tiered_fdr(
    out$p[placed],
    out$fdr_path[placed],
    ranked = !thin[placed]
  )
  out$q[placed] <- adjusted$q
  out$k_total[placed] <- adjusted$k_total

  in_sd <- out$effect_measure %in% "sd_per_hour"
  # The contrast of 1 against 4 hours a day.
  hours <- 3
  out$contrast_estimate <- ifelse(in_sd, hours * out$estimate, NA_real_)
  out$contrast_low <- ifelse(in_sd, hours * out$ci_low, NA_real_)
  out$contrast_high <- ifelse(in_sd, hours * out$ci_high, NA_real_)
  equivalence <- test_equivalence(
    ifelse(in_sd, out$estimate, NA_real_),
    out$se,
    out$df,
    settings$smallest_effect_per_hour
  )
  out$p_equivalence <- equivalence$p_equivalence
  out$equivalent <- NA
  # The platform's impact band, for aligned estimates that have enough
  # studies to say; row-level estimates are in their own direction and are
  # not rated.
  rated <- rate_effect(
    out$contrast_estimate,
    out$contrast_low,
    out$contrast_high,
    out$p_equivalence,
    settings
  )
  # The longitudinal strand's slopes are other estimands (a lagged slope,
  # a slope of changes), and a null one is not evidence of no effect
  # (DR-0018), so they are not put in the bands.
  panel <- model_spec$sample[at] %in% "panel"
  # Nor is a model of moderation, whose estimate is a difference between
  # two groups' slopes and not an association (DR-0021).
  difference <- model_spec$engine[at] %in% "stage1_interaction"
  ratable <- in_sd &
    out$level %in% c("L0", "L1", "L2") &
    !thin &
    !panel &
    !difference &
    !out$finding %in% "not estimated"
  out$rating <- ifelse(ratable, rated$rating, NA_integer_)
  # A row that cannot be rated says why in the words of its `finding`.
  out$rating_label <- dplyr::case_when(
    ratable ~ rated$rating_label,
    !(in_sd & out$level %in% c("L0", "L1", "L2")) ~ NA_character_,
    panel ~ "not rated: longitudinal models are not rated",
    difference ~ "not rated: a difference between two groups' slopes",
    out$finding %in% "specific outcomes disagree" ~
      "not rated: specific outcomes disagree",
    out$finding %in% "too few studies to say" ~
      "not rated: too few studies to say",
    .default = "not rated: not estimated"
  )

  # The slope predictions, and those about moderation by sex and by
  # socioeconomic position, whose test is named after its model; those
  # about thresholds and location attach where those models are assembled.
  moderation <- hypotheses[
    hypotheses$test %in% c("moderation_sex", "moderation_ses"),
  ]
  hypotheses <- hypotheses[hypotheses$test %in% "slope", ]
  predicted <- match(
    paste(out$exposure, out$family),
    paste(hypotheses$exposure, hypotheses$family)
  )
  # Predictions are about the aligned estimates of the primary models.
  predicted[!primary | out$level %in% c("L2", "L3")] <- NA
  out$hypothesis <- hypotheses$hypothesis[predicted]
  out$prediction <- hypotheses$prediction[predicted]
  moderated <- match(
    paste(out$model_name, out$exposure, out$family),
    paste(moderation$test, moderation$exposure, moderation$family)
  )
  moderated[out$level %in% c("L2", "L3")] <- NA
  registered <- !is.na(moderated)
  out$hypothesis[registered] <- moderation$hypothesis[moderated[registered]]
  out$prediction[registered] <- moderation$prediction[moderated[registered]]
  out$q_equivalence <- NA_real_
  none <- out$prediction %in%
    "none" &
    !is.na(out$p_equivalence) &
    placed &
    !thin
  for (path in unique(out$fdr_path[none])) {
    index <- which(none & out$fdr_path == path)
    out$q_equivalence[index] <- stats::p.adjust(
      out$p_equivalence[index],
      method = "BH",
      n = out$k_total[index][1]
    )
  }
  # An estimate is called equivalent where it was predicted to show no
  # association, was pooled from enough studies to say, is not a family
  # whose specific outcomes disagree, and its adjusted equivalence p-value
  # is below 0.05.
  out$equivalent[none] <- out$q_equivalence[none] < 0.05
  # Whether a registered prediction is supported is said of the family
  # estimate of its primary model: a predicted direction with a q-value
  # below 0.05, or a prediction of no association shown equivalent. The
  # rating describes the size; this says whether the prediction held.
  family_test <- !is.na(out$prediction) & out$level %in% "L0" & !thin
  # A prediction that the association is stronger for girls, or for
  # children of lower socioeconomic position, is that the first group's
  # aligned slope is the lower of the two: a negative difference (DR-0021).
  stronger <- c("stronger_for_girls", "stronger_for_lower_ses")
  directional <- family_test &
    out$prediction %in% c("negative", "positive", stronger)
  out$prediction_supported <- NA
  out$prediction_supported[directional] <- !is.na(out$q[directional]) &
    out$q[directional] < 0.05 &
    sign(out$estimate[directional]) ==
      ifelse(out$prediction[directional] == "positive", 1, -1)
  no_effect <- family_test & out$prediction %in% "none"
  out$prediction_supported[no_effect] <- out$equivalent[no_effect] %in% TRUE
  dplyr::relocate(
    out,
    "model_name",
    "tier",
    "analysis_class",
    "finding",
    "development"
  )
}
