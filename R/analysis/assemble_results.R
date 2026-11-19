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
#' - the preregistered `hypothesis` and `prediction` for the row's exposure
#'   and family, and for a prediction of no association `q_equivalence`,
#'   the equivalence p-values adjusted among themselves with the row's
#'   `k_total`, and `equivalent`, TRUE where that is below 0.05.
#'
#' Estimates at L0 to L2 are in SD per hour and aligned, positive where
#' more screen use goes with a better outcome, so a predicted `negative`
#' association is a negative estimate in every family. L3 rows are in their
#' own direction and effect measure.
#'
#' `finding` says how a row may be described: `finding` at L0 and L1 of a
#' confirmatory model, otherwise the model's class, or `exploratory` for
#' L2 and L3 (ANALYSIS_PLAN.md, Step 10 task 6); `too few studies to say`
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
  frozen = FALSE
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
  out <- dplyr::bind_rows(tree, cells)
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
  out <- out[listed, ]
  at <- at[listed]
  primary <- model_spec$analysis_class[at] %in%
    "confirmatory" &
    model_spec$engine[at] %in% "two_stage" &
    is.na(model_spec$variant[at])
  out$finding <- dplyr::case_when(
    out$status %in% "too few studies to say" ~ "too few studies to say",
    !out$status %in% "pooled" | is.na(out$estimate) ~ "not estimated",
    out$level %in% "L0" & out$headline %in% FALSE ~
      "specific outcomes disagree",
    !out$analysis_class %in% "confirmatory" ~ out$analysis_class,
    out$level %in% c("L0", "L1") ~ "finding",
    .default = "exploratory"
  )
  out$development <- !frozen

  usable <- !is.na(out$estimate) & !is.na(out$se) & !is.na(out$df) & out$se > 0
  out$p <- NA_real_
  out$p[usable] <- 2 *
    stats::pt(
      -abs(out$estimate[usable] / out$se[usable]),
      df = out$df[usable]
    )
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

  # The slope predictions; those about thresholds and moderation attach
  # where those models are assembled.
  hypotheses <- hypotheses[hypotheses$test %in% "slope", ]
  predicted <- match(
    paste(out$exposure, out$family),
    paste(hypotheses$exposure, hypotheses$family)
  )
  # Predictions are about the aligned estimates of the primary models.
  predicted[!primary | out$level %in% c("L2", "L3")] <- NA
  out$hypothesis <- hypotheses$hypothesis[predicted]
  out$prediction <- hypotheses$prediction[predicted]
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
  dplyr::relocate(
    out,
    "model_name",
    "tier",
    "analysis_class",
    "finding",
    "development"
  )
}
