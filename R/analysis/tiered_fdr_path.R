#' Where a test sits in the tiered false discovery rate procedure
#'
#' Gives each test the path `adjust_tiered_fdr()` counts along (DR-0017).
#' The layers follow the preregistration's tiers:
#'
#' - Tier 1, the five families, for overall screen use: `families`;
#' - Tier 2, the content classes within a family:
#'   `families > classes:<family>`;
#' - a specific outcome (L1) sits under its family's test for the same
#'   exposure: `... > specific:<family>:<exposure>`;
#' - a specific outcome by informant (L2) and an outcome row (L3) sit under
#'   that specific outcome's layer, as `informants` and `rows`;
#' - a secondary model of Tier 1 or 2, such as the content classes adjusted
#'   for other screen use, sits under the primary test at its level, in a
#'   layer named after the model;
#' - Tier 3 (thresholds), Tier 4 (moderation) and Tier 5 (exploratory and
#'   sensitivity analyses) each form one layer per family, under the
#'   family's content classes and each other in that order.
#'
#' @param tier The model's tier, 1 to 5.
#' @param level `L0`, `L1`, `L2` or `L3`.
#' @param family The outcome family.
#' @param exposure The exposure.
#' @param primary Whether the model is the tier's primary one; for Tiers 1
#'   and 2 only.
#' @param model_name The model, naming the layer of a secondary model.
#' @return A character vector of paths; NA where the family is unknown.
tiered_fdr_path <- function(
  tier,
  level,
  family,
  exposure,
  primary,
  model_name
) {
  classes <- paste0("families > classes:", family)
  top <- ifelse(tier == 1, "families", classes)
  specific <- paste0(top, " > specific:", family, ":", exposure)
  within <- dplyr::case_when(
    level %in% "L0" ~ top,
    level %in% "L1" ~ specific,
    level %in% "L2" ~ paste0(specific, " > informants"),
    level %in% "L3" ~ paste0(specific, " > rows")
  )
  thresholds <- paste0(classes, " > thresholds:", family)
  moderation <- paste0(thresholds, " > moderation:", family)
  path <- dplyr::case_when(
    tier <= 2 & primary ~ within,
    tier <= 2 ~ paste0(within, " > ", model_name),
    tier == 3 ~ thresholds,
    tier == 4 ~ moderation,
    tier == 5 ~ paste0(moderation, " > exploratory:", family)
  )
  path[is.na(family)] <- NA_character_
  path
}
