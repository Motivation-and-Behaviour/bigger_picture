#' Read the preregistered predictions
#'
#' One row per exposure and outcome family the preregistration predicts
#' about: a `negative` or `positive` association, or `none`, which is tested
#' for equivalence against the smallest effect of interest.
#'
#' @param path Path to `hypotheses.csv`.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @return A tibble of `hypothesis`, `exposure`, `family`, `prediction` and
#'   `notes`.
read_hypotheses <- function(
  path = bp_hypotheses_path(),
  outcome_registry,
  exposure_registry
) {
  out <- read_registry_csv(path, bp_hypotheses_columns())
  fail <- function(...) stop(basename(path), ": ", ..., call. = FALSE)
  if (anyNA(out[c("hypothesis", "exposure", "family", "prediction")])) {
    fail("only `notes` may be blank")
  }
  bad <- setdiff(out$prediction, c("negative", "positive", "none"))
  if (length(bad) > 0) {
    fail("unknown prediction: ", paste(bad, collapse = ", "))
  }
  tree <- outcome_registry$tree
  bad <- setdiff(out$family, tree$node[tree$level %in% "L0"])
  if (length(bad) > 0) {
    fail("not outcome families: ", paste(bad, collapse = ", "))
  }
  known <- c(bp_total_exposure(), exposure_registry$variable_name)
  bad <- setdiff(out$exposure, known)
  if (length(bad) > 0) {
    fail("not exposures: ", paste(bad, collapse = ", "))
  }
  if (anyDuplicated(out[c("exposure", "family")]) > 0) {
    fail("an exposure and family has two predictions")
  }
  out
}
