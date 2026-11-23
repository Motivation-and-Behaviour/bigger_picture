#' Read the preregistered predictions
#'
#' One row per test, exposure and outcome family the preregistration
#' predicts about. A `slope` is predicted `negative`, `positive` or `none`,
#' the last tested for equivalence against the smallest effect of interest.
#' The other tests are the threshold (H3) and moderation by location (H4,
#' predicted `none`), sex (H5b) and socioeconomic status (H5c).
#'
#' @param path Path to `hypotheses.csv`.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @return A tibble of `hypothesis`, `test`, `exposure`, `family`,
#'   `prediction` and `notes`.
read_hypotheses <- function(
  path = bp_hypotheses_path(),
  outcome_registry,
  exposure_registry
) {
  out <- read_registry_csv(path, bp_hypotheses_columns())
  fail <- function(...) stop(basename(path), ": ", ..., call. = FALSE)
  needed <- c("hypothesis", "test", "exposure", "family", "prediction")
  if (anyNA(out[needed])) {
    fail("only `notes` may be blank")
  }
  allowed <- bp_hypothesis_predictions()
  bad <- setdiff(out$test, names(allowed))
  if (length(bad) > 0) {
    fail("unknown test: ", paste(bad, collapse = ", "))
  }
  fits <- mapply(
    function(test, prediction) prediction %in% allowed[[test]],
    out$test,
    out$prediction
  )
  if (!all(fits)) {
    fail(
      "a prediction its test cannot make: ",
      paste(unique(out$prediction[!fits]), collapse = ", ")
    )
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
  if (anyDuplicated(out[c("test", "exposure", "family")]) > 0) {
    fail("a test of an exposure and family has two predictions")
  }
  out
}
