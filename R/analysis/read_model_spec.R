#' Read and check the model list
#'
#' `models.csv` has one row per model family: everything the analysis fits
#' is a row here, and a sensitivity analysis is a further row, not a further
#' code path (ANALYSIS_PLAN.md, Design principles). A row names sets of
#' exposures and outcomes, which `expand_model_cells()` turns into cells
#' against the registries, so the list stays short.
#'
#' @param path Path to `models.csv`.
#' @return A tibble with the columns of `bp_models_columns()`: `step` and
#'   `tier` as integers, `built` as logical, and `levels`, `exposure_set`
#'   and `moderator` as list columns of their pipe-separated parts.
read_model_spec <- function(path = bp_models_path()) {
  spec <- read_registry_csv(path, bp_models_columns(), "built")
  fail <- function(...) stop(basename(path), ": ", ..., call. = FALSE)
  needed <- setdiff(bp_models_columns(), c("moderator", "variant", "notes"))
  for (column in needed) {
    if (anyNA(spec[[column]])) {
      fail("`", column, "` is blank in a row")
    }
  }
  if (anyDuplicated(spec$model_name) > 0) {
    fail("model names repeat")
  }
  partner <- sub("_other$", "", grep("_other$", spec$model_name, value = TRUE))
  if (!all(partner %in% spec$model_name)) {
    fail("a model named `<model>_other` needs `<model>` in the list")
  }
  confirmed <- spec$analysis_class %in% "confirmatory" & spec$tier == "5"
  if (any(confirmed)) {
    fail(
      "confirmatory models take tiers 1 to 4: ",
      paste(spec$model_name[confirmed], collapse = ", ")
    )
  }
  if (!all(grepl("^[a-z][a-z0-9_]*$", spec$model_name))) {
    fail("model names are lower case, digits and underscores")
  }
  check <- function(column, allowed, values = spec[[column]]) {
    bad <- setdiff(values, allowed)
    if (length(bad) > 0) {
      fail("unknown `", column, "`: ", paste(bad, collapse = ", "))
    }
  }
  parts <- function(column) strsplit(spec[[column]], "|", fixed = TRUE)
  check("tier", as.character(1:5))
  check("step", as.character(10:14))
  check("analysis_class", bp_model_classes())
  check("engine", bp_model_engines())
  check("covariate_tier", c("1", "2", "3", "family"))
  check("outcome_set", bp_model_outcome_sets())
  check("levels", c("L0", "L1", "L2", "L3"), unlist(parts("levels")))
  check("exposure_set", bp_model_exposure_sets(), unlist(parts("exposure_set")))
  # The confirmatory tiers are the prereg's; nothing exploratory sits there.
  loose <- spec$analysis_class %in%
    c("exploratory", "descriptive", "post_freeze") &
    spec$tier != "5"
  if (any(loose)) {
    fail(
      "exploratory, descriptive and post-freeze models take tier 5: ",
      paste(spec$model_name[loose], collapse = ", ")
    )
  }
  spec$step <- as.integer(spec$step)
  spec$tier <- as.integer(spec$tier)
  spec$levels <- parts("levels")
  spec$exposure_set <- parts("exposure_set")
  spec$moderator <- parts("moderator")
  spec
}
