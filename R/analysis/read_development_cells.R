#' Read the development cells, expanded to exposure and outcome rows
#'
#' The engine is built on real data, so a short list of exposure and outcome
#' cells is fitted before the freeze (ANALYSIS_PLAN.md, Step 5). These are the
#' only associations seen before the specification is frozen, and every
#' results table marks them. Each line of `development_cells.csv` names an
#' exposure row and either an outcome family, meaning every pooled outcome
#' row of that family, or one outcome row.
#'
#' @param path Path to `development_cells.csv`.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @return A tibble of `exposure` and `outcome` (dataschema rows), one row per
#'   cell.
read_development_cells <- function(
  path = bp_development_cells_path(),
  outcome_registry,
  exposure_registry
) {
  cells <- read_registry_csv(path, bp_development_cells_columns())
  rows <- outcome_registry$rows
  fail <- function(...) stop(basename(path), ": ", ..., call. = FALSE)

  unknown <- setdiff(cells$exposure, exposure_registry$variable_name)
  if (length(unknown) > 0) {
    fail("not exposure rows: ", paste(unknown, collapse = ", "))
  }
  both <- !is.na(cells$family) & !is.na(cells$variable_name)
  neither <- is.na(cells$family) & is.na(cells$variable_name)
  if (any(both | neither)) {
    fail("each line names a family or a variable_name, not both")
  }
  if (anyNA(cells$reason)) {
    fail("every line needs a reason")
  }
  bad_family <- setdiff(stats::na.omit(cells$family), rows$prereg_family)
  if (length(bad_family) > 0) {
    fail("not outcome families: ", paste(bad_family, collapse = ", "))
  }
  bad_row <- setdiff(stats::na.omit(cells$variable_name), rows$variable_name)
  if (length(bad_row) > 0) {
    fail("not outcome rows: ", paste(bad_row, collapse = ", "))
  }

  pooled <- rows[
    rows$role %in% "outcome" & rows$use %in% "core" & rows$synthesis %in% TRUE,
  ]
  expand_one <- function(i) {
    outcomes <- if (is.na(cells$family[i])) {
      cells$variable_name[i]
    } else {
      pooled$variable_name[pooled$prereg_family %in% cells$family[i]]
    }
    tibble::tibble(exposure = cells$exposure[i], outcome = outcomes)
  }
  dplyr::distinct(dplyr::bind_rows(lapply(seq_len(nrow(cells)), expand_one)))
}
