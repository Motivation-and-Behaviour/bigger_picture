#' Name the model each pooled difference between groups belongs to
#'
#' A registered test of moderation covers some families of an exposure and
#' its `_other` partner the rest (`expand_model_cells()`), so a pooled row
#' is placed by its exposure and family, which `label_models()` cannot do
#' for rows without an outcome.
#'
#' @param tree Rows of `pool_tree()` on a table of differences between two
#'   groups' slopes.
#' @param model_cells From `expand_model_cells()`.
#' @param outcome_registry The outcome registry.
#' @param model The registered model, `moderation_sex` or `moderation_ses`;
#'   rows outside its cells go to `<model>_other`.
#' @param moderator The moderator's name, kept on each row.
#' @return `tree` with `model_name` and `moderator`, less any row in
#'   neither model's cells.
label_moderation <- function(
  tree,
  model_cells,
  outcome_registry,
  model,
  moderator
) {
  if (is.null(tree) || nrow(tree) == 0) {
    return(tibble::tibble())
  }
  rows <- outcome_registry$rows
  pairs <- function(name) {
    cells <- model_cells[model_cells$model_name %in% name, ]
    family <- rows$prereg_family[match(cells$outcome, rows$variable_name)]
    unique(paste(cells$exposure, family))
  }
  other <- paste0(model, "_other")
  registered <- pairs(model)
  rest <- setdiff(pairs(other), registered)
  key <- paste(tree$exposure, tree$family)
  tree$model_name <- dplyr::case_when(
    key %in% registered ~ model,
    key %in% rest ~ other,
    .default = NA_character_
  )
  tree$moderator <- rep(moderator, nrow(tree))
  dplyr::relocate(tree[!is.na(tree$model_name), ], "model_name")
}
