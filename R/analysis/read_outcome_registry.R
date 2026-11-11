#' Read and check the outcome registry
#'
#' @param tree_path Path to `outcome_tree.csv`.
#' @param rows_path Path to `outcome_rows.csv`.
#' @param dataschema The dataschema, as `read_dataschema()` returns it.
#' @return `list(tree, rows)`, checked by `validate_outcome_registry()`.
read_outcome_registry <- function(
  tree_path = bp_outcome_tree_path(),
  rows_path = bp_outcome_rows_path(),
  dataschema = read_dataschema()
) {
  tree <- read_registry_csv(tree_path, bp_outcome_tree_columns(), "headline")
  rows <- read_registry_csv(
    rows_path,
    bp_outcome_rows_columns(),
    c("synthesis", "frozen")
  )
  validate_outcome_registry(tree, rows, dataschema)
  list(tree = tree, rows = rows)
}
