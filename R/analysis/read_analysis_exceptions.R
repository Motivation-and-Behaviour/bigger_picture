#' Read one dataset's analysis exceptions, if it has any
#'
#' An optional `harmonisation/datasets/BPIPD-<id>/analysis_exceptions.csv`
#' holds decisions about that dataset which a row-level registry cannot
#' express, each with a reason (ANALYSIS_PLAN.md, Step 4). Columns:
#'
#' - `variable_name`: the dataschema row the decision is about;
#' - `wave`: a wave label, or blank for every wave;
#' - `action`: `exclude` (leave the row out), `keep_out_of_bounds` (do not
#'   drop values outside the schema's bounds, for a documented score form),
#'   `node` (assign to another node of the outcome tree) or `standardise_by`
#'   (replace the row's standardisation keys);
#' - `value`: the node or the pipe-separated keys, blank otherwise;
#' - `reason`: why, in a sentence.
#'
#' @param path Path to the file; a zero-length or missing path means the
#'   dataset has none.
#' @param dataschema The dataschema.
#' @param outcome_registry The outcome registry, for node names.
#' @return A tibble with the columns above, with no rows when there is no
#'   file.
read_analysis_exceptions <- function(path, dataschema, outcome_registry) {
  columns <- bp_analysis_exception_columns()
  empty <- tibble::as_tibble(
    stats::setNames(rep(list(character()), length(columns)), columns)
  )
  if (length(path) == 0 || !file.exists(path)) {
    return(empty)
  }
  exceptions <- read_registry_csv(path, columns)
  label <- path
  fail <- function(...) stop(label, ": ", ..., call. = FALSE)

  unknown <- setdiff(exceptions$variable_name, dataschema$variable_name)
  if (length(unknown) > 0) {
    fail("not dataschema rows: ", paste(unknown, collapse = ", "))
  }
  bad_action <- setdiff(exceptions$action, bp_analysis_exception_actions())
  if (length(bad_action) > 0) {
    fail("unknown action: ", paste(bad_action, collapse = ", "))
  }
  if (anyNA(exceptions$reason)) {
    fail("every exception needs a reason")
  }
  needs_value <- exceptions$action %in% c("node", "standardise_by")
  if (any(needs_value & is.na(exceptions$value))) {
    fail("`node` and `standardise_by` need a value")
  }
  nodes <- exceptions$value[exceptions$action %in% "node"]
  bad_node <- setdiff(nodes, outcome_registry$tree$node)
  if (length(bad_node) > 0) {
    fail("not nodes of the outcome tree: ", paste(bad_node, collapse = ", "))
  }
  exceptions
}

bp_analysis_exception_columns <- function() {
  c("variable_name", "wave", "action", "value", "reason")
}

bp_analysis_exception_actions <- function() {
  c("exclude", "keep_out_of_bounds", "node", "standardise_by")
}
