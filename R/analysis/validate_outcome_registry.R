#' Check the outcome tree and outcome rows against each other and the schema
#'
#' Stops with every problem found, so one run lists all fixes. The rules:
#'
#' - Tree: unique snake_case node ids; L0 nodes (families) have no parent; L1
#'   nodes (specific outcomes) have a parent in the tree, reach an L0 node and
#'   say whether they are in the headline scope of the initial report.
#' - Rows: exactly one row per dataschema row in the registry domains, so a
#'   new schema row cannot silently fall out of the analysis (run
#'   `sync_outcome_registry()`); vocabularies from
#'   `bp_outcome_registry_values()`.
#' - An outcome row's node is an L1 node, a placeholder, or blank (reported
#'   individually, so `synthesis` must be FALSE); other roles have no node.
#' - An assigned row (a tree node) has an informant, score type, direction,
#'   `synthesis` and `use`, and its `prereg_family` is its node's family.
#' - `unassigned` is forbidden once any row is frozen.
#'
#' @param tree Outcome tree tibble.
#' @param rows Outcome rows tibble.
#' @param dataschema The dataschema.
#' @return `list(tree, rows)`, invisibly checked.
validate_outcome_registry <- function(tree, rows, dataschema) {
  values <- bp_outcome_registry_values()
  problems <- character()
  add <- function(condition, message, ids) {
    if (any(condition)) {
      problems <<- c(
        problems,
        paste0(message, ": ", paste(unique(ids[condition]), collapse = ", "))
      )
    }
  }
  blank <- function(x) is.na(x) | !nzchar(x)

  # --- tree -----------------------------------------------------------------
  add(
    duplicated(tree$node),
    "outcome_tree.csv: duplicate nodes",
    tree$node
  )
  add(
    blank(tree$node) | !grepl("^[a-z][a-z0-9_]*$", tree$node),
    "outcome_tree.csv: nodes must be snake_case ids",
    ifelse(blank(tree$node), "<blank>", tree$node)
  )
  add(
    tree$node %in% values$node_placeholder,
    "outcome_tree.csv: placeholder names cannot be nodes",
    tree$node
  )
  add(
    !tree$level %in% values$level,
    "outcome_tree.csv: unknown level",
    tree$node
  )
  add(
    !tree$prereg_status %in% values$prereg_status,
    "outcome_tree.csv: unknown prereg_status",
    tree$node
  )
  add(blank(tree$label), "outcome_tree.csv: missing label", tree$node)
  add(
    tree$level == "L1" & is.na(tree$headline),
    "outcome_tree.csv: L1 nodes need headline TRUE or FALSE",
    tree$node
  )
  add(
    tree$level == "L0" & !blank(tree$parent),
    "outcome_tree.csv: L0 nodes take no parent",
    tree$node
  )
  add(
    tree$level == "L1" & !tree$parent %in% tree$node,
    "outcome_tree.csv: L1 parent not in the tree",
    tree$node
  )

  # The family (L0 ancestor) of every node; NA when the chain breaks or loops.
  parent_of <- stats::setNames(tree$parent, tree$node)
  level_of <- stats::setNames(tree$level, tree$node)
  family_of <- vapply(
    tree$node,
    function(node) {
      for (step in seq_len(nrow(tree) + 1)) {
        if (is.na(node) || !node %in% names(level_of)) {
          return(NA_character_)
        }
        if (identical(level_of[[node]], "L0")) {
          return(node)
        }
        node <- parent_of[[node]]
      }
      NA_character_
    },
    character(1)
  )
  add(
    is.na(family_of),
    "outcome_tree.csv: node does not reach an L0 family",
    tree$node
  )

  # --- rows -----------------------------------------------------------------
  in_scope <- dataschema$domain %in% bp_outcome_registry_domains()
  schema_names <- as.character(dataschema$variable_name[in_scope])
  add(
    duplicated(rows$variable_name),
    "outcome_rows.csv: duplicate rows",
    rows$variable_name
  )
  missing <- setdiff(schema_names, rows$variable_name)
  add(
    rep(length(missing) > 0, length(missing)),
    "outcome_rows.csv: schema rows missing (run sync_outcome_registry())",
    missing
  )
  add(
    !rows$variable_name %in% schema_names,
    "outcome_rows.csv: rows not in the dataschema's registry domains",
    rows$variable_name
  )
  schema_domain <- stats::setNames(
    as.character(dataschema$domain),
    dataschema$variable_name
  )
  add(
    rows$variable_name %in%
      schema_names &
      !rows$schema_domain %in% schema_domain[rows$variable_name],
    "outcome_rows.csv: schema_domain differs from the dataschema",
    rows$variable_name
  )

  id <- rows$variable_name
  add(!rows$role %in% values$role, "outcome_rows.csv: unknown role", id)
  check_value <- function(column, allowed) {
    add(
      !blank(rows[[column]]) & !rows[[column]] %in% allowed,
      paste0("outcome_rows.csv: unknown ", column),
      id
    )
  }
  check_value("informant", values$informant)
  check_value("score_type", values$score_type)
  check_value("direction", values$direction)
  check_value("use", values$use)
  check_value("prereg_family", tree$node[tree$level == "L0"])
  add(is.na(rows$frozen), "outcome_rows.csv: frozen must be TRUE or FALSE", id)

  is_outcome <- rows$role %in% "outcome"
  l1_nodes <- tree$node[tree$level == "L1"]
  add(
    !is_outcome & !blank(rows$node),
    "outcome_rows.csv: only outcome rows take a node",
    id
  )
  add(
    is_outcome &
      !blank(rows$node) &
      !rows$node %in% c(l1_nodes, values$node_placeholder),
    "outcome_rows.csv: node is not an L1 node or placeholder",
    id
  )
  add(
    is_outcome & blank(rows$node) & !rows$synthesis %in% FALSE,
    "outcome_rows.csv: rows with no node must have synthesis FALSE",
    id
  )
  if (any(rows$frozen %in% TRUE)) {
    add(
      rows$node %in% "unassigned",
      "outcome_rows.csv: unassigned rows are forbidden after the freeze",
      id
    )
  }

  assigned <- is_outcome & rows$node %in% l1_nodes
  for (column in c("informant", "score_type", "direction", "use")) {
    add(
      assigned & blank(rows[[column]]),
      paste0("outcome_rows.csv: assigned rows need ", column),
      id
    )
  }
  add(
    assigned & is.na(rows$synthesis),
    "outcome_rows.csv: assigned rows need synthesis",
    id
  )
  node_family <- unname(family_of[rows$node])
  add(
    assigned & !blank(rows$prereg_family) & rows$prereg_family != node_family,
    "outcome_rows.csv: prereg_family differs from the node's family",
    id
  )

  part_lists <- strsplit(
    ifelse(blank(rows$part_of), "", rows$part_of),
    "|",
    fixed = TRUE
  )
  bad_part <- vapply(
    seq_along(part_lists),
    function(i) {
      parts <- part_lists[[i]][nzchar(part_lists[[i]])]
      any(!parts %in% rows$variable_name | parts == id[i])
    },
    logical(1)
  )
  add(
    bad_part,
    "outcome_rows.csv: part_of names a row not in the registry, or itself",
    id
  )

  measure_rows <- grep(
    "_measure$",
    as.character(dataschema$variable_name),
    value = TRUE
  )
  key_lists <- strsplit(
    ifelse(blank(rows$standardise_by), "", rows$standardise_by),
    "|",
    fixed = TRUE
  )
  bad_key <- vapply(
    key_lists,
    function(keys) {
      keys <- keys[nzchar(keys)]
      any(!keys %in% c(values$standardise_key, measure_rows))
    },
    logical(1)
  )
  add(
    bad_key,
    "outcome_rows.csv: unknown standardise_by key",
    id
  )

  if (length(problems) > 0) {
    stop(
      "The outcome registry has problems:\n- ",
      paste(problems, collapse = "\n- "),
      call. = FALSE
    )
  }

  invisible(list(tree = tree, rows = rows))
}
