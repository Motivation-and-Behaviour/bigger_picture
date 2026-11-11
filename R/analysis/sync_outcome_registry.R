#' Bring outcome_rows.csv in line with the dataschema
#'
#' Follows `sync_harmonisation_vars_file()`: every dataschema row in the
#' registry domains (`bp_outcome_registry_domains()`) gets exactly one registry
#' row, in dataschema order. New rows take `outcome_row_defaults()` and wait
#' under `unassigned`, or under `post_freeze` once any row is frozen, so a
#' schema row added after the freeze cannot enter L0 or L1 of the frozen
#' analysis. Rows no longer in the schema stop the sync unless
#' `remove_unknown_variables` is set. A missing file is created.
#'
#' @param path Path to `outcome_rows.csv`.
#' @param dataschema The dataschema, as `read_dataschema()` returns it.
#' @param remove_unknown_variables Drop registry rows the schema no longer has.
#' @param write Write the file when it changes.
#' @return A one-row tibble summarising the changes.
sync_outcome_registry <- function(
  path = bp_outcome_rows_path(),
  dataschema = read_dataschema(),
  remove_unknown_variables = FALSE,
  write = TRUE
) {
  columns <- bp_outcome_rows_columns()
  logicals <- c("synthesis", "frozen")
  if (fs::file_exists(path)) {
    rows <- read_registry_csv(path, columns, logicals)
  } else {
    rows <- outcome_row_defaults(dataschema[0, ])
  }

  duplicated_rows <- unique(rows$variable_name[duplicated(rows$variable_name)])
  if (length(duplicated_rows) > 0) {
    stop(
      "Cannot sync `",
      path,
      "`: duplicate `variable_name` rows: ",
      paste(duplicated_rows, collapse = ", "),
      call. = FALSE
    )
  }

  in_scope <- dataschema$domain %in% bp_outcome_registry_domains()
  schema_rows <- dataschema[in_scope, , drop = FALSE]
  schema_names <- as.character(schema_rows$variable_name)

  unknown <- setdiff(rows$variable_name, schema_names)
  if (length(unknown) > 0 && !remove_unknown_variables) {
    stop(
      "Cannot sync `",
      path,
      "`: rows not in the dataschema's registry domains: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  rows <- rows[!rows$variable_name %in% unknown, , drop = FALSE]

  missing <- setdiff(schema_names, rows$variable_name)
  frozen_registry <- any(rows$frozen %in% TRUE)
  if (length(missing) > 0) {
    placeholder <- if (frozen_registry) "post_freeze" else "unassigned"
    new_rows <- outcome_row_defaults(
      schema_rows[match(missing, schema_names), , drop = FALSE],
      node = placeholder
    )
    rows <- dplyr::bind_rows(rows, new_rows)
  }

  synced <- rows[match(schema_names, rows$variable_name), , drop = FALSE]
  reordered <- !identical(rows$variable_name, synced$variable_name)
  changed <- reordered ||
    length(missing) > 0 ||
    length(unknown) > 0 ||
    !fs::file_exists(path)

  if (write && changed) {
    fs::dir_create(fs::path_dir(path))
    readr::write_csv(synced, path, na = "")
  }

  tibble::tibble(
    registry_file = as.character(path),
    added_n = length(missing),
    added_variables = paste(missing, collapse = ", "),
    removed_n = length(unknown),
    removed_variables = paste(unknown, collapse = ", "),
    reordered = reordered,
    changed = changed
  )
}
