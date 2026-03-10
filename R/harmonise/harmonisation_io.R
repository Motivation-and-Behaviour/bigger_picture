read_dataschema <- function(path = bp_dataschema_path()) {
  dataschema <- readr::read_csv(path, show_col_types = FALSE)

  validate_dataschema(dataschema)
}

read_harmonisation_config <- function(dataset_id, dataschema) {
  paths <- dataset_harmonisation_paths(dataset_id)

  config <- list(
    dataset_id = as.character(dataset_id),
    layout = read_harmonisation_layout(paths$layout_file),
    variables = read_harmonisation_variables(paths$variables_file),
    lookups = read_lookup_tables(paths$lookup_dir),
    paths = paths
  )

  validate_harmonisation_config(config, dataschema)
}

read_harmonisation_layout <- function(path) {
  layout <- readr::read_csv(path, show_col_types = FALSE)

  expected <- c(
    "step_order",
    "step_type",
    "output_table",
    "table_name",
    "table_name_2",
    "input_tables",
    "join_keys",
    "expression",
    "notes"
  )

  ensure_required_columns(layout, expected, "layout.csv")
  layout <- add_missing_columns(layout, expected)

  layout$step_order <- as.integer(layout$step_order)
  layout
}

read_harmonisation_variables <- function(path) {
  variables <- readr::read_csv(path, show_col_types = FALSE)

  expected <- c(
    "target_variable",
    "status",
    "source_table",
    "source_columns",
    "expression",
    "algorithm_summary",
    "lookup_table",
    "assumptions",
    "notes",
    "review_status"
  )

  ensure_required_columns(variables, expected, "variables.csv")
  add_missing_columns(variables, expected)
}

read_lookup_tables <- function(dir_path) {
  if (!fs::dir_exists(dir_path)) {
    return(list())
  }

  files <- fs::dir_ls(dir_path, recurse = TRUE, type = "file", glob = "*.csv")

  out <- lapply(files, readr::read_csv, show_col_types = FALSE)
  names(out) <- fs::path_ext_remove(fs::path_file(files))
  out
}

validate_dataschema <- function(dataschema) {
  expected <- c(
    "variable_name",
    "label",
    "description",
    "domain",
    "entity",
    "row_grain",
    "data_type",
    "unit",
    "allowed_values",
    "min_value",
    "max_value",
    "missing_rule",
    "core",
    "notes"
  )

  ensure_required_columns(dataschema, expected, "dataschema.csv")

  duplicated_vars <-
    dataschema$variable_name[duplicated(dataschema$variable_name)]
  duplicated_vars <-
    duplicated_vars[!is.na(duplicated_vars) & nzchar(duplicated_vars)]

  if (length(duplicated_vars) > 0) {
    stop(
      "Duplicate `variable_name` values in dataschema.csv: ",
      paste(unique(duplicated_vars), collapse = ", "),
      call. = FALSE
    )
  }

  empty_names <-
    is.na(dataschema$variable_name) | !nzchar(dataschema$variable_name)
  if (any(empty_names)) {
    stop("dataschema.csv contains empty `variable_name` values.", call. = FALSE)
  }

  missing_core <- setdiff(bp_schema()$required, dataschema$variable_name)
  if (length(missing_core) > 0) {
    stop(
      "dataschema.csv is missing required core variables: ",
      paste(missing_core, collapse = ", "),
      call. = FALSE
    )
  }

  dataschema
}

validate_harmonisation_config <- function(config, dataschema) {
  validate_harmonisation_layout(config$layout)
  validate_harmonisation_vars(
    config$variables, dataschema, names(config$lookups)
  )
  config
}

validate_harmonisation_layout <- function(layout) {
  if (nrow(layout) == 0) {
    stop("layout.csv must contain at least one row.", call. = FALSE)
  }

  if (any(is.na(layout$step_order))) {
    stop("layout.csv contains missing `step_order` values.", call. = FALSE)
  }

  missing_step_type <- is.na(layout$step_type) | !nzchar(layout$step_type)
  if (any(missing_step_type)) {
    stop("layout.csv contains empty `step_type` values.", call. = FALSE)
  }

  missing_output <- is.na(layout$output_table) | !nzchar(layout$output_table)
  if (any(missing_output)) {
    stop("layout.csv contains empty `output_table` values.", call. = FALSE)
  }

  unknown_steps <- setdiff(unique(layout$step_type), bp_layout_step_types())
  unknown_steps <- unknown_steps[!is.na(unknown_steps) & nzchar(unknown_steps)]

  if (length(unknown_steps) > 0) {
    stop(
      "Unknown `step_type` values in layout.csv: ",
      paste(unknown_steps, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"analysis_base" %in% layout$output_table) {
    stop(
      "layout.csv must define an `analysis_base` output table.",
      call. = FALSE
    )
  }

  invisible(layout)
}

validate_harmonisation_vars <- function(
  variables, dataschema, lookup_names
) {
  if (nrow(variables) == 0) {
    stop("variables.csv must contain at least one row.", call. = FALSE)
  }

  missing_target <- is.na(variables$target_variable) |
    !nzchar(variables$target_variable)
  if (any(missing_target)) {
    stop(
      "variables.csv contains empty `target_variable` values.",
      call. = FALSE
    )
  }

  missing_status <- is.na(variables$status) | !nzchar(variables$status)
  if (any(missing_status)) {
    stop("variables.csv contains empty `status` values.", call. = FALSE)
  }

  unknown_status <-
    setdiff(unique(variables$status), bp_harmonisation_status_values())
  unknown_status <-
    unknown_status[!is.na(unknown_status) & nzchar(unknown_status)]
  if (length(unknown_status) > 0) {
    stop(
      "Unknown `status` values in variables.csv: ",
      paste(unknown_status, collapse = ", "),
      call. = FALSE
    )
  }

  duplicated_vars <-
    variables$target_variable[duplicated(variables$target_variable)]
  duplicated_vars <-
    duplicated_vars[!is.na(duplicated_vars) & nzchar(duplicated_vars)]
  if (length(duplicated_vars) > 0) {
    stop(
      "Duplicate `target_variable` rows in variables.csv: ",
      paste(unique(duplicated_vars), collapse = ", "),
      call. = FALSE
    )
  }

  schema_vars <- dataschema$variable_name
  unknown_targets <- setdiff(
    variables$target_variable,
    schema_vars
  )
  unknown_targets <-
    unknown_targets[!is.na(unknown_targets) & nzchar(unknown_targets)]

  if (length(unknown_targets) > 0) {
    stop(
      "variables.csv contains targets not present in dataschema.csv: ",
      paste(unknown_targets, collapse = ", "),
      call. = FALSE
    )
  }

  expected_targets <- setdiff(schema_vars, bp_system_schema_variables())
  missing_targets <- setdiff(expected_targets, variables$target_variable)

  if (length(missing_targets) > 0) {
    stop(
      "variables.csv is missing schema variables: ",
      paste(missing_targets, collapse = ", "),
      call. = FALSE
    )
  }

  active_rows <- !variables$status %in% c("incompatible", "unavailable")
  missing_expr <-
    active_rows & (is.na(variables$expression) | !nzchar(variables$expression))
  if (any(missing_expr)) {
    stop(
      "variables.csv must provide an `expression` for mapped variables: ",
      paste(variables$target_variable[missing_expr], collapse = ", "),
      call. = FALSE
    )
  }

  missing_summary <- active_rows & (
    is.na(variables$algorithm_summary) | !nzchar(variables$algorithm_summary)
  )
  if (any(missing_summary)) {
    stop(
      "variables.csv must provide `algorithm_summary` for mapped variables: ",
      paste(variables$target_variable[missing_summary], collapse = ", "),
      call. = FALSE
    )
  }

  declared_lookups <- variables$lookup_table
  declared_lookups <-
    declared_lookups[!is.na(declared_lookups) & nzchar(declared_lookups)]
  missing_lookups <- setdiff(unique(declared_lookups), lookup_names)
  if (length(missing_lookups) > 0) {
    stop(
      "variables.csv references lookup tables that do not exist: ",
      paste(missing_lookups, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(variables)
}

ensure_required_columns <- function(tbl, required_cols, file_label) {
  missing_cols <- setdiff(required_cols, names(tbl))
  if (length(missing_cols) > 0) {
    stop(
      file_label,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(tbl)
}

add_missing_columns <- function(tbl, cols) {
  for (col in cols) {
    if (!col %in% names(tbl)) {
      tbl[[col]] <- NA_character_
    }
  }

  dplyr::select(tbl, dplyr::all_of(cols), dplyr::everything())
}
