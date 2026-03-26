`%||%` <- function(x, y) if (is.null(x)) y else x

harmonisation_variable_columns <- function() {
  c(
    "target_variable",
    "status",
    "source_columns",
    "expression",
    "notes",
    "lookup_table"
  )
}

list_harmonisation_variables_files <- function(
  dataset_specs_dir = bp_dataset_specs_dir()
) {
  fs::dir_ls(
    dataset_specs_dir,
    recurse = TRUE,
    type = "file",
    regexp = "variables\\.csv$"
  ) |>
    sort()
}

sync_harmonisation_variables_file <- function(
  path,
  dataschema = read_dataschema(),
  default_status = "in_progress",
  remove_unknown_variables = FALSE,
  write = TRUE
) {
  if (!default_status %in% bp_harmonisation_status_values()) {
    stop(
      "`default_status` must be one of: ",
      paste(bp_harmonisation_status_values(), collapse = ", "),
      call. = FALSE
    )
  }

  variables <- readr::read_csv(path, show_col_types = FALSE)
  expected_cols <- harmonisation_variable_columns()

  ensure_required_columns(variables, expected_cols, path)
  variables <- add_missing_columns(variables, expected_cols)

  missing_target <- is.na(variables$target_variable) |
    !nzchar(variables$target_variable)
  if (any(missing_target)) {
    stop(
      "Cannot sync `",
      path,
      "` because it contains empty `target_variable` values.",
      call. = FALSE
    )
  }

  duplicated_targets <- unique(
    variables$target_variable[duplicated(variables$target_variable)]
  )
  duplicated_targets <- duplicated_targets[
    !is.na(duplicated_targets) & nzchar(duplicated_targets)
  ]
  if (length(duplicated_targets) > 0) {
    stop(
      "Cannot sync `",
      path,
      "` because it contains duplicate `target_variable` rows: ",
      paste(duplicated_targets, collapse = ", "),
      call. = FALSE
    )
  }

  schema_targets <- setdiff(
    as.character(dataschema$variable_name),
    bp_system_schema_variables()
  )

  unknown_targets <- setdiff(variables$target_variable, schema_targets)
  unknown_targets <- unknown_targets[
    !is.na(unknown_targets) & nzchar(unknown_targets)
  ]
  if (length(unknown_targets) > 0 && !remove_unknown_variables) {
    stop(
      "Cannot sync `",
      path,
      "` because it contains targets not present in dataschema.csv: ",
      paste(unknown_targets, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(unknown_targets) > 0 && remove_unknown_variables) {
    variables <- variables[
      !variables$target_variable %in% unknown_targets,
      ,
      drop = FALSE
    ]
  }

  missing_targets <- setdiff(schema_targets, variables$target_variable)

  if (length(missing_targets) > 0) {
    missing_rows <- as.data.frame(
      stats::setNames(
        replicate(length(names(variables)), NA_character_, simplify = FALSE),
        names(variables)
      ),
      stringsAsFactors = FALSE
    )[rep(1, length(missing_targets)), , drop = FALSE]

    missing_rows$target_variable <- missing_targets
    missing_rows$status <- default_status

    variables <- dplyr::bind_rows(variables, tibble::as_tibble(missing_rows))
  }

  synced <- variables[match(schema_targets, variables$target_variable), ,
    drop = FALSE
  ]

  was_reordered <- !identical(
    as.character(variables$target_variable),
    as.character(synced$target_variable)
  )
  changed <- was_reordered ||
    length(missing_targets) > 0 ||
    (remove_unknown_variables && length(unknown_targets) > 0)

  if (write && changed) {
    readr::write_csv(synced, path, na = "")
  }

  tibble::tibble(
    variables_file = path,
    added_n = length(missing_targets),
    added_variables = paste(missing_targets, collapse = ", "),
    removed_n = if (remove_unknown_variables) length(unknown_targets) else 0L,
    removed_variables = if (remove_unknown_variables) {
      paste(unknown_targets, collapse = ", ")
    } else {
      ""
    },
    reordered = was_reordered,
    changed = changed
  )
}

sync_harmonisation_variables <- function(
  variables_files = list_harmonisation_variables_files(),
  dataschema = read_dataschema(),
  default_status = "in_progress",
  remove_unknown_variables = FALSE,
  write = TRUE
) {
  variables_files <- as.character(variables_files)

  if (length(variables_files) == 0) {
    return(tibble::tibble(
      variables_file = character(),
      added_n = integer(),
      added_variables = character(),
      removed_n = integer(),
      removed_variables = character(),
      reordered = logical(),
      changed = logical()
    ))
  }

  results <- lapply(
    variables_files,
    sync_harmonisation_variables_file,
    dataschema = dataschema,
    default_status = default_status,
    remove_unknown_variables = remove_unknown_variables,
    write = write
  )

  dplyr::bind_rows(results)
}

sum_nonmissing <- function(...) {
  values <- list(...)

  if (length(values) == 0) {
    stop("`sum_nonmissing()` requires at least one input.", call. = FALSE)
  }

  if (
    length(values) == 1L &&
      (is.data.frame(values[[1]]) || is.matrix(values[[1]]))
  ) {
    values <- values[[1]]
  } else {
    values <- as.data.frame(
      values,
      optional = TRUE,
      stringsAsFactors = FALSE
    )
  }

  observed_count <- rowSums(!is.na(values))
  totals <- rowSums(values, na.rm = TRUE)
  totals[observed_count == 0] <- NA_real_
  totals
}

lookup_values <- function(
  values,
  lookup,
  dataset_id = NULL,
  target_variable = NULL,
  source_col = "source_value",
  value_col = "harmonised_value",
  default = NA
) {
  if (!inherits(lookup, "data.frame")) {
    stop("`lookup` must be a data frame.", call. = FALSE)
  }

  required_cols <- c(source_col, value_col)
  missing_cols <- setdiff(required_cols, names(lookup))
  if (length(missing_cols) > 0) {
    stop(
      "Lookup table is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(dataset_id) && "dataset_id" %in% names(lookup)) {
    lookup <- lookup[
      as.character(lookup$dataset_id) == as.character(dataset_id),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(target_variable) && "target_variable" %in% names(lookup)) {
    lookup <- lookup[
      as.character(lookup$target_variable) == as.character(target_variable),
      ,
      drop = FALSE
    ]
  }

  if (nrow(lookup) == 0) {
    stop(
      "Lookup table contains no rows after applying dataset/variable filters.",
      call. = FALSE
    )
  }

  lookup_keys <- as.character(lookup[[source_col]])
  duplicated_keys <- unique(lookup_keys[
    duplicated(lookup_keys) & !is.na(lookup_keys)
  ])
  if (length(duplicated_keys) > 0) {
    stop(
      "Lookup table contains duplicate source values after filtering: ",
      paste(duplicated_keys, collapse = ", "),
      call. = FALSE
    )
  }

  matched <- match(as.character(values), lookup_keys)
  out <- lookup[[value_col]][matched]

  unmatched <- is.na(matched) & !is.na(values)
  if (any(unmatched)) {
    out[unmatched] <- default
  }

  out
}
