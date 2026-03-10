harmonise_from_tables <- function(tidied_dataset, spec, dataschema,
                                  harmonisation_config) {
  tables <- build_layout_tables(
    tidied_dataset = tidied_dataset,
    layout = harmonisation_config$layout,
    spec = spec,
    lookups = harmonisation_config$lookups
  )

  analysis_base <- get_named_table(tables, "analysis_base")
  outputs <- vector("list", nrow(dataschema))

  names(outputs) <- dataschema$variable_name

  for (i in seq_len(nrow(dataschema))) {
    schema_row <- dataschema[i, , drop = FALSE]
    variable_name <- schema_row$variable_name[[1]]
    mapping_row <- get_mapping_row(
      harmonisation_config$variables, variable_name
    )

    values <- derive_schema_variable(
      schema_row = schema_row,
      mapping_row = mapping_row,
      analysis_base = analysis_base,
      spec = spec,
      tidied_dataset = tidied_dataset,
      tables = tables,
      lookups = harmonisation_config$lookups
    )

    outputs[[variable_name]] <- values
  }

  tibble::as_tibble(outputs)
}

build_layout_tables <- function(tidied_dataset, layout, spec, lookups) {
  tables <- tidied_dataset$data
  layout <- dplyr::arrange(layout, step_order)

  for (i in seq_len(nrow(layout))) {
    step <- layout[i, , drop = FALSE]
    tables[[step$output_table[[1]]]] <- run_layout_step(
      tables = tables,
      step = step,
      tidied_dataset = tidied_dataset,
      spec = spec,
      lookups = lookups
    )
  }

  tables
}

run_layout_step <- function(tables, step, tidied_dataset, spec, lookups) {
  step_type <- step$step_type[[1]]

  if (identical(step_type, "use_table")) {
    return(get_named_table(tables, step$table_name[[1]]))
  }

  if (identical(step_type, "transform")) {
    tbl <- get_named_table(tables, step$table_name[[1]])
    env <- harmonisation_eval_env(
      spec = spec,
      tidied_dataset = tidied_dataset,
      tables = tables,
      lookups = lookups,
      tbl = tbl
    )

    return(tibble::as_tibble(eval_harmonisation_expression(
      step$expression[[1]],
      data = tbl,
      env = env
    )))
  }

  if (step_type %in% c("left_join", "inner_join", "full_join")) {
    lhs <- get_named_table(tables, step$table_name[[1]])
    rhs <- get_named_table(tables, step$table_name_2[[1]])
    by <- parse_delimited_field(step$join_keys[[1]])

    join_fn <- switch(step_type,
      left_join = dplyr::left_join,
      inner_join = dplyr::inner_join,
      full_join = dplyr::full_join
    )

    return(tibble::as_tibble(join_fn(lhs, rhs, by = by)))
  }

  if (identical(step_type, "bind_rows")) {
    table_names <- parse_delimited_field(step$input_tables[[1]])
    table_list <- lapply(table_names, get_named_table, tables = tables)
    return(tibble::as_tibble(dplyr::bind_rows(table_list)))
  }

  stop("Unsupported layout step type: ", step_type, call. = FALSE)
}

get_mapping_row <- function(variables, variable_name) {
  row <- variables[variables$target_variable == variable_name, , drop = FALSE]

  if (nrow(row) == 0) {
    return(NULL)
  }

  row
}

derive_schema_variable <- function(schema_row, mapping_row, analysis_base, spec,
                                   tidied_dataset, tables, lookups) {
  variable_name <- schema_row$variable_name[[1]]
  data_type <- schema_row$data_type[[1]]
  n_rows <- nrow(analysis_base)

  if (identical(variable_name, "dataset_id")) {
    return(
      cast_to_schema_type(rep(as.character(spec$dataset_id), n_rows), data_type)
    )
  }

  if (identical(variable_name, "dataset_name")) {
    return(
      cast_to_schema_type(
        rep(as.character(spec$dataset_name), n_rows), data_type
      )
    )
  }

  if (is.null(mapping_row)) {
    stop("No mapping row found for `", variable_name, "`.", call. = FALSE)
  }

  status <- mapping_row$status[[1]]
  if (status %in% c("incompatible", "unavailable")) {
    return(typed_na_vector(data_type, n_rows))
  }

  env <- harmonisation_eval_env(
    spec = spec,
    tidied_dataset = tidied_dataset,
    tables = tables,
    lookups = lookups,
    tbl = analysis_base
  )

  raw_value <- eval_harmonisation_expression(
    mapping_row$expression[[1]],
    data = analysis_base,
    env = env
  )

  cast_to_schema_type(
    recycle_to_n_rows(raw_value, n_rows, variable_name), data_type
  )
}

eval_harmonisation_expression <- function(expression, data, env) {
  expr <- rlang::parse_expr(expression)
  value <- rlang::eval_tidy(expr, data = data, env = env)

  if (inherits(value, "data.frame")) {
    if (ncol(value) != 1) {
      stop(
        "Expressions must return a vector or a one-column data frame.",
        call. = FALSE
      )
    }

    value <- value[[1]]
  }

  value
}

harmonisation_eval_env <- function(spec, tidied_dataset, tables, lookups, tbl) {
  lookup_bindings <- lookups
  if (length(lookup_bindings) > 0) {
    names(lookup_bindings) <- paste0("lookup_", names(lookups))
  }

  list2env(
    c(
      list(
        spec = spec,
        tidied_dataset = tidied_dataset,
        tables = tables,
        lookup_tables = lookups,
        tbl = tbl
      ),
      lookup_bindings
    ),
    parent = globalenv()
  )
}

get_named_table <- function(tables, table_name) {
  if (is.na(table_name) || !nzchar(table_name)) {
    stop("A table name is required for this layout step.", call. = FALSE)
  }

  if (!table_name %in% names(tables)) {
    stop(
      "Unknown table referenced in harmonisation config: ",
      table_name,
      call. = FALSE
    )
  }

  tibble::as_tibble(tables[[table_name]])
}

parse_delimited_field <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return(character())
  }

  values <- strsplit(x, ";", fixed = TRUE)[[1]]
  trimws(values[nzchar(trimws(values))])
}

recycle_to_n_rows <- function(value, n_rows, variable_name) {
  if (length(value) == n_rows) {
    return(value)
  }

  if (length(value) == 1L) {
    return(rep(value, n_rows))
  }

  stop(
    "Expression for `", variable_name, "` returned length ",
    length(value),
    " but expected 1 or ",
    n_rows,
    ".",
    call. = FALSE
  )
}

typed_na_vector <- function(data_type, n_rows) {
  switch(data_type,
    character = rep(NA_character_, n_rows),
    double = rep(NA_real_, n_rows),
    integer = rep(NA_integer_, n_rows),
    logical = rep(NA, n_rows),
    date = rep(as.Date(NA), n_rows),
    rep(NA_character_, n_rows)
  )
}

cast_to_schema_type <- function(value, data_type) {
  switch(data_type,
    character = as.character(value),
    double = as.numeric(value),
    integer = as.integer(value),
    logical = as.logical(value),
    date = as.Date(value),
    value
  )
}
