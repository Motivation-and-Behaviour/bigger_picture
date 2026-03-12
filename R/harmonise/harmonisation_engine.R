harmonise_from_tables <- function(analysis_base, spec, dataschema,
                                  harmonisation_config) {
  analysis_base <- tibble::as_tibble(analysis_base)
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
      lookups = harmonisation_config$lookups
    )

    outputs[[variable_name]] <- values
  }

  tibble::as_tibble(outputs)
}

get_mapping_row <- function(variables, variable_name) {
  row <- variables[variables$target_variable == variable_name, , drop = FALSE]

  if (nrow(row) == 0) {
    return(NULL)
  }

  row
}

derive_schema_variable <- function(schema_row, mapping_row, analysis_base, spec,
                                   lookups) {
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
    analysis_base = analysis_base,
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

harmonisation_eval_env <- function(spec, analysis_base, lookups, tbl) {
  lookup_bindings <- lookups
  if (length(lookup_bindings) > 0) {
    names(lookup_bindings) <- paste0("lookup_", names(lookups))
  }

  list2env(
    c(
      list(
        spec = spec,
        analysis_base = analysis_base,
        lookup_tables = lookups,
        tbl = tbl
      ),
      lookup_bindings
    ),
    parent = globalenv()
  )
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
