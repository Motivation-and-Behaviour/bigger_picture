harmonise_from_tables <- function(
  analysis_base,
  spec,
  dataschema,
  harmonisation_config
) {
  analysis_base <- tibble::as_tibble(analysis_base)
  variables <- harmonisation_config$variables
  measure <- harmonisation_measure_of(variables)

  # The primary measure: every schema variable, from the untagged rows.
  primary <- harmonise_measure_block(
    analysis_base = analysis_base,
    spec = spec,
    dataschema = dataschema,
    mapping_rows = variables[is.na(measure), , drop = FALSE],
    lookups = harmonisation_config$lookups,
    measure_id = "primary"
  )

  measures <- unique(measure[!is.na(measure)])
  if (length(measures) == 0) {
    return(primary)
  }

  # Each additional measure is a block of rows for the same participants:
  # the screen-time variables come from that measure's rows (or are NA when
  # the measure does not map them), everything else is copied from the
  # primary block, and rows carrying no screen-time quantity are dropped.
  scoped <- bp_measure_scoped_variables(dataschema)
  quantities <- setdiff(scoped, bp_measure_metadata_variables())
  blocks <- lapply(measures, function(tag) {
    rows <- variables[!is.na(measure) & measure == tag, , drop = FALSE]
    block <- primary
    n_rows <- nrow(block)

    for (variable_name in scoped) {
      schema_row <- dataschema[
        dataschema$variable_name == variable_name,
        ,
        drop = FALSE
      ]
      mapping_row <- get_mapping_row(rows, variable_name)
      block[[variable_name]] <- if (is.null(mapping_row)) {
        typed_na_vector(schema_row$data_type[[1]], n_rows)
      } else {
        derive_schema_variable(
          schema_row = schema_row,
          mapping_row = mapping_row,
          analysis_base = analysis_base,
          spec = spec,
          lookups = harmonisation_config$lookups,
          measure_id = tag
        )
      }
    }
    if ("st_measure_id" %in% names(block)) {
      block$st_measure_id <- rep(tag, n_rows)
    }

    present <- intersect(quantities, names(block))
    observed <- Reduce(
      `|`,
      lapply(present, function(nm) !is.na(block[[nm]])),
      rep(FALSE, n_rows)
    )
    block[observed, , drop = FALSE]
  })

  dplyr::bind_rows(c(list(primary), blocks))
}

# Derive every dataschema variable from one set of mapping rows.
harmonise_measure_block <- function(
  analysis_base,
  spec,
  dataschema,
  mapping_rows,
  lookups,
  measure_id
) {
  outputs <- vector("list", nrow(dataschema))
  names(outputs) <- dataschema$variable_name

  for (i in seq_len(nrow(dataschema))) {
    schema_row <- dataschema[i, , drop = FALSE]
    variable_name <- schema_row$variable_name[[1]]
    mapping_row <- get_mapping_row(mapping_rows, variable_name)

    outputs[[variable_name]] <- derive_schema_variable(
      schema_row = schema_row,
      mapping_row = mapping_row,
      analysis_base = analysis_base,
      spec = spec,
      lookups = lookups,
      measure_id = measure_id
    )
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

derive_schema_variable <- function(
  schema_row,
  mapping_row,
  analysis_base,
  spec,
  lookups,
  measure_id = "primary"
) {
  variable_name <- schema_row$variable_name[[1]]
  data_type <- schema_row$data_type[[1]]
  n_rows <- nrow(analysis_base)

  if (identical(variable_name, "st_measure_id")) {
    return(cast_to_schema_type(
      rep(as.character(measure_id), n_rows),
      data_type
    ))
  }

  if (identical(variable_name, "dataset_id")) {
    return(
      cast_to_schema_type(rep(as.character(spec$dataset_id), n_rows), data_type)
    )
  }

  if (identical(variable_name, "dataset_name")) {
    return(
      cast_to_schema_type(
        rep(as.character(spec$dataset_name), n_rows),
        data_type
      )
    )
  }

  if (is.null(mapping_row)) {
    stop("No mapping row found for `", variable_name, "`.", call. = FALSE)
  }

  status <- mapping_row$status[[1]]
  if (status %in% c("incompatible", "unavailable", "in_progress")) {
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
    recycle_to_n_rows(raw_value, n_rows, variable_name),
    data_type
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

  # Sandbox: dataset bindings -> allowlisted functions -> base R. Expressions
  # cannot reach the global environment or attached packages; see
  # `bp_harmonisation_functions()` for the bare-name vocabulary.
  functions_env <- rlang::new_environment(
    bp_harmonisation_functions(),
    parent = baseenv()
  )

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
    parent = functions_env
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
    "Expression for `",
    variable_name,
    "` returned length ",
    length(value),
    " but expected 1 or ",
    n_rows,
    ".",
    call. = FALSE
  )
}

typed_na_vector <- function(data_type, n_rows) {
  switch(
    data_type,
    character = rep(NA_character_, n_rows),
    double = rep(NA_real_, n_rows),
    integer = rep(NA_integer_, n_rows),
    logical = rep(NA, n_rows),
    date = rep(as.Date(NA), n_rows),
    rep(NA_character_, n_rows)
  )
}

cast_to_schema_type <- function(value, data_type) {
  switch(
    data_type,
    character = as.character(value),
    double = as.numeric(value),
    integer = as.integer(value),
    logical = as.logical(value),
    date = as.Date(value),
    value
  )
}
