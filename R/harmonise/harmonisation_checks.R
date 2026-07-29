# Pre-flight checks over the `expression` column of `variables.csv` files.
#
# The contract tests already fail on unparseable expressions and on calls that
# cannot resolve in the harmonisation environment, but only as part of a full
# test run. `check_harmonisation_exprs()` runs the same checks on demand
# and returns them as a tibble, so a mapping can be validated while it is being
# written rather than after CI rejects it.

# Statuses whose rows are evaluated by the harmoniser. Rows with any other
# status are filled with typed NA, so their expressions never run.
bp_evaluated_status_values <- function() {
  setdiff(
    bp_harmonisation_status_values(),
    c("incompatible", "unavailable", "in_progress")
  )
}

# Format a single R expression with Air, returning the formatted string.
# Air reads from stdin and writes to stdout when given `--stdin-file-path`.
# Returns NULL if Air is unavailable or rejects the input.
format_r_expression <- function(expression, air_path = bp_air_path()) {
  if (is.na(air_path) || !nzchar(air_path)) {
    return(NULL)
  }

  formatted <- suppressWarnings(system2(
    air_path,
    c("format", "--stdin-file-path", "expression.R", "--no-color"),
    input = expression,
    stdout = TRUE,
    stderr = FALSE
  ))

  # A non-zero exit means Air could not parse the input; the parse check
  # reports that case with a better message, so stay quiet here.
  if (!is.null(attr(formatted, "status", exact = TRUE))) {
    return(NULL)
  }
  if (length(formatted) == 0) {
    return(NULL)
  }

  paste(formatted, collapse = "\n")
}

bp_air_path <- function() {
  unname(Sys.which("air"))
}

check_harmonisation_exprs <- function(
  variables_files = list_harmonisation_var_files(),
  active_only = TRUE,
  check_format = TRUE,
  air_path = bp_air_path()
) {
  variables_files <- as.character(variables_files)

  if (check_format && (is.na(air_path) || !nzchar(air_path))) {
    warning(
      "Air was not found on the PATH, so formatting is not being checked. ",
      "Install Air (https://posit-dev.github.io/air) or pass `air_path`.",
      call. = FALSE
    )
    check_format <- FALSE
  }

  eval_env <- harmonisation_eval_env(
    spec = list(
      dataset_id = "0",
      dataset_name = "check",
      status = "in_progress"
    ),
    analysis_base = tibble::tibble(),
    lookups = list(),
    tbl = tibble::tibble()
  )

  issues <- lapply(variables_files, function(path) {
    variables <- readr::read_csv(path, show_col_types = FALSE)
    ensure_required_columns(variables, harmonisation_variable_columns(), path)

    keep <- !is.na(variables$expression) & nzchar(variables$expression)
    if (active_only) {
      keep <- keep & variables$status %in% bp_evaluated_status_values()
    }
    rows <- variables[keep, , drop = FALSE]

    row_issues <- lapply(seq_len(nrow(rows)), function(i) {
      check_one_expression(
        expression = rows$expression[[i]],
        target_variable = rows$target_variable[[i]],
        status = rows$status[[i]],
        path = path,
        eval_env = eval_env,
        check_format = check_format,
        air_path = air_path
      )
    })

    dplyr::bind_rows(row_issues)
  })

  results <- dplyr::bind_rows(issues)

  if (nrow(results) == 0) {
    return(empty_expression_issues())
  }

  results
}

check_one_expression <- function(
  expression,
  target_variable,
  status,
  path,
  eval_env,
  check_format,
  air_path
) {
  issue <- function(issue, message, suggestion = NA_character_) {
    tibble::tibble(
      variables_file = as.character(path),
      target_variable = as.character(target_variable),
      status = as.character(status),
      issue = issue,
      message = message,
      suggestion = suggestion
    )
  }

  calls <- tryCatch(
    harmonisation_expression_calls(expression),
    error = identity
  )

  # A parse failure is fatal for this row: nothing else can be checked.
  if (inherits(calls, "error")) {
    return(issue("parse_error", conditionMessage(calls)))
  }

  found <- list()

  for (call_name in calls) {
    resolves <- if (grepl("::", call_name, fixed = TRUE)) {
      parts <- strsplit(call_name, ":{2,3}")[[1]]
      requireNamespace(parts[[1]], quietly = TRUE) &&
        parts[[2]] %in% getNamespaceExports(parts[[1]])
    } else {
      exists(call_name, envir = eval_env, mode = "function")
    }

    if (!resolves) {
      found[[length(found) + 1]] <- issue(
        "unknown_function",
        paste0(
          "`",
          call_name,
          "()` does not resolve in the harmonisation environment; namespace ",
          "it or add it to bp_harmonisation_functions()"
        )
      )
    }
  }

  if (check_format) {
    formatted <- format_r_expression(expression, air_path = air_path)
    if (!is.null(formatted) && !identical(formatted, expression)) {
      found[[length(found) + 1]] <- issue(
        "unformatted",
        "Expression is not formatted as Air would format it",
        suggestion = formatted
      )
    }
  }

  dplyr::bind_rows(found)
}

empty_expression_issues <- function() {
  tibble::tibble(
    variables_file = character(),
    target_variable = character(),
    status = character(),
    issue = character(),
    message = character(),
    suggestion = character()
  )
}
