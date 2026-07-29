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

# Expressions live in CSV cells rather than in `.R` files, so they are
# formatted to their own settings: a wider line than the repository's `.R`
# files use, and no preserved line breaks, because a break inside a cramped
# CSV cell reflects hand-editing rather than intent.
bp_air_expression_width <- function() {
  120L
}

# Air takes its settings from an `air.toml` found by searching upward from the
# path given to `--stdin-file-path`. Writing one into a temporary directory
# applies the settings above without touching the repository's own `air.toml`,
# which governs the `.R` files and must stay at its own width.
air_expression_config_dir <- function(
  line_width = bp_air_expression_width(),
  persistent_line_breaks = FALSE,
  dir = tempfile("air-expression-")
) {
  fs::dir_create(dir)
  writeLines(
    c(
      "[format]",
      paste0("line-width = ", as.integer(line_width)),
      paste0(
        "persistent-line-breaks = ",
        if (isTRUE(persistent_line_breaks)) "true" else "false"
      )
    ),
    fs::path(dir, "air.toml")
  )
  dir
}

# Format a single R expression with Air, returning the formatted string.
# Air reads from stdin and writes to stdout when given `--stdin-file-path`.
# Returns NULL if Air is unavailable or rejects the input.
format_r_expression <- function(
  expression,
  air_path = bp_air_path(),
  config_dir = air_expression_config_dir()
) {
  if (is.na(air_path) || !nzchar(air_path)) {
    return(NULL)
  }

  formatted <- suppressWarnings(system2(
    air_path,
    c(
      "format",
      "--stdin-file-path",
      fs::path(config_dir, "expression.R"),
      "--no-color"
    ),
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

# Locate the Air binary. `Sys.which()` covers a normal install, but in this
# devcontainer Air is bundled inside the VS Code extension, and that directory
# is only added to the PATH of VS Code's *integrated terminal*. R sessions
# started by the R extension (or by RStudio) inherit the extension host's
# environment instead, where `air` is not on the PATH at all. Fall back to the
# bundled binary so the check works from any R session.
bp_air_path <- function() {
  found <- unname(Sys.which("air"))
  if (nzchar(found)) {
    return(found)
  }

  candidates <- Sys.glob(file.path(
    path.expand("~"),
    c(".vscode-server", ".vscode"),
    "extensions",
    "posit.air-vscode-*",
    "bundled",
    "bin",
    "air"
  ))
  candidates <- candidates[file.access(candidates, mode = 1) == 0]

  if (length(candidates) == 0) {
    return("")
  }

  # Several extension versions can coexist; the newest install wins.
  candidates[[which.max(file.mtime(candidates))]]
}

check_harmonisation_exprs <- function(
  variables_files = list_harmonisation_var_files(),
  active_only = TRUE,
  check_format = TRUE,
  fix = FALSE,
  line_width = bp_air_expression_width(),
  persistent_line_breaks = FALSE,
  air_path = bp_air_path()
) {
  variables_files <- as.character(variables_files)

  if (check_format && (is.na(air_path) || !nzchar(air_path))) {
    warning(
      "Air was not found, so formatting is not being checked. Install Air ",
      "(https://posit-dev.github.io/air), pass `air_path`, or add its ",
      "directory to PATH in ~/.Renviron so R sessions inherit it.",
      call. = FALSE
    )
    check_format <- FALSE
  }

  if (fix && !check_format) {
    stop(
      "`fix = TRUE` needs `check_format = TRUE` and a working Air install; ",
      "there is nothing else this function knows how to rewrite.",
      call. = FALSE
    )
  }

  config_dir <- tempfile("air-expression-")
  on.exit(unlink(config_dir, recursive = TRUE), add = TRUE)
  if (check_format) {
    air_expression_config_dir(
      line_width = line_width,
      persistent_line_breaks = persistent_line_breaks,
      dir = config_dir
    )
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

    evaluated <- variables$status %in% bp_evaluated_status_values()
    has_expression <- !is.na(variables$expression) &
      nzchar(variables$expression)

    # An evaluated status promises a mapping, so a blank expression is an
    # error in its own right — and the row would otherwise be skipped here
    # while still failing when the config is read.
    blank <- variables[evaluated & !has_expression, , drop = FALSE]
    blank_issues <- tibble::tibble(
      variables_file = as.character(path),
      target_variable = as.character(blank$target_variable),
      status = as.character(blank$status),
      issue = "missing_expression",
      message = paste0(
        "status `",
        blank$status,
        "` is evaluated, so this row needs an `expression` (or a status ",
        "such as `in_progress` while it is still being written)"
      ),
      suggestion = NA_character_
    )

    keep <- has_expression
    if (active_only) {
      keep <- keep & evaluated
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
        air_path = air_path,
        config_dir = config_dir
      )
    })

    dplyr::bind_rows(blank_issues, row_issues)
  })

  results <- dplyr::bind_rows(issues)

  if (nrow(results) == 0) {
    return(empty_expression_issues())
  }

  results$fixed <- FALSE
  if (fix) {
    results <- apply_expression_fixes(results)
  }

  results
}

# Write `suggestion` back into the `expression` column for every `unformatted`
# row. Nothing else is rewritten: the other issues need a human decision.
apply_expression_fixes <- function(results) {
  fixable <- results$issue == "unformatted" & !is.na(results$suggestion)

  for (path in unique(results$variables_file[fixable])) {
    rows <- fixable & results$variables_file == path

    # Read every column as text so that rewriting the file cannot change
    # cells this function was not asked to touch.
    variables <- readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character())
    )

    matched <- match(results$target_variable[rows], variables$target_variable)
    variables$expression[matched] <- results$suggestion[rows]

    readr::write_csv(variables, path, na = "")
    results$fixed[rows] <- TRUE
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
  air_path,
  config_dir
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
    formatted <- format_r_expression(
      expression,
      air_path = air_path,
      config_dir = config_dir
    )
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
    suggestion = character(),
    fixed = logical()
  )
}
