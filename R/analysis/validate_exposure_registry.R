#' Check exposures.csv against the dataschema
#'
#' Stops with every problem found. Every screen-time schema row except the
#' system `st_measure_id` has exactly one row; vocabularies come from
#' `bp_exposure_registry_values()`; a row's unit matches the schema (hours, or
#' an index for rows with no unit), and metadata rows (instrument descriptions)
#' carry no unit or day and are never included.
#'
#' @param exposures Exposures tibble.
#' @param dataschema The dataschema.
#' @return `exposures`, invisibly checked.
validate_exposure_registry <- function(exposures, dataschema) {
  values <- bp_exposure_registry_values()
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
  id <- exposures$variable_name

  in_scope <- dataschema$domain %in%
    "screen_time" &
    !dataschema$variable_name %in% bp_system_schema_variables()
  schema_names <- as.character(dataschema$variable_name[in_scope])
  missing <- setdiff(schema_names, id)
  add(
    rep(length(missing) > 0, length(missing)),
    "exposures.csv: screen-time schema rows missing",
    missing
  )
  add(
    !id %in% schema_names,
    "exposures.csv: rows not in the dataschema's screen-time domain",
    id
  )
  add(duplicated(id), "exposures.csv: duplicate rows", id)
  add(
    blank(exposures$exposure) | !grepl("^[a-z][a-z0-9_]*$", exposures$exposure),
    "exposures.csv: exposure must be a snake_case id",
    id
  )
  add(!exposures$axis %in% values$axis, "exposures.csv: unknown axis", id)
  add(
    !blank(exposures$content_class) &
      !exposures$content_class %in% values$content_class,
    "exposures.csv: unknown content_class",
    id
  )
  add(
    !exposures$context %in% values$context,
    "exposures.csv: unknown context",
    id
  )
  add(
    is.na(exposures$include),
    "exposures.csv: include must be TRUE or FALSE",
    id
  )
  add(
    is.na(exposures$headline),
    "exposures.csv: headline must be TRUE or FALSE",
    id
  )
  add(
    exposures$headline %in% TRUE & !exposures$include %in% TRUE,
    "exposures.csv: headline rows must be included",
    id
  )

  is_metadata <- exposures$axis %in% "metadata"
  add(
    is_metadata & (!blank(exposures$unit) | !blank(exposures$day)),
    "exposures.csv: metadata rows take no unit or day",
    id
  )
  add(
    is_metadata & exposures$include %in% TRUE,
    "exposures.csv: metadata rows are never included",
    id
  )
  add(
    !is_metadata & !exposures$day %in% values$day,
    "exposures.csv: unknown day",
    id
  )
  schema_unit <- stats::setNames(
    as.character(dataschema$unit),
    dataschema$variable_name
  )
  expected_unit <- ifelse(
    schema_unit[id] %in% "hours",
    "hours",
    "index"
  )
  add(
    !is_metadata & id %in% schema_names & exposures$unit != expected_unit,
    "exposures.csv: unit differs from the dataschema",
    id
  )

  if (length(problems) > 0) {
    stop(
      "The exposure registry has problems:\n- ",
      paste(problems, collapse = "\n- "),
      call. = FALSE
    )
  }

  invisible(exposures)
}
