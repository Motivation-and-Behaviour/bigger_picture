#' Check derivations.csv against the dataschema
#'
#' Stops with every problem found. Targets and sources are dataschema rows, a
#' target is never one of its own sources, `method` comes from
#' `bp_derivation_methods()`, `weights` holds one positive number per source
#' for `weighted_mean` and is blank otherwise, and a target has at most one
#' rule for all datasets and one per named dataset.
#'
#' @param derivations Derivations tibble.
#' @param dataschema The dataschema.
#' @return `derivations`, invisibly checked.
validate_derivations <- function(derivations, dataschema) {
  problems <- character()
  add <- function(condition, message, ids) {
    if (any(condition)) {
      problems <<- c(
        problems,
        paste0(message, ": ", paste(unique(ids[condition]), collapse = ", "))
      )
    }
  }
  id <- derivations$target
  schema_names <- as.character(dataschema$variable_name)
  sources <- strsplit(derivations$sources, "|", fixed = TRUE)
  weights <- strsplit(
    ifelse(
      is.na(derivations$weights),
      "",
      derivations$weights
    ),
    "|",
    fixed = TRUE
  )

  add(
    !id %in% schema_names,
    "derivations.csv: targets not in the dataschema",
    id
  )
  add(
    vapply(
      sources,
      function(x) length(x) == 0 || !all(x %in% schema_names),
      TRUE
    ),
    "derivations.csv: sources missing or not in the dataschema",
    id
  )
  add(
    mapply(function(target, x) target %in% x, id, sources),
    "derivations.csv: target is one of its own sources",
    id
  )
  add(
    !derivations$method %in% bp_derivation_methods(),
    "derivations.csv: unknown method",
    id
  )
  weighted <- derivations$method %in% "weighted_mean"
  numbers <- lapply(weights, function(x) suppressWarnings(as.numeric(x)))
  add(
    weighted &
      !mapply(
        function(w, x) length(w) == length(x) && all(!is.na(w) & w > 0),
        numbers,
        sources
      ),
    "derivations.csv: weighted_mean needs one positive weight per source",
    id
  )
  add(
    !weighted & !is.na(derivations$weights),
    "derivations.csv: weights set on a method that takes none",
    id
  )
  add(
    duplicated(derivations[c("target", "dataset_id")]),
    "derivations.csv: more than one rule for a target and dataset",
    id
  )

  if (length(problems) > 0) {
    stop(paste(problems, collapse = "\n"), call. = FALSE)
  }
  invisible(derivations)
}
