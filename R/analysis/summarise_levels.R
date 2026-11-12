#' Counts of every level of every categorical variable, per dataset and wave
#'
#' Feeds coverage, the demographics tables and the check against
#' `allowed_values`. Screen-time variables are counted for every measure
#' block and other variables on the primary rows only. For an identifier
#' column, and any column with more distinct values than `max_levels`, only
#' the number of non-missing values is kept, under the level `(many values)`,
#' so no participant, family or cluster ID reaches a summary.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param dataschema The dataschema.
#' @param max_levels Largest number of distinct values tabulated.
#' @return A tibble with one row per variable, wave, measure and level, or
#'   NULL.
summarise_levels <- function(
  harmonised,
  info,
  dataschema,
  max_levels = analysis_settings()$max_levels
) {
  rows <- included_rows(harmonised, info)
  if (is.null(rows)) {
    return(NULL)
  }
  variables <- dataschema$variable_name[dataschema$data_type %in% "character"]
  variables <- setdiff(
    intersect(variables, names(rows)),
    c("dataset_id", "dataset_name", "wave", "st_measure_id")
  )
  wave <- as.character(rows$wave)
  measure <- as.character(rows$st_measure_id)
  primary <- measure %in% "primary"

  count_one <- function(variable) {
    values <- as.character(rows[[variable]])
    keep <- !is.na(values)
    if (!startsWith(variable, "st_")) {
      keep <- keep & primary
    }
    if (!any(keep)) {
      return(NULL)
    }
    dt <- data.table::data.table(
      wave = wave[keep],
      st_measure_id = measure[keep],
      level = values[keep]
    )
    identifier <- variable %in% bp_identifier_columns()
    if (identifier || data.table::uniqueN(dt$level) > max_levels) {
      dt$level <- "(many values)"
    }
    out <- dt[, list(n = .N), by = c("wave", "st_measure_id", "level")]
    out$variable_name <- variable
    out
  }

  out <- tibble::as_tibble(data.table::rbindlist(lapply(variables, count_one)))
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$dataset_id <- as.character(info$dataset_id)
  dplyr::relocate(out, "dataset_id", "variable_name")
}

# Columns that identify a person, family or sampling unit: their values are
# never tabulated, however few a dataset has.
bp_identifier_columns <- function() {
  c("participant_id", "family_id", "survey_strata", "survey_cluster")
}
