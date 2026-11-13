#' Values per stage 1 unit, for every variable a dataset has
#'
#' Counts, per unit (wave by country) and dataschema variable, the primary
#' rows with a real value: neither missing nor `Unknown`. It is what tells
#' sporadic from systematic missingness per unit, and what the feasibility
#' counts are built from.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param dataschema The dataschema.
#' @return A tibble with one row per unit and variable that has any value:
#'   `dataset_id`, `wave`, `country`, `variable_name`, `n_value`. NULL when
#'   the dataset is not included.
summarise_unit_counts <- function(harmonised, info, dataschema) {
  rows <- included_rows(harmonised, info, primary_only = TRUE)
  if (is.null(rows)) {
    return(NULL)
  }
  unit <- data.table::data.table(
    wave = as.character(rows$wave),
    country = unit_country(rows)
  )
  variables <- setdiff(
    intersect(dataschema$variable_name, names(rows)),
    c("dataset_id", "dataset_name", "wave", "country", "st_measure_id")
  )
  count_one <- function(variable) {
    values <- rows[[variable]]
    real <- !is.na(values)
    if (!is.numeric(values)) {
      real <- real & !as.character(values) %in% "Unknown"
    }
    if (!any(real)) {
      return(NULL)
    }
    out <- unit[real, list(n_value = .N), by = c("wave", "country")]
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

# The country that defines a row's unit: `Unknown` is no country.
unit_country <- function(rows) {
  if (!"country" %in% names(rows)) {
    return(rep(NA_character_, nrow(rows)))
  }
  country <- as.character(rows$country)
  country[country %in% "Unknown"] <- NA_character_
  country
}
