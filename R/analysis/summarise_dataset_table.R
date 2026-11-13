#' One row per included dataset, for the report's appendix
#'
#' Design, countries, collection years, ages, participants and waves, and how
#' many shared variables the dataset supplies (ANALYSIS_PLAN.md, Step 6).
#' Datasets are identified by number, never by name.
#'
#' @param units Combined `units` tables from `describe_dataset()`.
#' @param mapping_counts Combined output of `count_mapping()`.
#' @param citations The dataset citations (`dataset_id`, `citekey`), from
#'   `references/`. Each dataset's keys are written as a pandoc citation,
#'   `[@key1; @key2]`, which a Quarto document that lists the bibliography
#'   formats in its own style; blank where a dataset has none yet.
#' @return A tibble with one row per dataset.
summarise_dataset_table <- function(units, mapping_counts, citations = NULL) {
  design <- dplyr::summarise(
    units,
    participants_rows = sum(.data$rows),
    waves = dplyr::n_distinct(.data$wave),
    countries = dplyr::n_distinct(.data$country, na.rm = TRUE),
    first_year = bp_stat(.data$year, min),
    last_year = bp_stat(.data$year, max),
    youngest = bp_stat(.data$age_median, min),
    oldest = bp_stat(.data$age_median, max),
    follows_participants = any(.data$repeats_participants),
    weighted = any(.data$weighted),
    .by = "dataset_id"
  )
  design$design <- dplyr::case_when(
    design$follows_participants ~ "Follows the same children",
    design$waves > 1 ~ "Repeated survey, new sample each time",
    .default = "Single occasion"
  )
  mapped <- dplyr::summarise(
    mapping_counts[mapping_counts$status %in% bp_evaluated_statuses(), ],
    variables_supplied = sum(.data$rows),
    .by = "dataset_id"
  )
  out <- dplyr::left_join(design, mapped, by = "dataset_id")
  out$sources <- rep("", nrow(out))
  if (!is.null(citations) && nrow(citations) > 0) {
    keys <- tapply(
      citations$citekey,
      citations$dataset_id,
      function(k) paste0("[", paste0("@", k, collapse = "; "), "]")
    )
    found <- match(out$dataset_id, names(keys))
    out$sources[!is.na(found)] <- unname(keys[found[!is.na(found)]])
  }
  out[order(as.numeric(out$dataset_id)), ]
}
