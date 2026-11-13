#' Compare the live data manifest with a recorded one
#'
#' Lists every dataset added, removed or changed since the record was made
#' (ANALYSIS_PLAN.md, Step 9). Before a report is delivered any difference is
#' a deviation; afterwards the list is what the reconciliation table explains.
#'
#' @param manifest The live manifest.
#' @param recorded_path Path to a file written by `record_data_manifest()`.
#' @return A tibble with one row per dataset that differs: `dataset_id`,
#'   `change` (`added`, `removed` or `changed`), and the row and participant
#'   counts on each side. No rows when the two agree or no record exists.
compare_data_manifest <- function(manifest, recorded_path) {
  empty <- tibble::tibble(
    dataset_id = character(),
    change = character(),
    rows_recorded = numeric(),
    rows_live = numeric(),
    participants_recorded = numeric(),
    participants_live = numeric()
  )
  if (length(recorded_path) == 0 || !file.exists(recorded_path)) {
    return(empty)
  }
  recorded <- readr::read_csv(
    recorded_path,
    col_types = readr::cols(
      dataset_id = readr::col_character(),
      hash = readr::col_character(),
      .default = readr::col_guess()
    )
  )
  both <- dplyr::full_join(
    recorded[c("dataset_id", "rows", "participants", "hash")],
    manifest[c("dataset_id", "rows", "participants", "hash")],
    by = "dataset_id",
    suffix = c("_recorded", "_live")
  )
  both$change <- dplyr::case_when(
    is.na(both$hash_recorded) ~ "added",
    is.na(both$hash_live) ~ "removed",
    both$hash_recorded != both$hash_live ~ "changed",
    .default = NA_character_
  )
  both <- both[!is.na(both$change), ]
  dplyr::bind_rows(empty, both[names(empty)])
}
