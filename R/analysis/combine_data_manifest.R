#' The data manifest: one row per included dataset, with an overall hash
#'
#' @param ... Rows from `manifest_dataset()`, or tibbles of them; NULLs are
#'   dropped.
#' @return A tibble sorted by dataset ID, with the hash of the whole set of
#'   dataset hashes repeated in `overall_hash`.
combine_data_manifest <- function(...) {
  manifest <- dplyr::bind_rows(...)
  if (nrow(manifest) == 0) {
    return(tibble::tibble(dataset_id = character(), overall_hash = character()))
  }
  manifest <- manifest[order(as.numeric(manifest$dataset_id)), ]
  manifest$overall_hash <- rlang::hash(manifest[c("dataset_id", "hash")])
  manifest
}
