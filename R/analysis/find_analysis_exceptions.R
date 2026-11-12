#' Path to a dataset's analysis exceptions file, when it exists
#'
#' Tracked as a file target for every dataset, so a new file is noticed. A
#' dataset with no file gives a zero-length path.
#'
#' @param dataset_id The dataset's ID.
#' @return A path, or `character(0)`.
find_analysis_exceptions <- function(dataset_id) {
  path <- fs::path(
    bp_harmonisation_dataset_dir(dataset_id),
    "analysis_exceptions.csv"
  )
  if (file.exists(path)) as.character(path) else character()
}
