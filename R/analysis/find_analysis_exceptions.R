#' Path to a dataset's analysis exceptions file, when it exists
#'
#' Tracked as a file target for every dataset. A dataset with no file gives a
#' zero-length path, and a file target with no path cannot notice a file
#' being created, so the pipeline works out `expected` for each dataset when
#' it is defined and passes it in: adding or removing a file then changes
#' that dataset's target.
#'
#' @param dataset_id The dataset's ID.
#' @param expected Whether the pipeline found a file when it was defined;
#'   only there to change the target when that changes.
#' @return A path, or `character(0)`.
find_analysis_exceptions <- function(dataset_id, expected = NA) {
  path <- fs::path(
    bp_harmonisation_dataset_dir(dataset_id),
    "analysis_exceptions.csv"
  )
  if (file.exists(path)) as.character(path) else character()
}
