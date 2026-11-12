#' Count one dataset's raw resources by role and reader
#'
#' @param resource_index A dataset's resource index.
#' @param dataset_id The dataset's ID.
#' @return A tibble of `dataset_id`, `role`, `reader` and `files`.
count_resources <- function(resource_index, dataset_id) {
  out <- dplyr::count(resource_index, .data$role, .data$reader, name = "files")
  out$dataset_id <- rep(as.character(dataset_id), nrow(out))
  out
}
