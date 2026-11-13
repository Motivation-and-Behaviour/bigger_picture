#' One dataset's line of the data manifest
#'
#' The manifest records which data a result rests on (DR-0007): counts and a
#' content hash, never names. The hash covers every harmonised row and column
#' of the dataset, so any change to its data changes it.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @return A one-row tibble, or NULL when the dataset is not included.
manifest_dataset <- function(harmonised, info) {
  data <- included_rows(harmonised, info)
  if (is.null(data)) {
    return(NULL)
  }
  primary <- data$st_measure_id %in% "primary"
  tibble::tibble(
    dataset_id = as.character(info$dataset_id),
    rows = nrow(data),
    primary_rows = sum(primary),
    participants = dplyr::n_distinct(data$participant_id),
    waves = dplyr::n_distinct(data$wave),
    columns = ncol(data),
    hash = rlang::hash(data)
  )
}
