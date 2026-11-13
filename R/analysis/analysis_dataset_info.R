#' What the analysis knows about one dataset, and whether it is included
#'
#' Every per-dataset analysis target reads this one-row slice of
#' `datasets.csv`, so an edit to another dataset's row leaves it unchanged
#' and nothing downstream of it reruns.
#'
#' Inclusion (ANALYSIS_PLAN.md, Step 2): until any dataset has been signed
#' off, every dataset in the pipeline is included; from then on, only
#' datasets with `signed_off` TRUE.
#'
#' @param datasets The table from `read_analysis_datasets()`.
#' @param dataset_id One dataset ID.
#' @return A one-row tibble: the dataset's `datasets.csv` row plus `included`.
analysis_dataset_info <- function(datasets, dataset_id) {
  dataset_id <- as.character(dataset_id)
  info <- datasets[datasets$dataset_id %in% dataset_id, ]
  if (nrow(info) == 0) {
    info <- dplyr::bind_rows(info, tibble::tibble(dataset_id = dataset_id))
  }
  info <- info[1, ]

  sign_off_started <- any(datasets$signed_off %in% TRUE)
  info$included <- !sign_off_started || info$signed_off %in% TRUE
  info
}
