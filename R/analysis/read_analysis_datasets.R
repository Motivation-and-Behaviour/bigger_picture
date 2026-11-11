#' Read datasets.csv, with a blank row for any dataset it does not list
#'
#' A dataset PR never edits `analysis/`, so a dataset can reach the pipeline
#' before it has a row. A missing row means the same as a blank one: no
#' baseline and not signed off.
#'
#' @param path Path to `datasets.csv`.
#' @param dataset_ids Dataset IDs that must each have a row.
#' @return A tibble with one row for each listed dataset and each of
#'   `dataset_ids`.
read_analysis_datasets <- function(
  path = bp_analysis_datasets_path(),
  dataset_ids = character()
) {
  datasets <- read_registry_csv(
    path,
    bp_analysis_datasets_columns(),
    c("team_contributed", "signed_off")
  )
  dplyr::bind_rows(
    datasets,
    tibble::tibble(dataset_id = setdiff(dataset_ids, datasets$dataset_id))
  )
}
