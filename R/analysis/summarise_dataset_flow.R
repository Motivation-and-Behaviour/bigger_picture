#' The dataset flow, from spec to inclusion
#'
#' The start of the flow diagram (ANALYSIS_PLAN.md, Step 2). It counts what
#' the repository knows: datasets with a spec, by status; datasets in the
#' pipeline; and datasets included in the analysis. Datasets identified or
#' requested but never given a spec are not in the repository and are added
#' to the diagram by hand.
#'
#' @param spec_files Paths of every dataset spec, tracked as files so a new
#'   spec or a changed status reruns this.
#' @param pipeline_ids IDs of the datasets the pipeline harmonises.
#' @param manifest The data manifest.
#' @return A tibble of `stage`, `datasets`, `participants` and `rows`.
summarise_dataset_flow <- function(spec_files, pipeline_ids, manifest) {
  status <- vapply(
    spec_files,
    function(file) {
      as.character(yaml::read_yaml(file)$status %||% NA_character_)
    },
    character(1)
  )
  count_status <- function(value) sum(status %in% value)
  tibble::tibble(
    stage = c(
      "Spec written",
      "Spec status: planned",
      "Spec status: blocked",
      "Spec status: in progress",
      "Spec status: harmonised",
      "In the pipeline",
      "Included in the analysis"
    ),
    datasets = c(
      length(status),
      count_status("planned"),
      count_status("blocked"),
      count_status("in_progress"),
      count_status("harmonised"),
      length(pipeline_ids),
      nrow(manifest)
    ),
    participants = c(rep(NA_real_, 6), sum(manifest$participants)),
    rows = c(rep(NA_real_, 6), sum(manifest$primary_rows))
  )
}
