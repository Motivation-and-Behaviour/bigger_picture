#' Template tidier for pre-harmonisation dataset shaping
#'
#' Use this step for study-specific table assembly that is awkward to represent
#' directly in `layout.csv`, for example joining multiple raw files, binding
#' waves, or producing named intermediate tables that the harmonisation layout
#' can then reference.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - a list with `data`
#' - optional `meta`
tidy_BPIPD_TEMPLATE <- function(raw_dataset, spec) {
  list(
    data = raw_dataset$data,
    meta = raw_dataset$meta
  )
}
