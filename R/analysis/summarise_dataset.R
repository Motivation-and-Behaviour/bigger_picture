#' Every summary and check of one dataset, from one read of its rows
#'
#' One target per dataset computes all of them. Each needs the dataset's whole
#' harmonised table, which for the largest dataset is about 10 GB in memory,
#' so separate targets would load it once each, at the same time, on as many
#' workers.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param dataschema The dataschema.
#' @param invariants The table from `invariants.csv`.
#' @param exposures The exposure registry.
#' @param settings From `analysis_settings()`.
#' @return A named list: `manifest`, `grain`, `numeric_summary`,
#'   `level_summary`, `unit_summary`, `unit_counts`, `invariant_check` and
#'   `day_check`. Each element is NULL when the dataset is not included.
summarise_dataset <- function(
  harmonised,
  info,
  dataschema,
  invariants,
  exposures,
  settings = analysis_settings()
) {
  # The manifest hashes every column; everything after it needs only the
  # columns the dataset fills.
  manifest <- manifest_dataset(harmonised, info)
  harmonised <- drop_empty_columns(harmonised)
  list(
    manifest = manifest,
    grain = summarise_grain(harmonised, info),
    numeric_summary = summarise_numeric(harmonised, info, dataschema),
    level_summary = summarise_levels(
      harmonised,
      info,
      dataschema,
      settings$max_levels
    ),
    unit_summary = summarise_units(harmonised, info, settings),
    unit_counts = summarise_unit_counts(harmonised, info, dataschema),
    invariant_check = check_invariants(harmonised, info, invariants),
    day_check = check_day_consistency(harmonised, info, exposures)
  )
}
