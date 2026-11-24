#' A one-row summary of a prepared dataset
#'
#' The numbers behind the per-dataset review:
#' rows kept and dropped, what was used, and how the total was built.
#'
#' @param prepared Output of `prepare_analysis_data()`.
#' @param settings From `analysis_settings()`.
#' @return A one-row tibble, or NULL when the dataset is not included.
summarise_prepared <- function(
  prepared,
  settings = default_analysis_settings()
) {
  if (is.null(prepared)) {
    return(NULL)
  }
  base <- prepared$base
  exposures <- prepared$exposures
  audit <- prepared$row_audit
  dropped <- prepared$dropped
  dropped_for <- function(reason) sum(dropped$n[dropped$reason %in% reason])
  variables_with <- function(kind, status) {
    hit <- audit$kind %in% kind & audit$status %in% status
    dplyr::n_distinct(audit$variable_name[hit])
  }
  total <- exposures$total_screen
  source <- exposures$total_source
  required <- intersect(settings$total_required, names(exposures))
  could_sum <- if (length(required) == 2) {
    total_is_summed(source) & stats::complete.cases(exposures[required])
  } else {
    rep(FALSE, nrow(exposures))
  }
  tibble::tibble(
    dataset_id = if (nrow(base) > 0) base$dataset_id[1] else NA_character_,
    rows_kept = nrow(base),
    participants = dplyr::n_distinct(base$participant_id),
    units = dplyr::n_distinct(base$unit),
    units_small = sum(table(base$unit) < settings$min_unit_rows),
    dropped_age_missing = dropped_for("age_missing"),
    dropped_age_over_limit = dropped_for("age_over_limit"),
    dropped_trial_arm = dropped_for("trial_arm_after_baseline"),
    exposures_used = variables_with("exposure", "used"),
    outcomes_used = variables_with("outcome", c("used", "derived")),
    outcomes_derived = variables_with("outcome", "derived"),
    outcomes_dropped_repeat = variables_with("outcome", "dropped_repeat"),
    outcomes_dropped_part = variables_with("outcome", "dropped_part"),
    outcomes_excluded = variables_with("outcome", "excluded"),
    outcomes_archive = variables_with("outcome", "archive"),
    total_dedicated = sum(source %in% "dedicated" & !is.na(total)),
    total_dedicated_partial = sum(
      source %in% "dedicated_partial" & !is.na(total)
    ),
    total_summed = sum(total_is_summed(source) & !is.na(total)),
    total_summed_for_partial = sum(
      source %in% "summed_for_partial" & !is.na(total)
    ),
    total_capped = sum(exposures$total_capped),
    total_lost_to_incomplete = sum(could_sum & is.na(total)),
    total_coverage_max = bp_stat(exposures$total_coverage, max),
    total_median_dedicated = bp_stat(
      total[source %in% "dedicated"],
      stats::median
    ),
    total_median_dedicated_partial = bp_stat(
      total[source %in% "dedicated_partial"],
      stats::median
    ),
    total_median_summed = bp_stat(
      total[total_is_summed(source)],
      stats::median
    ),
    measure_rows = nrow(prepared$measures)
  )
}
