#' Read the list of points where a dataset's screen item changed
#'
#' A repeated survey sometimes changes how it asks about screen use: a new
#' wording, new response bands, hours a week in place of hours a day. Values
#' either side of such a change are not one series, and a trend fitted across
#' it would read the redesign as change over time. Each line of
#' `series_breaks.csv` names a dataset, an exposure and the first year of the
#' new item, with the reason, taken from the dataset's mapping notes.
#'
#' @param path Path to `series_breaks.csv`.
#' @param exposure_registry The exposure registry.
#' @return A tibble of `dataset_id`, `exposure`, `from_year` and `reason`.
read_series_breaks <- function(
  path = bp_series_breaks_path(),
  exposure_registry
) {
  breaks <- read_registry_csv(path, bp_series_breaks_columns())
  fail <- function(...) stop(basename(path), ": ", ..., call. = FALSE)
  unknown <- setdiff(breaks$exposure, exposure_registry$exposure)
  if (length(unknown) > 0) {
    fail("not exposures: ", paste(unknown, collapse = ", "))
  }
  breaks$from_year <- suppressWarnings(as.numeric(breaks$from_year))
  if (anyNA(breaks$from_year) || anyNA(breaks$reason)) {
    fail("every line needs a year and a reason")
  }
  breaks
}
