#' Read the list of points where a dataset's screen item changed
#'
#' A repeated survey sometimes changes how it asks about screen use: a new
#' wording, new response bands, hours a week in place of hours a day. Values
#' either side of such a change are not one series, and a trend fitted across
#' it would read the redesign as change over time. Each line of
#' `series_breaks.csv` names a dataset, an exposure and where the changed item
#' applies, with the reason, taken from the study's questionnaires and
#' codebooks.
#'
#' A line says where the changed item applies in one of two ways. `from_year`
#' gives the first collection year of the new item, for a change that holds
#' from then on. `waves` lists the waves that used it, separated by
#' semicolons, for a study whose waves share or straddle calendar years or
#' whose item came and went (a mail-out questionnaire between main
#' interviews). `day` limits a line to the weekday, weekend or usual-day
#' variable of the exposure; blank means all of them.
#'
#' @param path Path to `series_breaks.csv`.
#' @param exposure_registry The exposure registry.
#' @return A tibble of `dataset_id`, `exposure`, `day`, `from_year`, `waves`
#'   (a list of character vectors) and `reason`.
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
  unknown <- setdiff(breaks$day, c(NA, exposure_registry$day))
  if (length(unknown) > 0) {
    fail("not days: ", paste(unknown, collapse = ", "))
  }
  year_text <- breaks$from_year
  breaks$from_year <- suppressWarnings(as.numeric(year_text))
  year <- breaks$from_year[!is.na(year_text)]
  if (anyNA(year) || any(year != round(year) | year < 1900 | year > 2100)) {
    fail("`from_year` must be a year")
  }
  if (any(is.na(breaks$from_year) == is.na(breaks$waves))) {
    fail("every line needs either a year or a list of waves, not both")
  }
  if (anyNA(breaks$reason) || anyNA(breaks$dataset_id)) {
    fail("every line needs a dataset and a reason")
  }
  breaks$waves <- lapply(strsplit(breaks$waves, ";", fixed = TRUE), trimws)
  breaks$waves[!is.na(year_text)] <- list(character())
  if (any(unlist(breaks$waves) %in% "")) {
    fail("`waves` has an empty entry")
  }
  key <- paste(
    breaks$dataset_id,
    breaks$exposure,
    breaks$day,
    year_text,
    vapply(breaks$waves, paste, character(1), collapse = ";"),
    sep = "\r"
  )
  if (anyDuplicated(key) > 0) {
    fail("the same line appears twice")
  }
  breaks
}

#' List the recorded item changes that separate no units
#'
#' A line of `series_breaks.csv` should have units with a value for its
#' exposure (and day) on both sides. One that does not splits nothing: its
#' year is outside the years collected, a wave label is misspelt, or the
#' dataset is not in the build. Such lines are listed so that they are
#' corrected and not silently ignored. A line whose exposure has no
#' descriptive statistics is checked against all of the dataset's units.
#'
#' @param breaks The table from `read_series_breaks()`.
#' @param units Per-unit descriptions with `unit`, `dataset_id`, `wave` and
#'   `year`.
#' @param screen Per-unit screen statistics with `unit` and `variable_name`,
#'   as `described_screen`.
#' @param exposure_registry The exposure registry.
#' @return The lines of `breaks` that separate no units, with `units` (the
#'   units the line is checked against) and `matched` (those on the new side
#'   of the line).
check_series_breaks <- function(breaks, units, screen, exposure_registry) {
  breaks$units <- NA_integer_
  breaks$matched <- NA_integer_
  for (i in seq_len(nrow(breaks))) {
    own <- units[units$dataset_id %in% breaks$dataset_id[i], ]
    rows <- exposure_registry$exposure %in% breaks$exposure[i]
    described <- intersect(
      exposure_registry$variable_name[rows],
      screen$variable_name
    )
    if (length(described) > 0) {
      wanted <- rows &
        (is.na(breaks$day[i]) |
          exposure_registry$day %in% breaks$day[i])
      measured <- screen$unit[
        screen$variable_name %in% exposure_registry$variable_name[wanted]
      ]
      own <- own[own$unit %in% measured, ]
    }
    breaks$units[i] <- nrow(own)
    breaks$matched[i] <- sum(series_break_applies(breaks[i, ], own))
  }
  breaks$waves <- vapply(breaks$waves, paste, character(1), collapse = ";")
  breaks[breaks$matched == 0 | breaks$matched == breaks$units, ]
}

# Whether each unit is on the new side of one line of the breaks table.
series_break_applies <- function(line, units) {
  if (is.na(line$from_year)) {
    return(units$wave %in% line$waves[[1]])
  }
  !is.na(units$year) & units$year >= line$from_year
}
