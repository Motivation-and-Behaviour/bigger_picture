#' Check the harmonised data against the dataschema, as a list of findings
#'
#' Never a pipeline failure: the result is a tidy table to review
#' (ANALYSIS_PLAN.md, Step 3). Every check works from the per-dataset
#' summaries, so no row of data is read here.
#'
#' Checks, in `check`:
#'
#' - `below_min`, `above_max`: numeric values outside the schema's bounds.
#'   The guide keeps these for the analysis to screen, so they are counted,
#'   not expected to be zero.
#' - `outside_allowed_values`: a level the schema does not list. Advisory
#'   until `allowed_values` is maintained (DR-0006).
#' - `screen_constant`, `screen_all_zero`: a screen-time variable with one
#'   distinct value, or only zeros, across a wave.
#' - `screen_scale`: a screen-time variable in hours whose 75th percentile is
#'   more than 4 times, or less than a twentieth of, the typical value across
#'   datasets, which is how a minutes-for-hours error shows.
#' - `usual_day_outside_week`: a usual-day mean well outside the range of the
#'   weekday and weekend means of the same exposure.
#' - `invariant_varies`: participants with more than one value on a row the
#'   guide carries within participant.
#' - `duplicate_key`: more than one row for a participant, wave and measure.
#' - `missing_participant_id`: rows with no participant ID.
#'
#' @param numeric_summary Combined output of `summarise_numeric()`.
#' @param level_summary Combined output of `summarise_levels()`.
#' @param grain Combined output of `summarise_grain()`.
#' @param invariant_checks Combined output of `check_invariants()`.
#' @param dataschema The dataschema.
#' @param exposures The exposure registry.
#' @return A tibble: `dataset_id`, `variable_name`, `wave`, `st_measure_id`,
#'   `check`, `severity` (`review` or `advisory`), `n` (cells or participants
#'   affected), `denominator` and `detail`.
check_analysis_data <- function(
  numeric_summary,
  level_summary,
  grain,
  invariant_checks,
  dataschema,
  exposures
) {
  finding <- function(tbl, check, severity, n, denominator, detail) {
    if (nrow(tbl) == 0) {
      return(NULL)
    }
    column <- function(name) {
      if (name %in% names(tbl)) {
        as.character(tbl[[name]])
      } else {
        rep(NA_character_, nrow(tbl))
      }
    }
    tibble::tibble(
      dataset_id = tbl$dataset_id,
      variable_name = column("variable_name"),
      wave = column("wave"),
      st_measure_id = column("st_measure_id"),
      check = check,
      severity = severity,
      n = as.numeric(n),
      denominator = as.numeric(denominator),
      detail = as.character(detail)
    )
  }
  num <- numeric_summary

  low <- num[(num$n_below_min > 0) %in% TRUE, ]
  high <- num[(num$n_above_max > 0) %in% TRUE, ]
  bounds <- list(
    finding(
      low,
      "below_min",
      "review",
      low$n_below_min,
      low$n,
      paste0("minimum observed ", signif(low$min, 4))
    ),
    finding(
      high,
      "above_max",
      "review",
      high$n_above_max,
      high$n,
      paste0("maximum observed ", signif(high$max, 4))
    )
  )

  allowed <- dataschema[
    !is.na(dataschema$allowed_values) & nzchar(dataschema$allowed_values),
    c("variable_name", "allowed_values")
  ]
  lev <- dplyr::inner_join(level_summary, allowed, by = "variable_name")
  listed <- mapply(
    function(level, values) level %in% strsplit(values, "|", fixed = TRUE)[[1]],
    lev$level,
    lev$allowed_values
  )
  outside <- lev[!as.logical(listed) & lev$level != "(many values)", ]
  outside <- finding(
    outside,
    "outside_allowed_values",
    "advisory",
    outside$n,
    NA,
    paste0("level '", outside$level, "'")
  )

  hours <- exposures$variable_name[exposures$unit %in% "hours"]
  st <- num[num$variable_name %in% hours & num$n >= 30, ]
  constant <- st[st$n_distinct == 1 & st$n_zero < st$n, ]
  all_zero <- st[st$n_zero == st$n, ]
  screen <- list(
    finding(
      constant,
      "screen_constant",
      "review",
      constant$n,
      constant$n,
      paste0("every value is ", signif(constant$median, 4))
    ),
    finding(all_zero, "screen_all_zero", "review", all_zero$n, all_zero$n, "")
  )

  # Location against other datasets, on the 75th percentile so zero-heavy
  # variables still have something to compare.
  typical <- dplyr::summarise(
    st[st$p75 > 0, ],
    across_datasets = stats::median(.data$p75),
    datasets = dplyr::n_distinct(.data$dataset_id),
    .by = "variable_name"
  )
  scale <- dplyr::inner_join(st[st$p75 > 0, ], typical, by = "variable_name")
  scale <- scale[scale$datasets >= 5, ]
  ratio <- scale$p75 / scale$across_datasets
  off <- scale[ratio > 4 | ratio < 0.05, ]
  scale_finding <- finding(
    off,
    "screen_scale",
    "review",
    off$n,
    off$n,
    paste0(
      "75th percentile ",
      signif(off$p75, 3),
      " hours against ",
      signif(off$across_datasets, 3),
      " across datasets"
    )
  )

  days <- exposures[
    exposures$unit %in% "hours" & exposures$day %in% bp_exposure_days(),
    c("variable_name", "exposure", "context", "day")
  ]
  by_day <- dplyr::inner_join(
    st[c("dataset_id", "variable_name", "wave", "st_measure_id", "mean", "n")],
    days,
    by = "variable_name"
  )
  wide <- tidyr::pivot_wider(
    by_day,
    id_cols = c("dataset_id", "wave", "st_measure_id", "exposure", "context"),
    names_from = "day",
    values_from = c("mean", "n", "variable_name")
  )
  needed <- paste0("mean_", bp_exposure_days())
  if (all(needed %in% names(wide))) {
    wide <- wide[stats::complete.cases(wide[needed]), ]
    lower <- pmin(wide$mean_weekday, wide$mean_weekend)
    upper <- pmax(wide$mean_weekday, wide$mean_weekend)
    gap <- wide[wide$mean_usual < 0.8 * lower | wide$mean_usual > 1.2 * upper, ]
    gap$variable_name <- gap$variable_name_usual
    usual_finding <- finding(
      gap,
      "usual_day_outside_week",
      "review",
      gap$n_usual,
      gap$n_usual,
      paste0(
        "usual ",
        signif(gap$mean_usual, 3),
        ", weekday ",
        signif(gap$mean_weekday, 3),
        ", weekend ",
        signif(gap$mean_weekend, 3)
      )
    )
  } else {
    usual_finding <- NULL
  }

  varying <- invariant_checks[invariant_checks$participants_varying > 0, ]
  varying <- finding(
    varying,
    "invariant_varies",
    "review",
    varying$participants_varying,
    varying$participants_repeated,
    "participants with more than one value"
  )

  duplicated <- grain[grain$duplicate_keys > 0, ]
  no_id <- grain[grain$missing_ids > 0, ]
  keys <- list(
    finding(
      duplicated,
      "duplicate_key",
      "review",
      duplicated$duplicate_keys,
      duplicated$participants,
      "participants with more than one row in the wave and measure"
    ),
    finding(
      no_id,
      "missing_participant_id",
      "review",
      no_id$missing_ids,
      no_id$rows,
      ""
    )
  )

  out <- dplyr::bind_rows(
    list(tibble::tibble(
      dataset_id = character(),
      variable_name = character(),
      wave = character(),
      st_measure_id = character(),
      check = character(),
      severity = character(),
      n = numeric(),
      denominator = numeric(),
      detail = character()
    )),
    bounds,
    list(outside),
    screen,
    list(scale_finding, usual_finding, varying),
    keys
  )
  out[order(as.numeric(out$dataset_id), out$check, out$variable_name), ]
}

bp_exposure_days <- function() {
  c("usual", "weekday", "weekend")
}
