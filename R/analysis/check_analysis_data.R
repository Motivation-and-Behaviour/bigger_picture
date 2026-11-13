#' Check the harmonised data against the dataschema, as a list of findings
#'
#' Never a pipeline failure: the result is a tidy table to review
#' (ANALYSIS_PLAN.md, Step 3). Every check works from the per-dataset
#' summaries, so no row of data is read here. A `review` finding needs a
#' look; an `advisory` finding is context.
#'
#' Checks, in `check`:
#'
#' - `below_min`, `above_max`: numeric values outside the schema's bounds.
#'   The guide keeps these for the analysis to screen, so they are counted,
#'   not expected to be zero. Ages over the bound are advisory, because the
#'   age rule of the preparation step removes them.
#' - `outside_allowed_values`: a level the schema does not list. Advisory
#'   until `allowed_values` is maintained (DR-0006).
#' - `screen_constant`, `screen_all_zero`: a screen-time variable with one
#'   distinct value, or only zeros, across a wave; `screen_nearly_all_zero`
#'   (advisory) where 99% or more are zero.
#' - `screen_scale`: a screen-time variable in hours whose upper quartile in
#'   a dataset is more than 4 times, or less than a twentieth of, the median
#'   dataset's, which is how a minutes-for-hours error shows. On the primary
#'   measure, one value per dataset, so a many-wave dataset does not set its
#'   own reference.
#' - `usual_day_mismatch`: usual-day values that differ from
#'   (5 x weekday + 2 x weekend) / 7 on rows that have all three.
#' - `invariant_varies`: participants with more than one value on a row the
#'   guide holds constant.
#' - `carry_gap`: participants with a value at some waves and none at others
#'   on a row the guide carries.
#' - `carried_value_changes` (advisory): participants who gave different
#'   answers at different waves on a carried row, which the guide keeps.
#' - `duplicate_key`: more than one row for a participant, wave and measure.
#' - `missing_participant_id`: rows with no participant ID.
#'
#' @param numeric_summary Combined output of `summarise_numeric()`.
#' @param level_summary Combined output of `summarise_levels()`.
#' @param grain Combined output of `summarise_grain()`.
#' @param invariant_checks Combined output of `check_invariants()`.
#' @param day_checks Combined output of `check_day_consistency()`.
#' @param invariants The table from `invariants.csv`.
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
  day_checks,
  invariants,
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
      ifelse(high$variable_name == "age_years", "advisory", "review"),
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
  nearly <- st[st$n_zero < st$n & st$n_zero >= 0.99 * st$n, ]
  screen <- list(
    finding(
      constant,
      "screen_constant",
      "review",
      constant$n,
      constant$n,
      paste0("every value is ", signif(constant$median, 4))
    ),
    finding(all_zero, "screen_all_zero", "review", all_zero$n, all_zero$n, ""),
    finding(
      nearly,
      "screen_nearly_all_zero",
      "advisory",
      nearly$n_zero,
      nearly$n,
      paste0(nearly$n - nearly$n_zero, " values above zero")
    )
  )

  # Location against other datasets: one upper quartile per dataset, on the
  # primary measure, compared with the median dataset.
  per_dataset <- dplyr::summarise(
    st[st$st_measure_id %in% "primary" & st$p75 > 0, ],
    p75 = stats::median(.data$p75),
    n = sum(.data$n),
    .by = c("dataset_id", "variable_name")
  )
  typical <- dplyr::summarise(
    per_dataset,
    across_datasets = stats::median(.data$p75),
    datasets = dplyr::n(),
    .by = "variable_name"
  )
  scale <- dplyr::inner_join(per_dataset, typical, by = "variable_name")
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
      "upper quartile ",
      signif(off$p75, 3),
      " hours against ",
      signif(off$across_datasets, 3),
      " in the median dataset"
    )
  )

  mismatch <- day_checks[day_checks$n_differs > 0, ]
  usual_finding <- finding(
    mismatch,
    "usual_day_mismatch",
    "review",
    mismatch$n_differs,
    mismatch$n,
    paste0(
      "usual-day mean ",
      signif(mismatch$mean_usual, 3),
      " against ",
      signif(mismatch$mean_expected, 3),
      " from weekday and weekend"
    )
  )

  checks <- dplyr::left_join(
    invariant_checks,
    invariants[c("variable_name", "rule")],
    by = "variable_name"
  )
  varying <- checks[checks$participants_varying > 0, ]
  must_hold <- varying[varying$rule %in% "constant", ]
  may_change <- varying[!varying$rule %in% "constant", ]
  gaps <- checks[checks$rule %in% "carried" & checks$participants_gap > 0, ]
  carried <- list(
    finding(
      must_hold,
      "invariant_varies",
      "review",
      must_hold$participants_varying,
      must_hold$participants_repeated,
      "participants with more than one value on a row held constant"
    ),
    finding(
      gaps,
      "carry_gap",
      "review",
      gaps$participants_gap,
      gaps$participants_repeated,
      "participants with a value at some waves and none at others"
    ),
    finding(
      may_change,
      "carried_value_changes",
      "advisory",
      may_change$participants_varying,
      may_change$participants_repeated,
      "participants who gave different answers at different waves"
    )
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
    list(scale_finding, usual_finding),
    carried,
    keys
  )
  out[order(as.numeric(out$dataset_id), out$check, out$variable_name), ]
}
