#' Prepare one dataset for modelling
#'
#' Turns a dataset's harmonised rows into what the models need, with a fixed
#' contract, as the tidier does for raw files (ANALYSIS_PLAN.md, Step 4).
#' Generic rules do almost all of it; the outcome registry holds what is true
#' of a row everywhere; the dataset's optional exceptions file holds decisions
#' about this dataset alone.
#'
#' The tables are per dataset and share one row order, so a model reads the
#' columns it needs without a join:
#'
#' - `base`: one row per eligible participant and wave, with ids, unit,
#'   design variables, covariates and the family, mediator and covariate rows;
#' - `exposures`: screen-time rows in hours (or a row's own index), with
#'   `total_screen` and its source, coverage and leave-one-out sums;
#' - `outcomes` and `outcomes_z`: the score of every analysed outcome row,
#'   and its standardised form for continuous rows;
#' - `measures`: the extra screen-time measure blocks (diaries, a second
#'   informant) for the occasions in `base`, for comparisons between measures
#'   within a study; `concurrent` is FALSE for recall blocks;
#' - `row_audit`: one row per wave and dataschema row, saying whether it was
#'   used and, if not, why, so that no row disappears silently;
#' - `dropped`: rows dropped per wave and reason;
#' - `repeats`: outcome rows found to repeat another row's score;
#'   `nodes`: any node an exception assigns in place of the registry's; and
#'   `derived`: the derivation rules that ran.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param outcome_registry,exposure_registry,derivations The registries.
#' @param dataschema The dataschema.
#' @param exceptions The dataset's analysis exceptions.
#' @param total_status The harmonisation status of the dataset's `st_total`
#'   mapping (`total_mapping_status()`); a `partial` one is not a total of
#'   all screen use (DR-0013).
#' @param settings From `analysis_settings()`.
#' @return A named list of the tables above, or NULL when the dataset is not
#'   included.
prepare_analysis_data <- function(
  harmonised,
  info,
  outcome_registry,
  exposure_registry,
  derivations,
  dataschema,
  exceptions,
  settings = default_analysis_settings(),
  total_status = NA_character_
) {
  all_rows <- included_rows(drop_empty_columns(harmonised), info)
  rm(harmonised)
  if (is.null(all_rows)) {
    return(NULL)
  }
  dataset_id <- as.character(info$dataset_id)
  primary <- all_rows$st_measure_id %in% "primary"
  # A copy only where there is something to leave out.
  rows <- if (all(primary)) all_rows else all_rows[primary, ]

  carried_roles <- outcome_registry$rows$variable_name[
    outcome_registry$rows$role %in% c("covariate", "mediator", "measure")
  ]
  carry <- unique(c(
    dataschema$variable_name[
      dataschema$domain %in% c("provenance", "demographics", "family")
    ],
    carried_roles,
    exposure_registry$variable_name[exposure_registry$axis %in% "metadata"]
  ))
  prepared <- prepare_base(rows, info, carry, settings)
  base <- prepared$base
  kept <- if (all(prepared$keep)) rows else rows[prepared$keep, ]
  rm(rows)

  # A wave no row of the dataset has is a mistake in any exception.
  named <- exceptions$wave[!is.na(exceptions$wave)]
  if (!all(named %in% as.character(base$wave))) {
    stop(
      "Dataset ",
      info$dataset_id[1],
      ": an analysis exception names a wave the dataset does not have: ",
      paste(setdiff(named, as.character(base$wave)), collapse = ", "),
      call. = FALSE
    )
  }
  # Exceptions about a screen-time row are for the exposures; the rest are
  # for the outcomes.
  on_exposure <- exceptions$variable_name %in% exposure_registry$variable_name
  misplaced <- on_exposure &
    !exceptions$action %in% c("exclude", "total_dedicated", "part_of_total")
  if (any(misplaced)) {
    stop(
      "Dataset ",
      info$dataset_id[1],
      ": an exception on a screen-time row can only exclude it, say its ",
      "total is dedicated, or say it is part of the total: ",
      paste(unique(exceptions$variable_name[misplaced]), collapse = ", "),
      call. = FALSE
    )
  }
  exposure_exceptions <- exceptions[on_exposure, ]
  # Saying a total is dedicated only means something for a total mapped as
  # partial, at a wave the dataset has.
  made_dedicated <- exposure_exceptions[
    exposure_exceptions$action %in% "total_dedicated",
  ]
  unknown_wave <- setdiff(
    made_dedicated$wave[!is.na(made_dedicated$wave)],
    as.character(base$wave)
  )
  if (nrow(made_dedicated) > 0 && !total_status %in% "partial") {
    stop(
      "Dataset ",
      info$dataset_id[1],
      ": a `total_dedicated` exception needs `st_total` mapped as partial.",
      call. = FALSE
    )
  }
  # A class is part of a dedicated total: there must be such a total, the
  # class must have values, and the wave must be one of the dataset's.
  parts <- exposure_exceptions[
    exposure_exceptions$action %in% "part_of_total",
  ]
  classes <- exposure_registry$variable_name[
    exposure_registry$unit %in%
      "hours" &
      exposure_registry$day %in% "usual" &
      exposure_registry$context %in% "any" &
      exposure_registry$axis %in% "activity" &
      !is.na(exposure_registry$content_class)
  ]
  has_total <- if ("st_total" %in% names(kept)) {
    !is.na(kept$st_total)
  } else {
    rep(FALSE, nrow(kept))
  }
  total_at <- function(at_wave) {
    any(has_total & (is.na(at_wave) | as.character(base$wave) %in% at_wave))
  }
  bad_part <- nrow(parts) > 0 &&
    (!all(parts$variable_name %in% intersect(classes, names(kept))) ||
      !all(vapply(parts$wave, total_at, logical(1))))
  if (bad_part) {
    stop(
      "Dataset ",
      info$dataset_id[1],
      ": a `part_of_total` exception needs a dedicated total at its wave ",
      "and a usual-day content-class row the dataset has.",
      call. = FALSE
    )
  }
  if (length(unknown_wave) > 0) {
    stop(
      "Dataset ",
      info$dataset_id[1],
      ": a `total_dedicated` exception names a wave the dataset does not ",
      "have: ",
      paste(unknown_wave, collapse = ", "),
      call. = FALSE
    )
  }
  exceptions <- exceptions[!on_exposure, ]
  # Waves at which an outcome row was asked differently: kept for the
  # models that compare a child's waves.
  series <- exceptions[
    exceptions$action %in% "own_series",
    c("variable_name", "wave", "reason")
  ]
  if (!all(series$wave %in% as.character(base$wave))) {
    stop(
      "Dataset ",
      info$dataset_id[1],
      ": an `own_series` exception names a wave the dataset does not have.",
      call. = FALSE
    )
  }
  exposures <- prepare_exposures(
    kept,
    base$wave,
    base$unit,
    exposure_registry,
    settings,
    total_partial = total_status %in% "partial",
    dedicated_waves = exposure_exceptions$wave[
      exposure_exceptions$action %in% "total_dedicated"
    ],
    parts = parts[c("variable_name", "wave")],
    left_out = exposure_exceptions[
      exposure_exceptions$action %in% "exclude",
    ]
  )
  outcomes <- prepare_outcomes(
    kept,
    base,
    outcome_registry,
    derivations,
    dataschema,
    exceptions,
    settings
  )
  if (!all(series$variable_name %in% names(outcomes$outcomes))) {
    stop(
      "Dataset ",
      info$dataset_id[1],
      ": an `own_series` exception names an outcome the dataset does not ",
      "have.",
      call. = FALSE
    )
  }

  screen <- exposure_registry$variable_name[
    exposure_registry$unit %in% c("hours", "index")
  ]
  blocks <- all_rows[!primary, ]
  measure_columns <- intersect(screen, names(blocks))
  measure_columns <- measure_columns[vapply(
    blocks[measure_columns],
    function(x) any(!is.na(x)),
    logical(1)
  )]
  measures <- blocks[c(
    "participant_id",
    "wave",
    "st_measure_id",
    intersect(
      exposure_registry$variable_name[exposure_registry$axis %in% "metadata"],
      names(blocks)
    ),
    measure_columns
  )]
  # The extra blocks follow the row rules too: only occasions in the base
  # table, and no hours over the plausibility limit.
  key <- function(tbl) {
    paste(tbl$participant_id, dplyr::coalesce(as.character(tbl$wave), ""))
  }
  measures <- measures[key(measures) %in% key(base), ]
  hours <- exposure_registry$variable_name[exposure_registry$unit %in% "hours"]
  for (column in intersect(hours, names(measures))) {
    values <- as.numeric(measures[[column]])
    values[!is.na(values) & values > settings$exposure_max_hours] <- NA_real_
    measures[[column]] <- values
  }
  # A recall block asks about an earlier period, so it stays out of analyses
  # of concurrent screen use.
  measures$concurrent <- !measures$st_measure_id %in% "recall"

  row_audit <- dplyr::bind_rows(exposures$audit, outcomes$audit)
  row_audit$dataset_id <- rep(dataset_id, nrow(row_audit))

  list(
    base = base,
    exposures = exposures$exposures,
    outcomes = outcomes$outcomes,
    outcomes_z = outcomes$outcomes_z,
    measures = measures,
    row_audit = row_audit,
    dropped = prepared$dropped,
    wave_order_declared = isTRUE(nzchar(info[["wave_order"]])),
    repeats = outcomes$repeats,
    nodes = outcomes$nodes,
    derived = outcomes$derived,
    series = series
  )
}
