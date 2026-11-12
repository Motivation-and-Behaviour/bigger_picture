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
#'   informant), for comparisons between measures within a study;
#' - `row_audit`: one row per wave and dataschema row, saying whether it was
#'   used and, if not, why, so that no row disappears silently;
#' - `dropped`: rows dropped per wave and reason;
#' - `repeats`: outcome rows found to repeat another row's score, and
#'   `derived`: the derivation rules that ran.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param outcome_registry,exposure_registry,derivations The registries.
#' @param dataschema The dataschema.
#' @param exceptions The dataset's analysis exceptions.
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
  settings = analysis_settings()
) {
  all_rows <- included_rows(harmonised, info)
  if (is.null(all_rows)) {
    return(NULL)
  }
  dataset_id <- as.character(info$dataset_id)
  primary <- all_rows$st_measure_id %in% "primary"
  rows <- all_rows[primary, ]

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
  kept <- rows[prepared$keep, ]

  exposures <- prepare_exposures(kept, base$wave, exposure_registry, settings)
  outcomes <- prepare_outcomes(
    kept,
    base,
    outcome_registry,
    derivations,
    dataschema,
    exceptions,
    settings
  )

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
    repeats = outcomes$repeats,
    derived = outcomes$derived
  )
}
