#' Settings every analysis step shares
#'
#' Kept in one place so a seed or a threshold is never set twice
#' (ANALYSIS_PLAN.md, Pipeline conventions). The thresholds are provisional
#' until the freeze.
#'
#' @return A named list.
analysis_settings <- function() {
  list(
    seed = 20261130L,
    # Rows aged under this are eligible (DR-0012).
    age_limit = 19,
    # Screen use above this many hours a day is implausible (DR-0011).
    exposure_max_hours = 24,
    # A stage 1 unit needs at least this many rows (provisional).
    min_unit_rows = 30L,
    # A cell is pooled from this many datasets (DR-0010).
    min_datasets = 3L,
    # A categorical column with more distinct values than this is treated as
    # an identifier and not tabulated.
    max_levels = 200L,
    # Age bands for the NSW school stages, lower bound included.
    school_stage_breaks = c(-Inf, 5, 8, 12, 16, Inf),
    school_stage_labels = c(
      "Before school",
      "Early Stage 1 to Stage 1",
      "Stages 2-3",
      "Stages 4-5",
      "Stage 6"
    ),
    # A derived total needs these activity rows at minimum (DR-0013).
    total_required = c("st_watch", "st_game"),
    # A content-class row enters a unit's sum when it is present on at least
    # this share of the unit's rows that have the required rows.
    total_row_min_share = 0.5,
    # A part is dropped for its whole when the whole is observed on at least
    # this share of the rows where the part is.
    part_cover_min = 0.9,
    # A second value needs this many rows before two rows can be called a
    # repeat of one score.
    repeat_min_minority = 5L,
    # Sex is a covariate where it is known for at least this share of a
    # unit's rows; below that it is left out and the rows stay.
    sex_known_min = 0.9,
    # A covariate of a further tier counts as recorded in a unit where it is
    # known for at least this share of the unit's rows, until missing
    # values are imputed (Step 8).
    covariate_known_min = 0.9,
    # Cluster-robust standard errors from fewer clusters than this are
    # flagged: they tend to be too small.
    min_clusters = 10L,
    # A trend over collection years needs this many datasets, spanning this
    # many years.
    year_min_datasets = 10L,
    year_min_span = 10,
    # The one-stage check's cell before the freeze, and the gap between the
    # two approaches, on the contrast of 1 against 4 hours a day, that
    # prompts a look: half the smallest effect of interest.
    onestage_cell = c(exposure = "st_watch", outcome = "wb_selfesteem_self"),
    onestage_trigger = 0.05,
    # Two outcome rows are a repeat of one score when every co-observed value
    # is equal, on at least this many rows.
    repeat_min_rows = 10L,
    # A log odds ratio divided by this joins the SD scale when a binary or
    # short ordinal row is pooled into its specific outcome and family: the
    # SD of the standard logistic distribution, pi / sqrt(3).
    log_odds_per_sd = pi / sqrt(3),
    # The correlation between two outcome rows' slopes in one unit, where
    # the unit's rows give no estimate of it (provisional).
    tree_default_correlation = 0.6,
    # The smallest effect of interest in SD per hour a day: 0.10 SD on the
    # contrast of 1 against 4 hours (provisional until the freeze).
    smallest_effect_per_hour = 0.10 / 3,
    # Curves (ANALYSIS_PLAN.md, Step 11): a natural cubic spline of screen
    # use with knots fixed in hours, so every unit has the same curve terms;
    # the hours a pooled curve is read at; and the low-use reference each
    # hour is compared with. A threshold is the lowest duration whose
    # interval lies beyond `smallest_effect` SD from the reference
    # (provisional until the freeze).
    curve_knots = c(1, 2, 4),
    curve_boundary = c(0, 6),
    curve_grid = seq(0, 8, by = 0.5),
    curve_reference = 1,
    # A unit's curve needs this many rows in each stretch between knots, so
    # no part of it rests on a handful of children; and a unit supports an
    # hour where it has the minimum number of rows within this many hours
    # of it.
    curve_interval_rows = 30L,
    curve_window = 0.5,
    smallest_effect = 0.10,
    # Multiple imputation (ANALYSIS_PLAN.md, Step 8; DR-0019). The number
    # of imputations is small while the engine is developed, and 50 for
    # the confirmatory run.
    imputations = 5L,
    imputation_iterations = 10L,
    # A variable is imputed in a unit where it is known for at least this
    # share of the unit's rows; below that its missingness is not sporadic
    # (DR-0019).
    impute_min_observed = 0.7,
    # And for a child only where it is known for at least this share of
    # the unit's children of the same age in whole years: an item not
    # asked of an age group is not imputed for it.
    impute_min_observed_age = 0.5,
    # The kinds of stage 1 model whose exposure is imputed.
    impute_exposure_models = c("linear", "logistic", "ordinal"),
    # A predictor enters a variable's imputation model where the two
    # correlate at least this much.
    impute_min_correlation = 0.05
  )
}

#' The settings a function falls back on when it is given none
#'
#' Functions take their settings as an argument, and the pipeline always
#' passes the few each stage reads (`bp_prepare_settings()` and the like),
#' so that changing one setting reruns only the stages that read it. A
#' default of `analysis_settings()` in a function's arguments would undo
#' that: `targets` counts every function a function names as something it
#' depends on, so adding or changing any setting would rerun everything.
#' The default therefore goes through this function, which finds the
#' settings by name, where `targets` does not follow.
#'
#' That is only safe because no call inside the pipeline relies on the
#' default: `tests/testthat/test-analysis-settings.R` fails if a function
#' that takes settings is called without them.
#'
#' @return The list from `analysis_settings()`.
default_analysis_settings <- function() {
  get("analysis_settings", mode = "function")()
}
