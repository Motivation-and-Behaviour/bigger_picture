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
    # Two outcome rows are a repeat of one score when every co-observed value
    # is equal, on at least this many rows.
    repeat_min_rows = 10L
  )
}
