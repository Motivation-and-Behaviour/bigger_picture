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
    # The one-stage check's cell before the freeze, and the gap between the
    # two approaches, on the contrast of 1 against 4 hours a day, that
    # prompts a look: half the smallest effect of interest.
    onestage_cell = c(exposure = "st_watch", outcome = "wb_selfesteem_self"),
    onestage_trigger = 0.05,
    # Two outcome rows are a repeat of one score when every co-observed value
    # is equal, on at least this many rows.
    repeat_min_rows = 10L
  )
}
