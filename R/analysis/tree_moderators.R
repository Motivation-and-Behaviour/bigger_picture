#' Attach the stage 2 moderators to aligned slopes
#'
#' Each moderator is a label on a slope, taken from the registries or from
#' the unit the slope was fitted in (ANALYSIS_PLAN.md, Step 7 task 4). A
#' slope a moderator does not apply to has NA for it.
#'
#' - `informant`: who reported the outcome row; NA where the row takes any
#'   respondent (`mixed`) or none is recorded, which are not kinds of
#'   informant to compare.
#' - `reporter`: `same reporter` where the outcome row and the unit's screen
#'   use have one kind of reporter (the child, a parent or a teacher for
#'   both; it cannot tell whether two parent reports came from one adult),
#'   `different reporter` where they differ or either is a test or a device,
#'   and NA where either is mixed, other, unknown or not recorded. A unit's
#'   reporter of screen use is the most common one among its rows.
#' - `measure_type` and `responder`: the unit's screen-use response format
#'   and who reported it.
#' - `instrument`: the outcome row itself, which names the instrument where
#'   the dataschema has a row per instrument.
#' - `score_type`: the outcome row's score type.
#' - `outcome_status` and `exposure_status`: the harmonisation status of the
#'   dataset's mapping of the outcome row and of the exposure row, or
#'   `derived` for a row the analysis computes from other rows, which has no
#'   mapping of its own.
#' - `total_source`: for the derived total only, whether the unit's total is
#'   a dedicated item or a sum.
#' - `risk_of_bias`: the dataset's rating, once rated.
#'
#' @param effects Rows from `tree_effects()`.
#' @param units Per-unit descriptions, as the `units` table of
#'   `describe_dataset()`.
#' @param mapping_status A tibble of `dataset_id`, `target_variable` and
#'   `status`, from `list_mapping_status()`, or NULL.
#' @param datasets The table from `read_analysis_datasets()`, or NULL.
#' @return `effects` with one column per moderator in
#'   `bp_tree_moderators()`.
tree_moderators <- function(
  effects,
  units,
  mapping_status = NULL,
  datasets = NULL
) {
  out <- effects
  unit <- match(out$unit, units$unit)
  out$measure_type <- as.character(units$measure_type[unit])
  out$responder <- as.character(units$responder[unit])
  screen <- unname(c(
    "Self" = "self",
    "Parent or carer" = "parent",
    "Teacher" = "teacher",
    "Device or objective measure" = "device"
  )[out$responder])
  reported_by <- out$informant
  known <- reported_by %in%
    c("self", "parent", "teacher", "task") &
    !is.na(screen)
  out$reporter <- ifelse(
    !known,
    NA_character_,
    ifelse(reported_by == screen, "same reporter", "different reporter")
  )
  out$informant <- ifelse(
    reported_by %in% c("mixed", "not recorded"),
    NA_character_,
    reported_by
  )
  out$instrument <- out$outcome
  status_of <- function(variable) {
    if (is.null(mapping_status)) {
      return(rep(NA_character_, nrow(out)))
    }
    at <- match(
      paste(out$dataset_id, variable),
      paste(mapping_status$dataset_id, mapping_status$target_variable)
    )
    dplyr::coalesce(mapping_status$status[at], "derived")
  }
  out$outcome_status <- status_of(out$outcome)
  out$exposure_status <- status_of(out$exposure)
  out$total_source <- ifelse(
    out$exposure %in% "total_screen",
    as.character(units$total_source[unit]),
    NA_character_
  )
  out$risk_of_bias <- if (is.null(datasets)) {
    rep(NA_character_, nrow(out))
  } else {
    as.character(datasets$risk_of_bias[match(
      out$dataset_id,
      datasets$dataset_id
    )])
  }
  out
}

#' One dataset's harmonisation status for each mapped variable
#'
#' @param variables A dataset's `variables.csv` table.
#' @param dataset_id The dataset's ID.
#' @return A tibble of `dataset_id`, `target_variable` and `status`, for the
#'   primary measure's evaluated rows.
list_mapping_status <- function(variables, dataset_id) {
  primary <- if ("measure" %in% names(variables)) {
    is.na(variables$measure) | variables$measure %in% c("", "primary")
  } else {
    rep(TRUE, nrow(variables))
  }
  keep <- primary & variables$status %in% bp_evaluated_statuses()
  tibble::tibble(
    dataset_id = rep(as.character(dataset_id), sum(keep)),
    target_variable = variables$target_variable[keep],
    status = variables$status[keep]
  )
}
