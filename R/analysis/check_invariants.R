#' Check the rows the harmonisation guide carries within a participant
#'
#' Checks the carry-forward and fills nothing (ANALYSIS_PLAN.md, Step 3).
#' `Unknown` counts as missing. Two things can go wrong, and which one applies
#' depends on the row's `rule` in `invariants.csv`:
#'
#' - a `constant` row (sex, and design values fixed at sampling) must take
#'   one value per participant, so more than one is a fault;
#' - a `carried` row keeps what was observed and fills missing waves from the
#'   earliest value, so a participant may rightly hold two answers, but must
#'   not have a value at one wave and none at another;
#' - an `onset` row is carried except to waves before onset, where it is
#'   rightly missing, so neither test applies and both counts are context.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param invariants The table from `invariants.csv`.
#' @return A tibble with one row per invariant that has values: participants
#'   with a value, seen at two or more waves, with more than one distinct
#'   value, and with a value at some waves but not others. NULL when the
#'   dataset is not included.
check_invariants <- function(harmonised, info, invariants) {
  rows <- included_rows(harmonised, info, primary_only = TRUE)
  if (is.null(rows)) {
    return(NULL)
  }
  variables <- intersect(invariants$variable_name, names(rows))
  participant <- as.character(rows$participant_id)
  # Occasions per participant, counted once. Grouping is by data.table
  # throughout: a table of two million participants is too slow otherwise.
  occasions <- data.table::data.table(id = participant)[
    !is.na(participant),
    list(seen = .N),
    by = "id"
  ]
  any_repeated <- nrow(occasions) > 0 && max(occasions$seen) > 1L

  check_one <- function(variable) {
    values <- as.character(rows[[variable]])
    keep <- !is.na(values) & values != "Unknown" & !is.na(participant)
    if (!any(keep)) {
      return(NULL)
    }
    dt <- data.table::data.table(id = participant[keep], value = values[keep])
    if (!any_repeated) {
      # One occasion each: nothing can vary or have a gap.
      return(tibble::tibble(
        variable_name = variable,
        participants = data.table::uniqueN(dt$id),
        participants_repeated = 0L,
        participants_varying = 0L,
        participants_gap = 0L
      ))
    }
    with_value <- dt[, list(n = .N), by = "id"]
    distinct <- unique(dt)[, list(distinct = .N), by = "id"]
    per_person <- merge(with_value, distinct, by = "id")
    per_person <- merge(per_person, occasions, by = "id")
    tibble::tibble(
      variable_name = variable,
      participants = nrow(per_person),
      participants_repeated = sum(per_person$seen > 1L),
      participants_varying = sum(per_person$distinct > 1L),
      participants_gap = sum(per_person$n < per_person$seen)
    )
  }

  out <- dplyr::bind_rows(lapply(variables, check_one))
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$dataset_id <- as.character(info$dataset_id)
  dplyr::relocate(out, "dataset_id")
}
