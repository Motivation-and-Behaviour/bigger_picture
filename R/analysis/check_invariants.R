#' Check that declared invariants hold within participants of one dataset
#'
#' The harmonisation guide carries some rows within a participant (sex,
#' ethnicity, early-life rows, design variables), so they should take one
#' value per participant. This checks the carry-forward and fills nothing
#' (ANALYSIS_PLAN.md, Step 3). `Unknown` counts as missing.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param invariants The table from `invariants.csv`.
#' @return A tibble with one row per invariant that has values: the number of
#'   participants with a value, with a value at two or more waves, and with
#'   more than one distinct value. NULL when the dataset is not included.
check_invariants <- function(harmonised, info, invariants) {
  rows <- included_rows(harmonised, info, primary_only = TRUE)
  if (is.null(rows)) {
    return(NULL)
  }
  variables <- intersect(invariants$variable_name, names(rows))
  participant <- as.character(rows$participant_id)

  check_one <- function(variable) {
    values <- as.character(rows[[variable]])
    keep <- !is.na(values) & values != "Unknown" & !is.na(participant)
    if (!any(keep)) {
      return(NULL)
    }
    dt <- data.table::data.table(id = participant[keep], value = values[keep])
    per_person <- dt[,
      list(n = .N, distinct = data.table::uniqueN(value)),
      by = "id"
    ]
    tibble::tibble(
      variable_name = variable,
      participants = nrow(per_person),
      participants_repeated = sum(per_person$n > 1L),
      participants_varying = sum(per_person$distinct > 1L)
    )
  }

  out <- dplyr::bind_rows(lapply(variables, check_one))
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$dataset_id <- as.character(info$dataset_id)
  dplyr::relocate(out, "dataset_id")
}
