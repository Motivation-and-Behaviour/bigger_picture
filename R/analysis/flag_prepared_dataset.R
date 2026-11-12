#' Flag what needs a look in a prepared dataset
#'
#' Only what a person should check (ANALYSIS_PLAN.md, Step 4); each flag ends
#' in an analysis exception, a harmonisation fix or no action.
#'
#' - `derive_at_analysis`: a mapping note says a row is derived at analysis,
#'   but no rule produced it.
#' - `repeat_across_nodes`: two outcome rows hold the same score but sit in
#'   different nodes, or in none, so neither was dropped.
#' - `near_identical`: two outcome rows correlate above 0.98 within the
#'   dataset and the registry does not say one is part of the other.
#' - `measure_changes`: an instrument (`*_measure`) row takes more than one
#'   value within a wave, so one score row may hold two instruments.
#' - `out_of_bounds`: more than 5% of a row's values fell outside the
#'   schema's bounds and were dropped, which suggests a different score form.
#' - `total_incomplete`: more than half the rows that have the required
#'   activity rows lose their summed total to another activity row's
#'   missingness.
#' - `rows_dropped`: more than a fifth of the dataset's rows were dropped.
#'
#' @param prepared Output of `prepare_analysis_data()`.
#' @param variables The dataset's `variables.csv` mapping table.
#' @param outcome_registry The outcome registry.
#' @return A tibble of `dataset_id`, `flag`, `variable_name` and `detail`,
#'   with no rows when nothing needs a look; NULL when not included.
flag_prepared_dataset <- function(prepared, variables, outcome_registry) {
  if (is.null(prepared)) {
    return(NULL)
  }
  flags <- list()
  flag <- function(name, variable, detail) {
    tibble::tibble(flag = name, variable_name = variable, detail = detail)
  }
  audit <- prepared$row_audit
  registry <- outcome_registry$rows
  summary <- summarise_prepared(prepared)

  noted <- variables$target_variable[
    grepl("at analysis", variables$notes, ignore.case = TRUE)
  ]
  noted <- intersect(
    noted,
    registry$variable_name[registry$role %in% "outcome"]
  )
  # A row the audit accounts for (used, derived or dropped as a repeat) needs
  # no flag.
  for (variable in setdiff(noted, audit$variable_name)) {
    flags[[length(flags) + 1]] <- flag("derive_at_analysis", variable, "noted in the mapping; no rule ran")
  }

  across <- prepared$repeats[!prepared$repeats$drop, ]
  across <- dplyr::distinct(across, .data$specific, .data$keep)
  for (i in seq_len(nrow(across))) {
    flags[[length(flags) + 1]] <- flag(
      "repeat_across_nodes",
      across$specific[i],
      paste0("same score as ", across$keep[i])
    )
  }

  z <- prepared$outcomes_z
  if (ncol(z) > 1 && nrow(z) > 0) {
    part_of <- stats::setNames(registry$part_of, registry$variable_name)
    is_part <- function(a, b) {
      b %in% strsplit(dplyr::coalesce(part_of[[a]], ""), "|", fixed = TRUE)[[1]]
    }
    correlation <- suppressWarnings(
      stats::cor(as.matrix(z), use = "pairwise.complete.obs")
    )
    pairs <- which(abs(correlation) > 0.98 & upper.tri(correlation), arr.ind = TRUE)
    for (i in seq_len(nrow(pairs))) {
      a <- colnames(correlation)[pairs[i, 1]]
      b <- colnames(correlation)[pairs[i, 2]]
      both <- sum(!is.na(z[[a]]) & !is.na(z[[b]]))
      if (both >= 30 && !is_part(a, b) && !is_part(b, a)) {
        flags[[length(flags) + 1]] <- flag(
          "near_identical",
          a,
          paste0("r = ", signif(correlation[a, b], 3), " with ", b)
        )
      }
    }
  }

  base <- prepared$base
  measure_rows <- grep("_measure$", names(base), value = TRUE)
  # These two are filled per row by design, so they vary within a wave.
  measure_rows <- setdiff(
    measure_rows,
    c("ses_household_measure", "hlth_bmi_measure")
  )
  for (variable in measure_rows) {
    per_wave <- tapply(
      base[[variable]],
      dplyr::coalesce(base$wave, ""),
      function(x) dplyr::n_distinct(x, na.rm = TRUE)
    )
    if (any(per_wave > 1)) {
      flags[[length(flags) + 1]] <- flag(
        "measure_changes",
        variable,
        paste0("up to ", max(per_wave), " instruments within one wave")
      )
    }
  }

  bounds <- audit[grepl("outside the schema's bounds", audit$detail), ]
  for (i in seq_len(nrow(bounds))) {
    dropped <- bounds$n_values[i] - bounds$n_used[i]
    if (dropped / bounds$n_values[i] > 0.05) {
      flags[[length(flags) + 1]] <- flag(
        "out_of_bounds",
        bounds$variable_name[i],
        paste0(
          dropped,
          " of ",
          bounds$n_values[i],
          " values dropped in wave ",
          bounds$wave[i]
        )
      )
    }
  }

  lost <- summary$total_lost_to_incomplete
  if (lost > summary$total_summed) {
    flags[[length(flags) + 1]] <- flag(
      "total_incomplete",
      "total_screen",
      paste0(lost, " rows have watching and gaming but no summed total")
    )
  }
  dropped <- sum(prepared$dropped$n)
  if (dropped > 0.2 * (dropped + summary$rows_kept)) {
    flags[[length(flags) + 1]] <- flag(
      "rows_dropped",
      NA_character_,
      paste0(dropped, " of ", dropped + summary$rows_kept, " rows dropped")
    )
  }

  out <- dplyr::bind_rows(
    tibble::tibble(
      flag = character(),
      variable_name = character(),
      detail = character()
    ),
    dplyr::bind_rows(flags)
  )
  out$dataset_id <- rep(summary$dataset_id, nrow(out))
  dplyr::relocate(out, "dataset_id")
}
