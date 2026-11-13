#' Flag what needs a look in a prepared dataset
#'
#' Only what a person should check (ANALYSIS_PLAN.md, Step 4). A `review`
#' flag ends in an analysis exception, a harmonisation fix or no action; an
#' `info` flag is context for whoever signs the dataset off.
#'
#' - `derive_at_analysis`: a mapping note begins "Derive at analysis", but
#'   no rule produced the row.
#' - `repeat_across_nodes`: two outcome rows hold the same score but sit in
#'   different nodes, or in none, so neither was dropped.
#' - `near_identical`: two outcome rows correlate above 0.98 within the
#'   dataset and the registry does not say one is part of the other.
#' - `measure_changes`: an instrument (`*_measure`) row takes more than one
#'   value within a wave, so one score row may hold two instruments.
#' - `out_of_bounds`: more than 5% of a row's values fell outside the
#'   schema's bounds and were dropped, which suggests a different score form.
#' - `total_incomplete`: a unit with a summed total loses more than a quarter
#'   of the rows that have watching and gaming.
#' - `weights_partial`: a unit has survey weights on some rows only, so a
#'   weighted model would drop the rest.
#' - `wave_order_tie`: two waves share a median collection year, so their
#'   order rests on median age, unless `datasets.csv` declares the order.
#' - `rows_dropped`: more than a fifth of the dataset's rows were dropped.
#' - `partial_rows` (info): outcome rows mapped as partial or proximate that
#'   are used.
#' - `small_units` (info): units below the minimum size, which no model fits.
#'
#' @param prepared Output of `prepare_analysis_data()`.
#' @param variables The dataset's `variables.csv` mapping table.
#' @param outcome_registry The outcome registry.
#' @param settings From `analysis_settings()`.
#' @return A tibble of `dataset_id`, `level`, `flag`, `variable_name` and
#'   `detail`, with no rows when nothing needs a look; NULL when not included.
flag_prepared_dataset <- function(
  prepared,
  variables,
  outcome_registry,
  settings = analysis_settings()
) {
  if (is.null(prepared)) {
    return(NULL)
  }
  flags <- list()
  flag <- function(name, variable, detail, level = "review") {
    tibble::tibble(
      level = level,
      flag = name,
      variable_name = variable,
      detail = detail
    )
  }
  audit <- prepared$row_audit
  registry <- outcome_registry$rows
  summary <- summarise_prepared(prepared)

  base <- prepared$base
  exposures <- prepared$exposures
  noted <- variables$target_variable[
    grepl("^derive at analysis", variables$notes, ignore.case = TRUE)
  ]
  # A row the audit accounts for needs no flag, nor does one the base table
  # derives, nor one with no rule yet for any dataset.
  accounted <- c(audit$variable_name, "fam_siblings_any", bp_underived_rows())
  for (variable in setdiff(noted, accounted)) {
    flags[[length(flags) + 1]] <- flag(
      "derive_at_analysis",
      variable,
      "noted in the mapping; no rule ran"
    )
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
    pairs <- which(
      abs(correlation) > 0.98 & upper.tri(correlation),
      arr.ind = TRUE
    )
    for (i in seq_len(nrow(pairs))) {
      a <- colnames(correlation)[pairs[i, 1]]
      b <- colnames(correlation)[pairs[i, 2]]
      both <- sum(!is.na(z[[a]]) & !is.na(z[[b]]))
      reported <- any(
        (across$specific == a & across$keep == b) |
          (across$specific == b & across$keep == a)
      )
      if (both >= 30 && !reported && !is_part(a, b) && !is_part(b, a)) {
        flags[[length(flags) + 1]] <- flag(
          "near_identical",
          a,
          paste0("r = ", signif(correlation[a, b], 3), " with ", b)
        )
      }
    }
  }

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

  required <- intersect(settings$total_required, names(exposures))
  if (length(required) == length(settings$total_required) && nrow(base) > 0) {
    could <- exposures$total_source %in%
      "summed" &
      stats::complete.cases(exposures[required])
    by_unit <- dplyr::summarise(
      tibble::tibble(
        unit = base$unit,
        could = could,
        lost = could & is.na(exposures$total_screen)
      ),
      could = sum(.data$could),
      lost = sum(.data$lost),
      .by = "unit"
    )
    hit <- by_unit[
      by_unit$could >= settings$min_unit_rows &
        by_unit$lost > 0.25 * by_unit$could,
    ]
    if (nrow(hit) > 0) {
      flags[[length(flags) + 1]] <- flag(
        "total_incomplete",
        "total_screen",
        paste0(
          nrow(hit),
          " units lose ",
          sum(hit$lost),
          " of ",
          sum(hit$could),
          " rows that have watching and gaming"
        )
      )
    }
  }

  if (nrow(base) > 0) {
    weights <- dplyr::summarise(
      tibble::tibble(unit = base$unit, has = !is.na(base$survey_weight_norm)),
      with = sum(.data$has),
      rows = dplyr::n(),
      .by = "unit"
    )
    partly <- weights[weights$with > 0 & weights$with < weights$rows, ]
    if (nrow(partly) > 0) {
      flags[[length(flags) + 1]] <- flag(
        "weights_partial",
        "survey_weight",
        paste0(
          sum(partly$rows - partly$with),
          " rows without a weight in ",
          nrow(partly),
          " units that have weights"
        )
      )
    }

    year <- if ("data_year" %in% names(base)) base$data_year else NA_real_
    waves <- dplyr::summarise(
      tibble::tibble(wave = base$wave, year = as.numeric(year)),
      year = bp_stat(.data$year, stats::median),
      .by = "wave"
    )
    tied <- waves$year[duplicated(waves$year) & !is.na(waves$year)]
    if (length(tied) > 0 && !isTRUE(prepared$wave_order_declared)) {
      flags[[length(flags) + 1]] <- flag(
        "wave_order_tie",
        "wave",
        paste0(
          "waves ",
          paste(waves$wave[waves$year %in% tied], collapse = ", "),
          " share a median collection year"
        )
      )
    }

    sizes <- table(base$unit)
    small <- sum(sizes < settings$min_unit_rows)
    if (small > 0) {
      flags[[length(flags) + 1]] <- flag(
        "small_units",
        NA_character_,
        paste0(
          small,
          " of ",
          length(sizes),
          " units have fewer than ",
          settings$min_unit_rows,
          " rows"
        ),
        level = "info"
      )
    }
  }

  used <- unique(audit$variable_name[
    audit$kind %in% "outcome" & audit$status %in% c("used", "derived")
  ])
  if ("status" %in% names(variables)) {
    weaker <- variables$target_variable[
      variables$status %in% c("partial", "proximate")
    ]
    weaker <- intersect(weaker, used)
    if (length(weaker) > 0) {
      flags[[length(flags) + 1]] <- flag(
        "partial_rows",
        NA_character_,
        paste0(
          length(weaker),
          " of ",
          length(used),
          " outcome rows used are mapped partial or proximate"
        ),
        level = "info"
      )
    }
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
      level = character(),
      flag = character(),
      variable_name = character(),
      detail = character()
    ),
    dplyr::bind_rows(flags)
  )
  out$dataset_id <- rep(summary$dataset_id, nrow(out))
  dplyr::relocate(out, "dataset_id")
}

# Rows the guide leaves to the analysis that have no derivation rule yet, for
# any dataset: reported once, not flagged per dataset.
bp_underived_rows <- function() {
  "hlth_bmi_z"
}
