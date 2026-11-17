#' Stage 1 of the longitudinal strand: models of children seen twice
#'
#' For each exposure and outcome cell, and each pair of successive waves of
#' a dataset in which the outcome was measured, three models are fitted on
#' children seen at both waves (ANALYSIS_PLAN.md, Step 13; DR-0018).
#' Each gives a slope in SD per hour a day, with the covariates, weights and
#' standard errors of `fit_stage1_unit()` taken from the earlier wave:
#'
#' - `lagged`: the outcome at the later wave on screen use at the earlier
#'   wave, adjusted for the outcome at the earlier wave. Both outcomes are
#'   standardised within their own wave, as in the cross-sectional models,
#'   so a change of instrument between waves does not matter;
#' - `matched`: the cross-sectional model at the earlier wave on the rows
#'   the lagged model uses, so the two can be compared on the same
#'   children;
#' - `within`: the change in the outcome on the change in screen use, in
#'   hours, between the two waves, which removes every difference between
#'   children that does not change. The change in the outcome is in its SD
#'   in the earlier unit: scores standardised within each wave would lose
#'   the change in the mean. It also needs screen use at the later wave, so
#'   it has fewer children than the other two, and it needs the same item
#'   at both waves: a pair is left out where the screen item changed
#'   (`series_breaks.csv`, or a different response format or respondent),
#'   or where the base table names the outcome's instrument and it
#'   differs. Most outcome rows have no such column, and for those a change
#'   of instrument between waves is not detected.
#'
#' A pair is a pseudo-unit: one unit of the earlier wave (so one country)
#' and the later wave, on the children of that unit seen again. The lagged
#' and matched models pair successive waves that have the outcome; the
#' within-child model pairs successive waves that have the outcome and
#' screen use, so its pairs can span more than theirs. Each row records
#' the typical gap between the waves in years (`lag_years`): the mean gap
#' in the children's ages where ages are recorded finely, otherwise the
#' mean gap in collection years.
#'
#' Limits: only outcomes modelled linearly are fitted (continuous rows, and
#' ordinal rows with five or more levels); a wave with no screen item is
#' not in the data, so a lag is the gap between waves that have one; and
#' children who left the study before the later wave are not seen, so
#' attrition is not accounted for.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param cells A tibble of `exposure` and `outcome` rows to fit.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @param dataschema The dataschema.
#' @param series_breaks From `read_series_breaks()`.
#' @param settings From `analysis_settings()`.
#' @return A tibble shaped like `fit_stage1()`'s with `model` (`lagged`,
#'   `matched` or `within`), `wave_from`, `wave_to`, `lag_years` and
#'   `n_pairs` (children with the outcome at both waves and screen use at
#'   the earlier one). NULL where the dataset is not a panel or holds none
#'   of the cells.
fit_stage1_longitudinal <- function(
  prepared,
  cells,
  outcome_registry,
  exposure_registry,
  dataschema,
  series_breaks,
  settings = default_analysis_settings()
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  base <- prepared$base
  cells <- cells[
    cells$exposure %in%
      names(prepared$exposures) &
      cells$outcome %in% names(prepared$outcomes),
  ]
  occasion <- paste(base$participant_id, base$wave, sep = "\r")
  repeated <- anyDuplicated(base$participant_id) > 0
  if (nrow(cells) == 0 || !repeated || anyNA(base$wave)) {
    return(NULL)
  }
  if (anyDuplicated(occasion) > 0) {
    stop(
      "fit_stage1_longitudinal(): a participant has two rows in one wave.",
      call. = FALSE
    )
  }
  registry <- outcome_registry$rows
  frame <- stage1_frame(base)
  waves <- dplyr::distinct(base[c("wave", "wave_order")])
  waves <- waves$wave[order(waves$wave_order)]
  wave_rows <- split(seq_len(nrow(base)), base$wave)
  year <- vapply(
    wave_rows,
    function(index) stats::median(base$data_year[index], na.rm = TRUE),
    numeric(1)
  )
  column <- function(name) {
    if (name %in% names(base)) as.character(base[[name]]) else NULL
  }

  results <- list()
  for (i in seq_len(nrow(cells))) {
    exposure <- cells$exposure[i]
    outcome <- cells$outcome[i]
    score_type <- registry$score_type[match(outcome, registry$variable_name)]
    model <- stage1_model_type(
      score_type,
      schema_level_count(outcome, dataschema)
    )
    if (model != "linear") {
      next
    }
    x <- prepared$exposures[[exposure]]
    raw <- as.numeric(prepared$outcomes[[outcome]])
    z <- if (outcome %in% names(prepared$outcomes_z)) {
      prepared$outcomes_z[[outcome]]
    } else {
      standardise_within(raw, base$unit, settings$min_unit_rows)
    }
    # The lagged and matched models pair successive waves that have the
    # outcome. The within-child model also needs screen use at the later
    # wave, so it pairs successive waves that have both: an outcome
    # measured more often than screen use would otherwise never be in it.
    has_outcome <- waves[vapply(
      waves,
      function(wave) any(!is.na(z[wave_rows[[wave]]])),
      logical(1)
    )]
    has_both <- waves[vapply(
      waves,
      function(wave) {
        rows <- wave_rows[[wave]]
        any(!is.na(z[rows]) & !is.na(x[rows]))
      },
      logical(1)
    )]
    passes <- list(
      list(measured = has_outcome, models = c("lagged", "matched")),
      list(measured = has_both, models = "within")
    )
    for (pass in passes) {
      measured <- pass$measured
      steps <- seq_len(max(length(measured) - 1, 0))
      # One pair per unit of the earlier wave, so a wave's countries stay
      # apart as they do in the cross-sectional models.
      pairs <- do.call(
        rbind,
        lapply(steps, function(j) {
          units <- unique(base$unit[wave_rows[[measured[j]]]])
          data.frame(j = rep(j, length(units)), unit = units)
        })
      )
      for (k in seq_len(NROW(pairs))) {
        j <- pairs$j[k]
        from <- wave_rows[[measured[j]]]
        from <- from[base$unit[from] %in% pairs$unit[k]]
        to <- wave_rows[[measured[j + 1]]]
        later <- to[match(base$participant_id[from], base$participant_id[to])]
        paired <- !is.na(later) &
          !is.na(x[from]) &
          !is.na(z[from]) &
          !is.na(z[later])
        if ("within" %in% pass$models) {
          paired <- paired & !is.na(x[later])
        }
        if (!any(paired)) {
          next
        }
        # The outcome's SD among all of the earlier unit's rows.
        spread <- stats::sd(raw[from], na.rm = TRUE)
        from <- from[paired]
        later <- later[paired]
        data <- frame[from, ]
        changed <- longitudinal_item_changed(
          base$dataset_id[1],
          exposure,
          measured[j + c(0, 1)],
          year[measured[j + c(0, 1)]],
          list(
            responder = column("st_responder"),
            format = column("st_measure_type"),
            instrument = column(longitudinal_measure_column(outcome, base))
          ),
          list(from, later),
          exposure_registry,
          series_breaks
        )
        versions <- list(
          lagged = list(x = x[from], y = z[later], y0 = z[from]),
          matched = list(x = x[from], y = z[from]),
          within = list(
            x = x[later] - x[from],
            y = (raw[later] - raw[from]) / spread
          )
        )
        for (version in pass$models) {
          values <- versions[[version]]
          one <- data
          one$x <- values$x
          one$y <- values$y
          one$y0 <- values$y0
          usable <- !is.na(one$x) & !is.na(one$y) & is.finite(one$y)
          fit <- if (version == "within" && !is.na(changed)) {
            fit_stage1_unit(one[0, ], "none", settings)
          } else if (!any(usable)) {
            next
          } else {
            fit_stage1_unit(
              one[usable, ],
              "linear",
              settings,
              extra_terms = if (version == "lagged") "y0" else character()
            )
          }
          if (version == "within" && !is.na(changed)) {
            fit$status <- changed
          }
          fit$model <- version
          fit$unit <- paste(pairs$unit[k], measured[j + 1], sep = " to ")
          fit$wave <- measured[j]
          fit$wave_from <- measured[j]
          fit$wave_to <- measured[j + 1]
          fit$lag_years <- longitudinal_lag(
            base$data_year[later] - base$data_year[from],
            base$age_years[later] - base$age_years[from],
            base$age_years[from]
          )
          fit$n_pairs <- length(from)
          fit$exposure <- exposure
          fit$outcome <- outcome
          fit$score_type <- score_type
          results[[length(results) + 1]] <- fit
        }
      }
    }
  }
  out <- dplyr::bind_rows(results)
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$dataset_id <- as.character(base$dataset_id[1])
  dplyr::relocate(
    out,
    "dataset_id",
    "unit",
    "model",
    "wave_from",
    "wave_to",
    "lag_years",
    "exposure",
    "outcome",
    "score_type"
  )
}

# The typical gap between two waves in years. Where ages are recorded
# finely (more than ten distinct values at the earlier wave) it is the mean
# gap in the children's ages, which follows the months between interviews.
# Otherwise it is the mean gap in collection years, and failing that in
# ages. NA where none gives a gap above zero.
longitudinal_lag <- function(year_gap, age_gap, age) {
  mean_of <- function(gap) {
    typical <- mean(gap, na.rm = TRUE)
    if (is.finite(typical) && typical > 0) typical else NA_real_
  }
  fine <- dplyr::n_distinct(age, na.rm = TRUE) > 10
  candidates <- if (fine) {
    c(mean_of(age_gap), mean_of(year_gap))
  } else {
    c(mean_of(year_gap), mean_of(age_gap))
  }
  candidates[!is.na(candidates)][1]
}

# The base column naming an outcome row's instrument, where there is one:
# the `_measure` column whose stem starts the row's name, the longest if
# several do.
longitudinal_measure_column <- function(outcome, base) {
  columns <- grep("_measure$", names(base), value = TRUE)
  stems <- sub("_measure$", "", columns)
  fits <- columns[startsWith(outcome, paste0(stems, "_")) | stems == outcome]
  if (length(fits) == 0) {
    return("")
  }
  fits[which.max(nchar(fits))]
}

# Why a change between two waves cannot be measured, or NA where it can:
# the screen item changed between them, or was answered in another format
# or by another respondent, or the outcome came from another instrument.
longitudinal_item_changed <- function(
  dataset_id,
  exposure,
  waves,
  years,
  labels,
  rows,
  exposure_registry,
  series_breaks
) {
  label <- exposure_registry$exposure[match(
    exposure,
    exposure_registry$variable_name
  )]
  day <- exposure_registry$day[match(exposure, exposure_registry$variable_name)]
  lines <- series_breaks[series_breaks$dataset_id %in% dataset_id, ]
  # The derived total is built from every item, so any change counts.
  if (!is.na(label)) {
    lines <- lines[
      lines$exposure %in% label & (is.na(lines$day) | lines$day %in% day),
    ]
  }
  units <- data.frame(wave = waves, year = unname(years))
  for (k in seq_len(nrow(lines))) {
    side <- series_break_applies(lines[k, ], units)
    if (side[1] != side[2]) {
      return("screen item changed between the waves")
    }
  }
  most_common <- function(values) {
    values <- values[!is.na(values)]
    if (length(values) == 0) NA_character_ else names(which.max(table(values)))
  }
  reasons <- c(
    responder = "screen use reported by a different respondent at each wave",
    format = "screen item in a different response format at each wave",
    instrument = "outcome from a different instrument at each wave"
  )
  for (what in names(reasons)) {
    values <- labels[[what]]
    if (is.null(values)) {
      next
    }
    first <- most_common(values[rows[[1]]])
    second <- most_common(values[rows[[2]]])
    if (!is.na(first) && !is.na(second) && first != second) {
      return(unname(reasons[what]))
    }
  }
  NA_character_
}
