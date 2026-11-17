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
#'   (`series_breaks.csv`), or where the base table names the outcome's
#'   instrument and it differs; and a child is left out of a pair where
#'   their screen use was reported by a different respondent or in a
#'   different response format at the two waves (`n_switched`). Most
#'   outcome rows have no instrument column, and for those a change of
#'   instrument between waves is not detected.
#'
#' A pair is a pseudo-unit: one unit of the earlier wave (so one country)
#' and that unit's next wave, on the children of the unit seen again. The
#' next wave is found unit by unit: the first later wave at which at least
#' the minimum number of the unit's children have what the model needs,
#' which is the outcome for the lagged and matched models and the outcome
#' and screen use for the within-child model. So the within-child model's
#' pairs can span more than the others', and a country or cohort that
#' lacks an item at one wave is paired with the wave after. A unit with no
#' such later wave has a row that says so. Each row records the typical
#' gap between the waves in years (`lag_years`): the mean gap in the
#' children's ages where ages are recorded finely, otherwise the mean gap
#' in collection years.
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
#'   `matched` or `within`), `wave_from`, `wave_to`, `lag_years`, `n_pairs`
#'   (the unit's children paired with the later wave) and `n_switched`
#'   (those left out of the within-child model for a change of respondent
#'   or format). NULL where the dataset is not a panel or holds none
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
  wave_of <- match(base$wave, waves)
  units_at <- lapply(wave_rows[waves], function(index) unique(base$unit[index]))
  respondent <- column("st_responder")
  format <- column("st_measure_type")
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
    instrument <- column(longitudinal_measure_column(outcome, base))
    # What a row needs at the later wave: the outcome, and for the
    # within-child model screen use too.
    passes <- list(
      list(models = c("lagged", "matched"), later_has = !is.na(z)),
      list(models = "within", later_has = !is.na(z) & !is.na(x))
    )
    for (pass in passes) {
      for (j in seq_len(length(waves) - 1)) {
        for (unit in units_at[[j]]) {
          from <- wave_rows[[waves[j]]]
          from <- from[base$unit[from] %in% unit]
          from <- from[!is.na(x[from]) & !is.na(z[from])]
          if (length(from) < settings$min_unit_rows) {
            next
          }
          # The unit's next wave: the first later one at which enough of
          # its children have what the model needs. It is found unit by
          # unit, so a country or a cohort without the item at a wave is
          # paired with the wave after.
          found <- NULL
          for (later_wave in waves[seq(j + 1, length(waves))]) {
            to <- wave_rows[[later_wave]]
            to <- to[pass$later_has[to]]
            later <- to[match(
              base$participant_id[from],
              base$participant_id[to]
            )]
            if (sum(!is.na(later)) >= settings$min_unit_rows) {
              found <- later_wave
              break
            }
          }
          label <- function(fit, to_wave) {
            fit$unit <- paste(unit, to_wave, sep = " to ")
            fit$wave <- waves[j]
            fit$wave_from <- waves[j]
            fit$wave_to <- to_wave
            fit$exposure <- exposure
            fit$outcome <- outcome
            fit$score_type <- score_type
            fit
          }
          if (is.null(found)) {
            for (version in pass$models) {
              fit <- fit_stage1_unit(frame[0, ], "none", settings)
              fit$status <- "no later wave with enough of the unit's children"
              fit$model <- version
              fit$lag_years <- NA_real_
              fit$n_pairs <- 0L
              fit$n_switched <- NA_integer_
              results[[length(results) + 1]] <- label(fit, NA_character_)
            }
            next
          }
          # The outcome's SD among all of the earlier unit's rows.
          spread <- stats::sd(
            raw[wave_rows[[waves[j]]][
              base$unit[wave_rows[[waves[j]]]] %in% unit
            ]],
            na.rm = TRUE
          )
          paired <- !is.na(later)
          from <- from[paired]
          later <- later[paired]
          pair_waves <- c(waves[j], found)
          changed <- longitudinal_item_changed(
            base$dataset_id[1],
            exposure,
            pair_waves,
            year[pair_waves],
            instrument,
            list(from, later),
            exposure_registry,
            series_breaks
          )
          # A child whose screen use was reported by someone else, or in
          # another format, at the later wave has no change to measure.
          differs <- function(values) {
            if (is.null(values)) {
              return(rep(FALSE, length(from)))
            }
            !is.na(values[from]) &
              !is.na(values[later]) &
              values[from] != values[later]
          }
          switched <- differs(respondent) | differs(format)
          lag_years <- longitudinal_lag(
            base$data_year[later] - base$data_year[from],
            base$age_years[later] - base$age_years[from],
            base$age_years[from]
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
            one <- frame[from, ]
            one$x <- values$x
            one$y <- values$y
            one$y0 <- values$y0
            usable <- !is.na(one$x) & !is.na(one$y) & is.finite(one$y)
            if (version == "within") {
              usable <- usable & !switched
            }
            fit <- if (version == "within" && !is.na(changed)) {
              blocked <- fit_stage1_unit(frame[0, ], "none", settings)
              blocked$status <- changed
              blocked
            } else if (!any(usable)) {
              empty <- fit_stage1_unit(frame[0, ], "none", settings)
              empty$status <- "no child with a change to measure"
              empty
            } else {
              fit_stage1_unit(
                one[usable, ],
                "linear",
                settings,
                extra_terms = if (version == "lagged") "y0" else character()
              )
            }
            fit$model <- version
            fit$lag_years <- lag_years
            fit$n_pairs <- length(from)
            fit$n_switched <- if (version == "within") {
              sum(switched)
            } else {
              NA_integer_
            }
            results[[length(results) + 1]] <- label(fit, found)
          }
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
# finely (more than two distinct fractions of a year at the earlier wave,
# so not whole years or a single mid-year band) it is the mean gap in the
# children's ages, which follows the months between interviews. Otherwise
# it is the mean gap in collection years, and failing that in ages. NA
# where none gives a gap above zero.
longitudinal_lag <- function(year_gap, age_gap, age) {
  mean_of <- function(gap) {
    typical <- mean(gap, na.rm = TRUE)
    if (is.finite(typical) && typical > 0) typical else NA_real_
  }
  fractions <- unique(round(age[!is.na(age)] %% 1, 2))
  candidates <- if (length(fractions) > 2) {
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

# Why a change between two waves cannot be measured for anyone, or NA
# where it can: the screen item changed between them, or the outcome came
# from another instrument. A change of respondent or response format is
# handled child by child, where the pairs are fitted.
longitudinal_item_changed <- function(
  dataset_id,
  exposure,
  waves,
  years,
  instrument,
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
  # The derived total is built from every item, so any change counts; and
  # a usual-day value is built from the weekday and weekend items, so a
  # change to either counts for it.
  if (!is.na(label)) {
    lines <- lines[
      lines$exposure %in%
        label &
        (is.na(lines$day) | lines$day %in% day | day %in% "usual"),
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
  if (!is.null(instrument)) {
    first <- most_common(instrument[rows[[1]]])
    second <- most_common(instrument[rows[[2]]])
    if (!is.na(first) && !is.na(second) && first != second) {
      return("outcome from a different instrument at each wave")
    }
  }
  NA_character_
}
