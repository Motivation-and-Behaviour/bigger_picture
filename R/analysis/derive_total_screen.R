#' Derive the analysis-level total screen time (DR-0013)
#'
#' Decided unit by unit (dataset by wave by country), so every row of a unit
#' has a total built the same way:
#'
#' - where the unit has a dedicated item (`st_total`), the total is that item;
#' - otherwise it is the sum of the unit's usual-day rows for the content
#'   classes (watching, gaming, social media and educational use), for rows
#'   with all of them, and only when the unit has the required rows (watching
#'   and gaming);
#' - a content-class row asked of only part of a unit is left out of that
#'   unit's sum, so it does not remove the total from everyone else: it must
#'   be present on at least `total_row_min_share` of the rows that have the
#'   required rows;
#' - device, subset, context and aggregate rows are never summed, nor are the
#'   narrower activity rows (messaging, video chat and the like), which
#'   overlap the content classes;
#' - a sum above the plausibility limit is set to the limit and flagged.
#'
#' A dedicated item whose mapping is `partial` is not a total of all screen
#' use. Where the unit has the content classes a sum needs, the sum is used
#' in its place; where it has not, the item stands and its source is
#' `dedicated_partial`. A sum that stands in for a `partial` item is
#' `summed_for_partial`: it can fall short of the item it replaces, so it
#' is told apart from an ordinary sum. An exception (`total_dedicated`) can
#' say that at some waves a `partial` item is a total after all.
#'
#' `other_<exposure>` is the rest of a row's screen use, for the models that
#' adjust one content class for the others: the sum of the unit's other
#' summed rows, or the dedicated total minus the exposure. The total is
#' reduced by a class only where the class's time is part of what the total
#' item measured, which is a fact about the study's questionnaire and is
#' recorded for the dataset (`parts`, from its `part_of_total`
#' exceptions). Without that record the unit has no `other_<exposure>` and
#' is left out of those models. A row whose class is larger than its total
#' has none either.
#'
#' @param exposures A tibble of exposure columns, one row per base row.
#' @param unit The stage 1 unit of each row.
#' @param exposure_registry The exposure registry.
#' @param settings From `analysis_settings()`.
#' @param total_partial Whether the dataset's `st_total` is mapped as
#'   `partial`.
#' @param wave The wave of each row, for the exceptions.
#' @param dedicated_waves Waves at which a `partial` item is used as a
#'   total all the same; NA for every wave.
#' @param parts The content-class rows recorded as parts of the dataset's
#'   dedicated total: a tibble of `variable_name` and `wave` (NA for every
#'   wave), or NULL.
#' @return A tibble with one row per input row: `total_screen`,
#'   `total_source` (`dedicated`, `dedicated_partial`, `summed` or
#'   `summed_for_partial`), `total_rows` (the rows used),
#'   `total_coverage` (how many content-class rows the unit measures),
#'   `total_capped`, and `other_<exposure>` for each content class present.
derive_total_screen <- function(
  exposures,
  unit,
  exposure_registry,
  settings = default_analysis_settings(),
  total_partial = FALSE,
  wave = rep(NA_character_, nrow(exposures)),
  dedicated_waves = character(),
  parts = NULL
) {
  n <- nrow(exposures)
  content <- exposure_registry[
    exposure_registry$unit %in%
      "hours" &
      exposure_registry$day %in% "usual" &
      exposure_registry$context %in% "any" &
      exposure_registry$axis %in% "activity" &
      !is.na(exposure_registry$content_class) &
      exposure_registry$variable_name %in% names(exposures),
  ]
  dedicated <- "st_total"
  limit <- settings$exposure_max_hours

  out <- tibble::tibble(
    total_screen = rep(NA_real_, n),
    total_source = rep(NA_character_, n),
    total_rows = rep(NA_character_, n),
    total_coverage = rep(NA_integer_, n),
    total_capped = rep(FALSE, n)
  )
  for (exposure in content$exposure) {
    out[[paste0("other_", exposure)]] <- rep(NA_real_, n)
  }
  if (n == 0) {
    return(out)
  }

  observed <- function(variable, index) {
    if (variable %in% names(exposures)) {
      !is.na(exposures[[variable]][index])
    } else {
      rep(FALSE, length(index))
    }
  }
  row_sum <- function(variables, index) {
    if (length(variables) == 0) {
      return(rep(NA_real_, length(index)))
    }
    rowSums(as.matrix(exposures[index, variables, drop = FALSE]))
  }
  other_column <- function(variable) {
    paste0("other_", content$exposure[content$variable_name == variable])
  }

  unit_key <- dplyr::coalesce(as.character(unit), "")
  for (key in unique(unit_key)) {
    index <- which(unit_key == key)
    measured <- content$variable_name[vapply(
      content$variable_name,
      function(variable) any(observed(variable, index)),
      logical(1)
    )]
    out$total_coverage[index] <- length(measured)

    # A dedicated item mapped as `partial` covers only part of screen use
    # (device time beside a separate television item, say), unless an
    # exception says that at this wave it is a total after all.
    excepted <- length(dedicated_waves) > 0 &&
      (anyNA(dedicated_waves) || all(wave[index] %in% dedicated_waves))
    partial <- isTRUE(total_partial) && !excepted
    needed <- Reduce(
      `&`,
      lapply(settings$total_required, observed, index = index)
    )
    can_sum <- all(settings$total_required %in% measured) && any(needed)
    # A partial item gives way to a sum of content classes where one can
    # be made, and stands, marked as partial, where it cannot.
    if (any(observed(dedicated, index)) && !(partial && can_sum)) {
      total <- exposures[[dedicated]][index]
      out$total_screen[index] <- total
      out$total_source[index] <- if (partial) {
        "dedicated_partial"
      } else {
        "dedicated"
      }
      out$total_rows[index] <- dedicated
      # The rest of a child's screen use is the total less the class only
      # where the class is recorded as part of the total at this wave.
      for (variable in measured) {
        recorded <- parts$wave[parts$variable_name %in% variable]
        within <- length(recorded) > 0 &&
          (anyNA(recorded) || all(wave[index] %in% recorded))
        if (within) {
          # A class larger than the total it is part of is an inconsistent
          # pair of answers, not a child with no other screen use.
          own <- exposures[[variable]][index]
          rest <- total - own
          rest[!is.na(rest) & rest < -1e-8] <- NA_real_
          out[[other_column(variable)]][index] <- pmax(rest, 0)
        }
      }
      next
    }
    if (!all(settings$total_required %in% measured)) {
      next
    }

    has_required <- Reduce(
      `&`,
      lapply(settings$total_required, observed, index = index)
    )
    share <- vapply(
      measured,
      function(variable) {
        sum(observed(variable, index) & has_required) / sum(has_required)
      },
      numeric(1)
    )
    summed <- measured[share >= settings$total_row_min_share]
    if (!all(settings$total_required %in% summed)) {
      next
    }
    total <- row_sum(summed, index)
    over <- !is.na(total) & total > limit
    total[over] <- limit
    out$total_screen[index] <- total
    out$total_capped[index] <- over
    out$total_source[index] <- if (partial && any(observed(dedicated, index))) {
      "summed_for_partial"
    } else {
      "summed"
    }
    out$total_rows[index] <- paste(summed, collapse = "|")
    for (variable in summed) {
      rest <- pmin(row_sum(setdiff(summed, variable), index), limit)
      out[[other_column(variable)]][index] <- rest
    }
  }
  out
}

#' Whether a total was built as a sum of content classes
#'
#' @param source `total_source` values.
#' @return TRUE for `summed` and `summed_for_partial`.
total_is_summed <- function(source) {
  source %in% c("summed", "summed_for_partial")
}
