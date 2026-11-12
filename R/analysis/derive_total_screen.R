#' Derive the analysis-level total screen time (DR-0013)
#'
#' Decided wave by wave. Where the wave has a dedicated item (`st_total`),
#' the total is that item. Otherwise it is the sum of the wave's usual-day
#' activity rows, for rows with all of them, and only when the wave has the
#' required rows (watching and gaming). Device, subset, context and aggregate
#' rows are never summed. A sum above the plausibility limit is set to the
#' limit and flagged.
#'
#' The same rule gives `other_<exposure>`, the summed activity rows leaving
#' one out, for the models that adjust one type of screen use for the rest.
#'
#' @param exposures A tibble of exposure columns, one row per base row.
#' @param wave The wave of each row.
#' @param exposure_registry The exposure registry.
#' @param settings From `analysis_settings()`.
#' @return A tibble with one row per input row: `total_screen`,
#'   `total_source` (`dedicated` or `summed`), `total_rows` (the rows used),
#'   `total_coverage` (how many activity rows were summed), `total_capped`
#'   and one `other_<exposure>` column per activity row present.
derive_total_screen <- function(
  exposures,
  wave,
  exposure_registry,
  settings = analysis_settings()
) {
  n <- nrow(exposures)
  usual <- exposure_registry[
    exposure_registry$unit %in% "hours" &
      exposure_registry$day %in% "usual" &
      exposure_registry$context %in% "any",
  ]
  activity <- usual[usual$axis %in% "activity", ]
  activity <- activity[activity$variable_name %in% names(exposures), ]
  dedicated <- "st_total"

  out <- tibble::tibble(
    total_screen = rep(NA_real_, n),
    total_source = rep(NA_character_, n),
    total_rows = rep(NA_character_, n),
    total_coverage = rep(NA_integer_, n),
    total_capped = rep(FALSE, n)
  )
  for (exposure in activity$exposure) {
    out[[paste0("other_", exposure)]] <- rep(NA_real_, n)
  }
  if (n == 0) {
    return(out)
  }

  has_values <- function(variable, index) {
    variable %in% names(exposures) && any(!is.na(exposures[[variable]][index]))
  }
  row_sum <- function(variables, index) {
    if (length(variables) == 0) {
      return(rep(NA_real_, length(index)))
    }
    rowSums(as.matrix(exposures[index, variables, drop = FALSE]))
  }

  wave_key <- dplyr::coalesce(as.character(wave), "")
  for (key in unique(wave_key)) {
    index <- which(wave_key == key)
    present <- activity$variable_name[vapply(
      activity$variable_name,
      has_values,
      logical(1),
      index = index
    )]

    if (has_values(dedicated, index)) {
      out$total_screen[index] <- exposures[[dedicated]][index]
      out$total_source[index] <- "dedicated"
      out$total_rows[index] <- dedicated
    } else if (all(settings$total_required %in% present)) {
      total <- row_sum(present, index)
      over <- !is.na(total) & total > settings$exposure_max_hours
      total[over] <- settings$exposure_max_hours
      out$total_screen[index] <- total
      out$total_capped[index] <- over
      out$total_source[index] <- "summed"
      out$total_rows[index] <- paste(present, collapse = "|")
      out$total_coverage[index] <- length(present)
    }

    for (variable in present) {
      others <- setdiff(present, variable)
      exposure <- activity$exposure[activity$variable_name == variable]
      other <- pmin(row_sum(others, index), settings$exposure_max_hours)
      out[[paste0("other_", exposure)]][index] <- other
    }
  }
  out
}
