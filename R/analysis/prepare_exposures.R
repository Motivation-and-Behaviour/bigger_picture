#' The exposure table of one dataset, aligned with its base table
#'
#' Keeps every screen-time row measured in hours or as an index, applies the
#' plausibility limit to hours (DR-0011) and adds the derived total
#' (DR-0013). Nothing else is excluded on the exposure.
#'
#' @param rows The dataset's kept primary rows, in base-table order.
#' @param wave The wave of each row.
#' @param unit The stage 1 unit of each row.
#' @param exposure_registry The exposure registry.
#' @param settings From `analysis_settings()`.
#' @param total_partial,dedicated_waves,parts See `derive_total_screen()`.
#' @param left_out The dataset's `exclude` exceptions about exposure rows
#'   (`variable_name`, `wave` and `reason`): each blanks its row at that
#'   wave, or at every wave, and is recorded in the audit.
#' @return A list: `exposures` (one row per base row) and `audit` (one row
#'   per wave and exposure row with any value).
prepare_exposures <- function(
  rows,
  wave,
  unit,
  exposure_registry,
  settings = default_analysis_settings(),
  total_partial = FALSE,
  dedicated_waves = character(),
  parts = NULL,
  left_out = NULL
) {
  registry <- exposure_registry[
    exposure_registry$unit %in%
      c("hours", "index") &
      exposure_registry$variable_name %in% names(rows),
  ]
  wave <- as.character(wave)
  exposures <- tibble::tibble(.rows = nrow(rows))
  audit <- list()

  for (i in seq_len(nrow(registry))) {
    variable <- registry$variable_name[i]
    values <- as.numeric(rows[[variable]])
    if (all(is.na(values))) {
      next
    }
    over <- registry$unit[i] == "hours" &
      !is.na(values) &
      values > settings$exposure_max_hours
    observed <- !is.na(values)
    values[over] <- NA_real_
    exposures[[variable]] <- values
    audit[[variable]] <- dplyr::summarise(
      tibble::tibble(wave = wave, observed = observed, over = over),
      n_values = sum(.data$observed),
      n_used = sum(.data$observed & !.data$over),
      .by = "wave"
    )
    audit[[variable]]$variable_name <- variable
  }

  audit <- dplyr::bind_rows(
    tibble::tibble(
      wave = character(),
      n_values = integer(),
      n_used = integer(),
      variable_name = character()
    ),
    dplyr::bind_rows(audit)
  )
  audit <- audit[audit$n_values > 0, ]
  excluded <- audit$n_values - audit$n_used
  audit$kind <- rep("exposure", nrow(audit))
  audit$status <- rep("used", nrow(audit))
  audit$detail <- as.character(ifelse(
    excluded > 0,
    paste0(excluded, " values over ", settings$exposure_max_hours, " hours"),
    NA_character_
  ))

  # An exception can leave an exposure row out at a wave, or at every wave:
  # its values there are not used for anything, and the audit says so. One
  # that names a row or a wave the dataset does not have is a mistake.
  for (i in seq_len(NROW(left_out))) {
    name <- left_out$variable_name[i]
    at_wave <- left_out$wave[i]
    if (!name %in% names(exposures)) {
      stop(
        "prepare_exposures(): an exception excludes `",
        name,
        "`, which is not an exposure with values in this dataset.",
        call. = FALSE
      )
    }
    if (!is.na(at_wave) && !at_wave %in% wave) {
      stop(
        "prepare_exposures(): an exception excludes `",
        name,
        "` at wave `",
        at_wave,
        "`, which is not a wave of this dataset.",
        call. = FALSE
      )
    }
    at <- is.na(at_wave) | wave %in% at_wave
    exposures[[name]][at] <- NA
    noted <- audit$variable_name == name &
      (is.na(at_wave) | audit$wave %in% at_wave)
    audit$n_used[noted] <- 0L
    audit$status[noted] <- "excluded"
    audit$detail[noted] <- paste0("exception: ", left_out$reason[i])
  }
  total <- derive_total_screen(
    exposures,
    unit,
    exposure_registry,
    settings,
    total_partial = total_partial,
    wave = wave,
    dedicated_waves = dedicated_waves,
    parts = parts
  )
  list(exposures = dplyr::bind_cols(exposures, total), audit = audit)
}
