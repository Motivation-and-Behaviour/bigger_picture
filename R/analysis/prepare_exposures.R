#' The exposure table of one dataset, aligned with its base table
#'
#' Keeps every screen-time row measured in hours or as an index, applies the
#' plausibility limit to hours (DR-0011) and adds the derived total
#' (DR-0013). Nothing else is excluded on the exposure.
#'
#' @param rows The dataset's kept primary rows, in base-table order.
#' @param wave The wave of each row.
#' @param exposure_registry The exposure registry.
#' @param settings From `analysis_settings()`.
#' @return A list: `exposures` (one row per base row) and `audit` (one row
#'   per wave and exposure row with any value).
prepare_exposures <- function(
  rows,
  wave,
  exposure_registry,
  settings = analysis_settings()
) {
  registry <- exposure_registry[
    exposure_registry$unit %in% c("hours", "index") &
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

  total <- derive_total_screen(exposures, wave, exposure_registry, settings)
  list(exposures = dplyr::bind_cols(exposures, total), audit = audit)
}
