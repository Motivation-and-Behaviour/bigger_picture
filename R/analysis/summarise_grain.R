#' Rows, participants and duplicate keys per dataset, wave and measure
#'
#' The harmonised grain is one row per participant, wave and screen-time
#' measure (DR-0003), so more than one row for a key is a harmonisation fault.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @return A tibble with one row per wave and measure, or NULL.
summarise_grain <- function(harmonised, info) {
  rows <- included_rows(harmonised, info)
  if (is.null(rows)) {
    return(NULL)
  }
  keys <- data.table::data.table(
    wave = as.character(rows$wave),
    st_measure_id = as.character(rows$st_measure_id),
    participant_id = as.character(rows$participant_id)
  )
  per_key <- keys[, list(n = .N), by = c("wave", "st_measure_id", "participant_id")]
  grain <- per_key[,
    list(
      rows = sum(n),
      participants = .N,
      duplicate_keys = sum(n > 1L & !is.na(participant_id)),
      missing_ids = sum(n[is.na(participant_id)])
    ),
    by = c("wave", "st_measure_id")
  ]
  out <- tibble::as_tibble(grain)
  out$dataset_id <- as.character(info$dataset_id)
  out[c(
    "dataset_id",
    "wave",
    "st_measure_id",
    "rows",
    "participants",
    "duplicate_keys",
    "missing_ids"
  )]
}
