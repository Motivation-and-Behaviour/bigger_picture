#' Tidier for BPIPD-1453 (Bouzid)
#'
#' The `Data` sheet has a two-row header: the merged `Game_Genre` cell spans
#' five columns whose genre names arrive as the first data row.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per student (single cross-sectional assessment)
tidy_BPIPD_1453 <- function(raw_dataset, spec) {
  d <- tibble::as_tibble(raw_dataset$data$data)

  genre_cols <- c("Game_Genre", "...6", "...7", "...8", "...9")
  names(d)[match(genre_cols, names(d))] <- paste0(
    "Game_Genre_",
    unlist(d[1, genre_cols], use.names = FALSE)
  )

  # Dropping row 1 would silently lose a student if it ever held an id.
  if (!is.na(d$Participant_ID[1])) {
    stop("BPIPD-1453: row 1 is not the genre sub-header row.", call. = FALSE)
  }

  d[-1, ]
}
