#' Tidier for BPIPD-1598 (Social Distancing and Development Project)
#'
#' One wide Excel sheet holds four parent-report timepoints in a column prefix
#' (`T1_`-`T4_`); T1 and T4 field no screen item, so T2 and T3 are pivoted long.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per timepoint (T2, T3) they took part in
tidy_BPIPD_1598 <- function(raw_dataset, spec) {
  # The "missing screen use" workbook is this one without the screen-use items.
  df <- tibble::as_tibble(
    raw_dataset$data$screentime_additional_measures_ngg_data
  )

  # The gender header cell ends in a line break no mapping expression can name.
  df <- dplyr::rename(df, Gender = "Gender \r\n")

  # Screen items exist only at T2 and T3 (Info sheet), so T1 and T4 are dropped.
  df <- dplyr::select(df, -dplyr::matches("^T[14]_"))

  # "NC" is the Info sheet's not-collected marker; zapping it also aligns types.
  measures <- setdiff(names(df), "ParticipantID")
  df[measures] <- lapply(df[measures], bp1598_zap_nc)

  stems <- unique(sub("^T[23]_", "", grep("^T[23]_", names(df), value = TRUE)))

  long <- df |>
    tidyr::pivot_longer(
      cols = dplyr::matches("^T[23]_"),
      names_to = c("wave", ".value"),
      names_pattern = "^(T[23])_(.*)$"
    ) |>
    dplyr::relocate("wave", .after = "ParticipantID")

  if (anyDuplicated(long[c("ParticipantID", "wave")]) > 0) {
    stop(
      "BPIPD-1598: `ParticipantID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  # A timepoint with no measured value is one the family did not take part in.
  observed <- rowSums(!is.na(long[stems])) > 0L
  long[observed, , drop = FALSE]
}

#' Turn the "not collected" marker into NA, and numeric columns back to numeric
bp1598_zap_nc <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  x[x == "NC"] <- NA_character_
  numbers <- suppressWarnings(as.numeric(x))
  if (all(is.na(x) == is.na(numbers))) {
    numbers
  } else {
    x
  }
}
