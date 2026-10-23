#' Tidier for BPIPD-64 (MoMo)
#'
#' One cross-sectional MoMo 2.0 file (2023-2024); participants who did not
#' attend on site answered an online questionnaire, whose media codes differ.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant (single occasion)
tidy_BPIPD_64 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$momo_dataset)

  # One row repeats an earlier row in all 204 columns (also counted twice in
  # the codebook); the first is kept.
  df <- df[!duplicated(df), ]

  if (anyDuplicated(df$stamm_id) > 0) {
    stop("BPIPD-64: `stamm_id` does not uniquely identify rows.", call. = FALSE)
  }

  bp64_split_online(df, bp64_media_time_items)
}

#' Media time items whose online codes (1-8) differ from the on-site hours codes
bp64_media_time_items <- c(
  "medien_fernsehenwoche",
  "medien_fernsehenwochenende",
  "medien_spielenwoche",
  "medien_spielenwochenende",
  "medien_internetwoche",
  "medien_internetwochenende",
  "medien_lernenanalogwoche",
  "medien_lernenanalogwochenende",
  "medien_lesenanalogwoche",
  "medien_lesenanalogwochenende",
  "medien_analogwoche",
  "medien_analogwochenende"
)

#' Move the online-questionnaire rows of `cols` into `<col>_online` columns
bp64_split_online <- function(df, cols) {
  # The codebook gives one hours coding for all rows, but online rows never
  # use 0 or 0.5 and alone use 7 and 8, so the two are kept apart.
  online <- !is.na(df$daten_onlinefragebogen)

  for (col in cols) {
    onsite <- df[[col]]
    onsite[online] <- NA
    attr(onsite, "label") <- paste(
      attr(onsite, "label"),
      "[on-site questionnaire rows; online rows in",
      paste0(col, "_online]")
    )

    online_col <- df[[col]]
    online_col[!online] <- NA
    labels <- attr(online_col, "labels")
    attr(online_col, "labels") <- labels[labels == -9]
    attr(online_col, "label") <- paste(
      attr(online_col, "label"),
      "[online questionnaire rows; codes 1-8 undocumented]"
    )

    df[[col]] <- onsite
    df[[paste0(col, "_online")]] <- online_col
    df <- dplyr::relocate(
      df,
      dplyr::all_of(paste0(col, "_online")),
      .after = dplyr::all_of(col)
    )
  }

  df
}
