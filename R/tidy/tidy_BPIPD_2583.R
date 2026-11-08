#' Tidier for BPIPD-2583 (YSS / CSTADS / CSADS)
#'
#' Six unlinked school-survey cycles; only the two Youth Smoking Survey cycles
#' ask a screen item (TV or video hours), so only they are stacked.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per student per survey cycle
tidy_BPIPD_2583 <- function(raw_dataset, spec) {
  # Drop-wave rule: CSTADS 2014-15 to CSADS 2023-24 field no screen-use item
  # (user guides and codebooks), so their tables are not used.
  yss_2002 <- raw_dataset$data[["YSS 2002 Data"]] |>
    dplyr::rename(DVSELF_4item = "DVSELF", DVTY2ST_7cat = "DVTY2ST") |>
    dplyr::mutate(participant_id = paste0(.wave, "_", PUMFID), .before = 1)

  # The 2006-07 .sav stores its names in lower case; its codebook and the 2002
  # file use upper case. DVSELF and DVTY2ST are coded differently per cycle.
  yss_2006 <- raw_dataset$data[["YSS 2006-07 Data"]] |>
    dplyr::rename_with(toupper, !dplyr::starts_with(".")) |>
    dplyr::rename(DVSELF_3item = "DVSELF", DVTY2ST_8cat = "DVTY2ST") |>
    dplyr::mutate(
      participant_id = paste0(.wave, "_", sprintf("%.0f", SCANID)),
      .before = 1
    )

  # `vctrs` will not bind columns whose SPSS display formats differ.
  yss_2002 <- haven::zap_widths(haven::zap_formats(yss_2002))
  yss_2006 <- haven::zap_widths(haven::zap_formats(yss_2006))

  # The shared columns use the same codes and differ only in label wording.
  for (col in c("GRADE", "DVTY1ST", "DVAMTSMK", "DVSMKPTN")) {
    labels <- c(
      attr(yss_2002[[col]], "labels"),
      attr(yss_2006[[col]], "labels")
    )
    labels <- labels[!duplicated(labels)]
    attr(yss_2002[[col]], "labels") <- labels
    attr(yss_2006[[col]], "labels") <- labels
  }

  df <- dplyr::bind_rows(yss_2002, yss_2006)

  if (anyDuplicated(df$participant_id) > 0) {
    stop("BPIPD-2583: `participant_id` is not unique.", call. = FALSE)
  }
  column_labels <- c(
    participant_id = "Cycle-prefixed student id (<cycle>_<PUMFID or SCANID>)",
    DVSELF_4item = "Self-esteem scale, 4 items (GPY_09A-D), 0-16 (2002)",
    DVSELF_3item = "Self-esteem scale, 3 items (OHOWFLA1-C1), 0-12 (2006-07)",
    DVTY2ST_7cat = "Detailed smoking status, 7 categories (2002)",
    DVTY2ST_8cat = "Detailed smoking status, 8 categories, 7 = Puffer (2006-07)"
  )
  for (col in names(column_labels)) {
    attr(df[[col]], "label") <- column_labels[[col]]
  }

  tibble::as_tibble(df)
}
