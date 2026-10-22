#' Tidier for BPIPD-718 (Tooth (ALSWH))
#'
#' One MatCH survey (2016-17) in which mothers report on up to three children;
#' the child file is the spine and every other file is mother-level.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per MatCH child (`CIDALIAS`), single timepoint
tidy_BPIPD_718 <- function(raw_dataset, spec) {
  data <- raw_dataset$data

  # `MODE` is the same MatCH survey's completion method in both MatCH files and
  # agrees on every child row, so the mother file's copy is dropped.
  mother <- dplyr::select(data$match1_mother, -"MODE")

  # `y1wtarea` is carried in both ALSWH survey files and is identical in them.
  survey7 <- dplyr::select(data$wha7yngb, -"y1wtarea")

  df <- data$match1_child |>
    tibble::as_tibble() |>
    dplyr::left_join(mother, by = "IDALIAS", relationship = "many-to-one") |>
    dplyr::left_join(
      data$wha1yngb,
      by = c("IDALIAS" = "idalias"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      survey7,
      by = c("IDALIAS" = "idalias"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      data$recentyngstatus,
      by = c("IDALIAS" = "idalias"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      bp718_klv_wide(data$KLV_1973_78),
      by = c("IDALIAS" = "idalias"),
      relationship = "many-to-one"
    )

  if (anyDuplicated(df$CIDALIAS) > 0) {
    stop(
      "BPIPD-718: `CIDALIAS` does not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' Key longitudinal variables, one mother-level row with a column per survey
bp718_klv_wide <- function(klv) {
  # One row per woman per ALSWH survey (1-9), so it pivots rather than joins.
  # The `klv_` prefix keeps the mother's `age` and `mh` from reading as a child's.
  tidyr::pivot_wider(
    klv,
    id_cols = "idalias",
    names_from = "survey",
    values_from = !dplyr::any_of(c("idalias", "survey")),
    names_glue = "klv_{.value}_s{survey}"
  )
}
