#' Tidier for BPIPD-655 (Košir)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per student who answered the student questionnaire,
#'   from the study's single 2014 cross-sectional file
tidy_BPIPD_655 <- function(raw_dataset, spec) {
  # Drop 9 rostered students who answered no questionnaire item (FB and all
  # 30 SDQ-II items missing); they carry only teacher and peer data.
  tibble::as_tibble(raw_dataset$data$Kosir_data_2014) |>
    dplyr::filter(
      !(is.na(FB) &
        dplyr::if_all(dplyr::all_of(paste0("SDQ", seq_len(30))), is.na))
    )
}
