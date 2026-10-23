#' Suffix the administrative columns every FLASHE file repeats
bp2296_tag <- function(tbl, tag) {
  shared <- c(
    "PID",
    "FASTCMPFL",
    "DATE_STARTED",
    "DATE_SUBMITTED",
    "QUOTA_P",
    "QUOTA_T",
    "SEQ",
    "SURVEY_TYPE",
    "TASKID",
    "WT_P_BOTH",
    "WT_T_BOTH"
  )
  dplyr::rename_with(tbl, ~ paste0(.x, "_", tag), dplyr::any_of(shared))
}

#' Tidier for BPIPD-2296 (FLASHE)
#'
#' One cross-sectional round: demographic, physical activity and diet files for
#' each teen and parent, linked on `DYADID` (flashe-data-users-guide.pdf p. 4).
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent, keyed on `PID` (equivalently `DYADID`)
tidy_BPIPD_2296 <- function(raw_dataset, spec) {
  # The teen demographic file holds every adolescent, so it is the spine.
  tidied <- raw_dataset$data$teen_demo_data |>
    dplyr::left_join(
      bp2296_tag(raw_dataset$data$teen_pa_data, "tpa"),
      by = "DYADID",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      bp2296_tag(raw_dataset$data$teen_diet_data, "tdiet"),
      by = "DYADID",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      bp2296_tag(raw_dataset$data$parent_demo_data, "pdemo"),
      by = "DYADID",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      bp2296_tag(raw_dataset$data$parent_pa_data, "ppa"),
      by = "DYADID",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      bp2296_tag(raw_dataset$data$parent_diet_data, "pdiet"),
      by = "DYADID",
      relationship = "one-to-one"
    )

  if (anyDuplicated(tidied$PID) > 0) {
    stop("BPIPD-2296: `PID` does not uniquely identify rows.", call. = FALSE)
  }

  tibble::as_tibble(tidied)
}
