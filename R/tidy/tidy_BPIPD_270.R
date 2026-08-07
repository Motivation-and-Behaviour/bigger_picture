#' Template tidier for pre-harmonisation dataset shaping
#'
#' Use this step for study-specific table assembly before harmonisation, for
#' example joining multiple raw files, binding waves, filtering records, or
#' reshaping raw tables into one canonical analysis tibble.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble to be used as the harmonisation input
tidy_BPIPD_270 <- function(raw_dataset, spec) {
  waves <- paste0("w", 1:7)
  cohorts <- c("elem", "mid")

  # The rda files seem to be the most complete. Every column in them except
  # ID/HID/PID carries a wave suffix (YGENDERw3, WEIGHTA1w1) which is stripped.
  read_respondent <- function(wave, cohort, respondent) {
    name <- paste("kcyps", wave, cohort, respondent, "data_rda", sep = "_")
    raw <- raw_dataset$data[[name]]
    # Siblings were not surveyed at wave 1
    if (is.null(raw)) {
      return(NULL)
    }
    raw |>
      tibble::as_tibble() |>
      dplyr::rename_with(\(x) sub("w[0-9]+$", "", x))
  }

  # The released files keep the full panel roster, so a youth who did not take
  # part in a wave still has a row: `SURVEY1 == 2` with every questionnaire item
  # missing. Those rows carry no observation, so drop them. `SURVEY1` is absent
  # at wave 1, where every panel member responded.
  drop_nonparticipants <- function(df) {
    if (is.null(df) || !"SURVEY1" %in% names(df)) {
      return(df)
    }
    df[!(!is.na(df$SURVEY1) & df$SURVEY1 == 2), , drop = FALSE]
  }

  read_wave_cohort <- function(this_wave, this_cohort) {
    # `main` and `sibling` are the same youth questionnaire put to two different
    # children in the same household: they share most of their columns and none
    # of their IDs, so they stack as extra participants rather than joining as
    # extra variables.
    youth <- dplyr::bind_rows(
      main = drop_nonparticipants(
        read_respondent(this_wave, this_cohort, "main")
      ),
      sibling = drop_nonparticipants(
        read_respondent(this_wave, this_cohort, "sibling")
      ),
      .id = "respondent"
    )

    # The guardian file holds at most one row per child across both youth
    # files, keyed on the child's ID, so it joins one-to-one. It is joined from
    # the left so that guardians of a non-participating youth do not reappear
    # as rows with no youth data.
    guardian <- read_respondent(this_wave, this_cohort, "guardian")

    # `cohort` is the file-derived label, not the study's own `COHORT` column
    # which is not present in every wave.
    youth |>
      dplyr::left_join(
        guardian,
        by = c("ID", "HID", "PID"),
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(wave = this_wave, cohort = this_cohort, .before = 1)
  }

  combos <- expand.grid(
    wave = waves,
    cohort = cohorts,
    stringsAsFactors = FALSE
  )

  df <- dplyr::bind_rows(
    unname(Map(read_wave_cohort, combos$wave, combos$cohort))
  )

  # Participants keep their ID across waves, so ID + wave is the row key.
  if (anyDuplicated(df[c("ID", "wave")]) > 0) {
    stop(
      "BPIPD-270: `ID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}
