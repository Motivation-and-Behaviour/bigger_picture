#' Tidier for BPIPD-1115 (Chen)
#'
#' Online child self-report survey of March 2020 in three Sichuan primary
#' schools; the folder's classroom file is not used.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child
tidy_BPIPD_1115 <- function(raw_dataset, spec) {
  df <- raw_dataset$data$online_survey_2020
  # The online_ prefix keeps the key distinct from any later file of the project.
  df$participant_id <- paste0("online_", df$ID)
  df <- dplyr::relocate(df, "participant_id")

  if (anyDuplicated(df$participant_id) > 0) {
    stop("BPIPD-1115: `participant_id` is not unique.", call. = FALSE)
  }

  labels <- bp1115_labels()
  for (column in names(labels)) {
    attr(df[[column]], "label") <- labels[[column]]
  }
  tibble::as_tibble(df)
}

#' Variable labels for the built column and the unlabelled source columns
bp1115_labels <- function() {
  items <- seq_len(21)
  c(
    participant_id = "Row key: online_<ID>",
    Time_phone = "Time on smartphone, hours per day",
    Time_SocMed = "Time on social media, hours per day",
    Time_game = "Time on gaming, hours per day",
    Grade = "School grade",
    Sex = "Sex (1 = M, 2 = F)",
    Race = "Ethnicity (1 = Han, 2 = Others)",
    Sick = "Ill in the past week (1 = Y, 2 = N)",
    stats::setNames(
      sprintf("DASS-21 item %d, coded 1-4 as released", items),
      paste0("DASS", items)
    )
  )
}
