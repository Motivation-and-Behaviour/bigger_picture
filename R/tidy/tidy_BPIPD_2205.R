#' Tidier for BPIPD-2205 (Burnell)
#'
#' One CSV holds a two-week daily diary, one row per adolescent-day, with the
#' baseline scales repeated on every day; the days are summarised per
#' adolescent as person means with day counts, and the labels come from the
#' codebook email and the paper.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent (single burst, no wave)
tidy_BPIPD_2205 <- function(raw_dataset, spec) {
  days <- tibble::as_tibble(raw_dataset$data$data)

  # Band codes 1-14 in hours; 15 (prefer not to answer) and the undocumented -999, only ever on an adolescent's last day, are not answers.
  days$SelfReportSMUse_hours <- c(0, 0.5, 1:12)[
    match(days$SelfReportSMUse, 1:14)
  ]
  days$ObjectiveSMUse_hours <- days$ObjectiveSMUse / 60
  # Item-weighted mean of the 3-item anxious and 2-item depressive composites, i.e. the 5-item mean, on days with both.
  days$NegAffect <- (3 * days$NegAffect_Anx + 2 * days$NegAffect_Dep) / 5

  daily <- c(
    "SelfReportSMUse_hours",
    "ObjectiveSMUse_hours",
    "PosAffect",
    "NegAffect_Anx",
    "NegAffect_Dep",
    "NegAffect"
  )
  # Every answered day counts, days 16-20 included, as the paper's Table 2 objective mean needs them.
  # A mean needs 7 answered days, half the designed 14.
  person_means <- dplyr::summarise(
    days,
    dplyr::across(
      dplyr::all_of(daily),
      list(
        mean = ~ if (sum(!is.na(.x)) >= 7L) {
          mean(.x, na.rm = TRUE)
        } else {
          NA_real_
        },
        n = ~ sum(!is.na(.x))
      )
    ),
    .by = "ID"
  )

  # The baseline scales repeat on every day row; the one-to-one join fails if any varies within an adolescent.
  baseline <- c(
    "Age",
    "Gender",
    "SocMedAddict",
    "PercSocMedAddict",
    "SMFQ_DepSymp"
  )
  df <- dplyr::distinct(days[c("ID", baseline)]) |>
    dplyr::left_join(person_means, by = "ID", relationship = "one-to-one")

  bp2205_label(df)
}

#' Attach variable and value labels from the codebook email and the paper
bp2205_label <- function(df) {
  rule <- "; NA if fewer than 7 such days (half the designed 14)"
  labels <- c(
    ID = "Participant identifier",
    Age = "Age (integer)",
    Gender = "Gender",
    SocMedAddict = "Addiction-like social media use composite, 0-2 (item mean), baseline",
    PercSocMedAddict = "Do you think you may be addicted to social media? Baseline",
    SMFQ_DepSymp = "Depressive symptoms, Short Mood and Feelings Questionnaire, 0-2 (item mean), baseline",
    SelfReportSMUse_hours_mean = paste0(
      "Self-reported hours on social media per day, person mean over days with a band answered (None 0, less than 1 hour 0.5, 1-11 hours as stated, 12 or more hours 12; prefer not to answer and -999 unanswered)",
      rule
    ),
    SelfReportSMUse_hours_n = "Days with a self-reported social media band answered (codes 1-14)",
    ObjectiveSMUse_hours_mean = paste0(
      "Hours per day in the iOS Screen Time 'Social' category (minutes / 60), person mean over days with minutes recorded (none recorded: no screenshot, not an iPhone, or 'Social' not in that day's top three)",
      rule
    ),
    ObjectiveSMUse_hours_n = "Days with iOS Screen Time 'Social' minutes recorded",
    PosAffect_mean = paste0(
      "Positive affect (happy, energetic; 1-5, day composite over that day's prompts), person mean over days with a composite",
      rule
    ),
    PosAffect_n = "Days with a positive affect composite",
    NegAffect_Anx_mean = paste0(
      "Anxious negative affect (anxious, stressed, irritated; 1-5, day composite over that day's prompts), person mean over days with a composite",
      rule
    ),
    NegAffect_Anx_n = "Days with an anxious negative affect composite",
    NegAffect_Dep_mean = paste0(
      "Depressive negative affect (sad, lonely; 1-5, day composite over that day's prompts), person mean over days with a composite",
      rule
    ),
    NegAffect_Dep_n = "Days with a depressive negative affect composite",
    NegAffect_mean = paste0(
      "Negative affect (anxious, stressed, irritated, sad, lonely; 1-5), day value (3 x anxious + 2 x depressive composite) / 5 on days with both, person mean over those days",
      rule
    ),
    NegAffect_n = "Days with both negative affect composites"
  )
  values <- list(
    PercSocMedAddict = c(
      "Definitely not" = 1,
      "Maybe mildly" = 2,
      "Maybe moderately" = 3,
      "Maybe severely" = 4
    ),
    Gender = c(Girl = 0, Boy = 1)
  )

  for (column in names(labels)) {
    attr(df[[column]], "label") <- labels[[column]]
  }
  for (column in names(values)) {
    df[[column]] <- haven::labelled(
      df[[column]],
      labels = values[[column]],
      label = labels[[column]]
    )
  }

  df
}
