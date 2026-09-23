#' Tidier for BPIPD-625 (YRBSS national high school surveys, 1999-2019)
#'
#' Each survey year is an independent cross-section in its own Access file, and
#' YRBS renumbers its questionnaire every time, so a construct sits under a
#' different `q` column each year. `bp625_items()` names the source column per
#' wave and carries the question wording that becomes the column's label; the
#' `q` columns themselves are not kept, because the same name means a different
#' question from one year to the next.
#'
#' The spec also declares 2021 and 2023, but neither year asks a screen-time
#' item this project can use: 2021 asks only one aggregate screen-time item and
#' 2023 only how often students use social media. Those two waves are left out
#' here by project decision rather than carried with no exposure.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per student per survey year
tidy_BPIPD_625 <- function(raw_dataset, spec) {
  items <- bp625_items()

  df <- dplyr::bind_rows(lapply(
    names(bp625_sources()),
    function(wave) bp625_wave_frame(raw_dataset, wave, items)
  ))

  if (anyDuplicated(df$participant_id) > 0) {
    stop("BPIPD-625: `participant_id` is not unique.", call. = FALSE)
  }

  # `bind_rows()` appends each later year's new stems, so put the map's order
  # back
  df <- df[c("participant_id", ".wave", ".wave_label", items$stem)]

  bp625_label(df, items)
}

#' The data resource holding each survey year's raw questionnaire responses
#'
#' From 2013 the spec also reads an `XXHqn` table; it holds only CDC's
#' dichotomous recodes of the same items, so `XXHq` is the source used here.
#' 2021 and 2023 are declared in the spec but deliberately absent (see
#' `tidy_BPIPD_625()`).
bp625_sources <- function() {
  c(
    "1999" = "yrbs_1999_data",
    "2003" = "yrbs_2003_data",
    "2005" = "yrbs_2005_data",
    "2007" = "yrbs_2007_data",
    "2009" = "yrbs_2009_data",
    "2011" = "yrbs_2011_data",
    "2013" = "yrbs_2013_hq",
    "2015" = "yrbs_2015_hq",
    "2017" = "yrbs_2017_hq",
    "2019" = "yrbs_2019_hq"
  )
}

#' One row of the column map: a label, then the source column at each wave
bp625_entry <- function(label, ...) {
  sources <- c(...)
  waves <- names(bp625_sources())
  c(
    list(label = label),
    as.list(stats::setNames(unname(sources[waves]), waves))
  )
}

#' Source column for each tidied stem at each survey year
#'
#' A wave the item omits is left out. Stems are split wherever the response
#' options or the construct differ, so a wave never contributes to a stem under
#' another wave's meaning: the pre-2007 race question numbers its categories
#' differently from CDC's `raceeth`, and from 2013 the games item's examples
#' add smartphones, YouTube and social networking, so 2013-2019 answers go to
#' `game_device_hours` rather than `game_hours`.
bp625_items <- function() {
  entries <- list(
    # --- provenance --------------------------------------------------------
    record = bp625_entry(
      "CDC record number within the survey year (2013 onward)",
      "2013" = "record",
      "2015" = "record",
      "2017" = "record",
      "2019" = "record"
    ),
    # --- demographics ------------------------------------------------------
    age_band = bp625_entry(
      "How old are you? (1 = 12 or younger ... 7 = 18 or older)",
      "1999" = "q1",
      "2003" = "q1",
      "2005" = "q1",
      "2007" = "q1",
      "2009" = "q1",
      "2011" = "q1",
      "2013" = "q1",
      "2015" = "q1",
      "2017" = "q1",
      "2019" = "q1"
    ),
    sex = bp625_entry(
      "What is your sex? (1 = Female, 2 = Male)",
      "1999" = "q2",
      "2003" = "q2",
      "2005" = "q2",
      "2007" = "q2",
      "2009" = "q2",
      "2011" = "q2",
      "2013" = "q2",
      "2015" = "q2",
      "2017" = "q2",
      "2019" = "q2"
    ),
    school_grade = bp625_entry(
      "In what grade are you? (1 = 9th ... 4 = 12th, 5 = ungraded or other)",
      "1999" = "q3",
      "2003" = "q3",
      "2005" = "q3",
      "2007" = "q3",
      "2009" = "q3",
      "2011" = "q3",
      "2013" = "q3",
      "2015" = "q3",
      "2017" = "q3",
      "2019" = "q3"
    ),
    race_combined_pre2007 = bp625_entry(
      "How do you describe yourself? (single race/ethnicity item, 4 = Hispanic, 6 = White)",
      "1999" = "q4",
      "2003" = "q4",
      "2005" = "q4"
    ),
    hispanic = bp625_entry(
      "Are you Hispanic or Latino? (1 = Yes, 2 = No)",
      "2007" = "q4",
      "2009" = "q4",
      "2011" = "q4",
      "2013" = "q4",
      "2015" = "q4",
      "2017" = "q4",
      "2019" = "q4"
    ),
    race_selected = bp625_entry(
      "What is your race? (select one or more; letters of the options chosen)",
      "2007" = "q5",
      "2009" = "q5",
      "2011" = "q5",
      "2013" = "q5",
      "2015" = "q5",
      "2017" = "q5",
      "2019" = "q5"
    ),
    race_ethnicity_cdc = bp625_entry(
      "Race/ethnicity derived by CDC from Q4 and Q5 (5 = White, 6 = Hispanic/Latino)",
      "2007" = "raceeth",
      "2009" = "raceeth",
      "2011" = "raceeth",
      "2013" = "raceeth",
      "2015" = "raceeth",
      "2017" = "raceeth",
      "2019" = "raceeth"
    ),
    # --- screen use --------------------------------------------------------
    tv_hours = bp625_entry(
      "On an average school day, how many hours do you watch TV? (1 = none ... 7 = 5 or more)",
      "1999" = "q82",
      "2003" = "q83",
      "2005" = "q81",
      "2007" = "q81",
      "2009" = "q81",
      "2011" = "q80",
      "2013" = "q81",
      "2015" = "q81",
      "2017" = "q80",
      "2019" = "q79"
    ),
    game_hours = bp625_entry(
      "On an average school day, how many hours do you play video or computer games or use a computer for something that is not school work? 2007-2011 add examples such as games consoles and the Internet (2011: iPod touch, Facebook) (1 = none ... 7 = 5 or more)",
      "2003" = "q92",
      "2005" = "q91",
      "2007" = "q82",
      "2009" = "q82",
      "2011" = "q81"
    ),
    game_device_hours = bp625_entry(
      "On an average school day, how many hours do you play video or computer games or use a computer for something that is not school work? Counts time on things such as Xbox, PlayStation, a tablet, a smartphone, YouTube and social media (2017 adds texting; 2019: playing games, watching videos, texting or using social media) (1 = none ... 7 = 5 or more)",
      "2013" = "q82",
      "2015" = "q82",
      "2017" = "q81",
      "2019" = "q80"
    ),
    # --- mental health -----------------------------------------------------
    sad_hopeless = bp625_entry(
      "Felt so sad or hopeless almost every day for two weeks or more that you stopped doing some usual activities, past 12 months (1 = Yes, 2 = No)",
      "1999" = "q22",
      "2003" = "q23",
      "2005" = "q23",
      "2007" = "q23",
      "2009" = "q23",
      "2011" = "q24",
      "2013" = "q26",
      "2015" = "q26",
      "2017" = "q25",
      "2019" = "q25"
    ),
    suicide_ideation = bp625_entry(
      "Seriously considered attempting suicide, past 12 months (1 = Yes, 2 = No)",
      "1999" = "q23",
      "2003" = "q24",
      "2005" = "q24",
      "2007" = "q24",
      "2009" = "q24",
      "2011" = "q25",
      "2013" = "q27",
      "2015" = "q27",
      "2017" = "q26",
      "2019" = "q26"
    ),
    suicide_plan = bp625_entry(
      "Made a plan about how you would attempt suicide, past 12 months (1 = Yes, 2 = No)",
      "1999" = "q24",
      "2003" = "q25",
      "2005" = "q25",
      "2007" = "q25",
      "2009" = "q25",
      "2011" = "q26",
      "2013" = "q28",
      "2015" = "q28",
      "2017" = "q27",
      "2019" = "q27"
    ),
    suicide_attempt = bp625_entry(
      "How many times did you actually attempt suicide, past 12 months (1 = 0 times ... 5 = 6 or more)",
      "1999" = "q25",
      "2003" = "q26",
      "2005" = "q26",
      "2007" = "q26",
      "2009" = "q26",
      "2011" = "q27",
      "2013" = "q29",
      "2015" = "q29",
      "2017" = "q28",
      "2019" = "q28"
    ),
    # --- behaviour ---------------------------------------------------------
    bullied_school = bp625_entry(
      "Ever been bullied on school property, past 12 months (1 = Yes, 2 = No)",
      "2009" = "q22",
      "2011" = "q22",
      "2013" = "q24",
      "2015" = "q24",
      "2017" = "q23",
      "2019" = "q23"
    ),
    bullied_electronic = bp625_entry(
      "Ever been electronically bullied, past 12 months (1 = Yes, 2 = No)",
      "2011" = "q23",
      "2013" = "q25",
      "2015" = "q25",
      "2017" = "q24",
      "2019" = "q24"
    ),
    physical_fight = bp625_entry(
      "How many times were you in a physical fight, past 12 months (1 = 0 times ... 8 = 12 or more)",
      "1999" = "q17",
      "2003" = "q18",
      "2005" = "q18",
      "2007" = "q18",
      "2009" = "q17",
      "2011" = "q17",
      "2013" = "q18",
      "2015" = "q18",
      "2017" = "q17",
      "2019" = "q17"
    ),
    truancy = bp625_entry(
      "During the past 30 days, on how many days did you miss classes or school without permission? (1 = 0 days ... 5 = 10 or more days)",
      "2005" = "q97"
    ),
    # --- school, health and sleep ------------------------------------------
    grades_school = bp625_entry(
      "How would you describe your grades in school, past 12 months (1 = mostly A's ... 5 = mostly F's, 6 = none of these, 7 = not sure)",
      "2003" = "q7",
      "2009" = "q98",
      "2015" = "q89",
      "2017" = "q89",
      "2019" = "q89"
    ),
    self_rated_health = bp625_entry(
      "How do you describe your health in general? (1 = Excellent ... 5 = Poor)",
      "2005" = "q7",
      "2007" = "q98"
    ),
    sleep_hours = bp625_entry(
      "On an average school night, how many hours of sleep do you get? (1 = 4 or less ... 7 = 10 or more)",
      "2007" = "q97",
      "2009" = "q97",
      "2011" = "q96",
      "2013" = "q92",
      "2015" = "q88",
      "2017" = "q88",
      "2019" = "q88"
    ),
    weight_perception = bp625_entry(
      "How do you describe your weight? (1 = Very underweight, 2 = Slightly underweight, 3 = About the right weight, 4 = Slightly overweight, 5 = Very overweight)",
      "1999" = "q65",
      "2003" = "q66",
      "2005" = "q64",
      "2007" = "q65",
      "2009" = "q65",
      "2011" = "q67",
      "2013" = "q66",
      "2015" = "q69",
      "2017" = "q68",
      "2019" = "q67"
    ),
    bmi_percentile = bp625_entry(
      "Body mass index percentile for age and sex, computed by CDC",
      "2005" = "bmipct",
      "2007" = "bmipct",
      "2009" = "bmipct",
      "2011" = "bmipct",
      "2013" = "bmipct",
      "2015" = "bmipct",
      "2017" = "bmipct",
      "2019" = "bmipct"
    ),
    # --- geography and survey design ---------------------------------------
    metro_status = bp625_entry(
      "Metropolitan status of the school (1 = Urban, 2 = Suburban, 3 = Rural)",
      "1999" = "metrost",
      "2003" = "metrost"
    ),
    census_region = bp625_entry(
      "Geographic region (1 = Northeast, 2 = Midwest, 3 = South, 4 = West)",
      "1999" = "greg",
      "2003" = "greg"
    ),
    survey_weight = bp625_entry(
      "Student sampling weight",
      "1999" = "weight",
      "2003" = "weight",
      "2005" = "weight",
      "2007" = "weight",
      "2009" = "weight",
      "2011" = "weight",
      "2013" = "weight",
      "2015" = "weight",
      "2017" = "weight",
      "2019" = "weight"
    ),
    stratum = bp625_entry(
      "Sampling stratum",
      "1999" = "stratum",
      "2003" = "stratum",
      "2005" = "stratum",
      "2007" = "stratum",
      "2009" = "stratum",
      "2011" = "stratum",
      "2013" = "stratum",
      "2015" = "stratum",
      "2017" = "stratum",
      "2019" = "stratum"
    ),
    psu = bp625_entry(
      "Primary sampling unit",
      "1999" = "psu",
      "2003" = "psu",
      "2005" = "psu",
      "2007" = "psu",
      "2009" = "psu",
      "2011" = "psu",
      "2013" = "psu",
      "2015" = "psu",
      "2017" = "psu",
      "2019" = "psu"
    )
  )

  waves <- names(bp625_sources())
  columns <- lapply(
    stats::setNames(waves, waves),
    function(wave) vapply(entries, `[[`, character(1), wave)
  )

  tibble::as_tibble(c(
    list(
      stem = names(entries),
      label = vapply(entries, `[[`, character(1), "label")
    ),
    columns
  ))
}

#' Pull one survey year's mapped columns out of its Access table
bp625_wave_frame <- function(raw_dataset, wave, items) {
  tbl <- raw_dataset$data[[bp625_sources()[[wave]]]]
  # 1999, 2003 and the 2017 `XXHqn` table name their columns in upper case
  names(tbl) <- tolower(names(tbl))

  sources <- items[[wave]]
  present <- !is.na(sources)
  out <- tibble::as_tibble(stats::setNames(
    as.list(tbl[sources[present]]),
    items$stem[present]
  ))

  # Only 2013 onwards carries CDC's `record` number; the earlier files hold no
  # student identifier, so the row's position in the file stands in for one.
  serial <- if ("record" %in% names(out)) out$record else seq_len(nrow(out))

  dplyr::mutate(
    out,
    participant_id = paste0(wave, "_", sprintf("%.0f", serial)),
    .wave = tbl$.wave,
    .wave_label = tbl$.wave_label,
    .before = 1
  )
}

#' Put the question wording back as each column's label
bp625_label <- function(df, items) {
  attr(df$participant_id, "label") <- "Survey year and student record number"

  for (i in seq_len(nrow(items))) {
    stem <- items$stem[[i]]
    attr(df[[stem]], "label") <- items$label[[i]]
  }

  df
}
