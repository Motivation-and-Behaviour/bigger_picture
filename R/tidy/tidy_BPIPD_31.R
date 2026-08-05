#' Tidier for BPIPD-31 (FFCWS, ICPSR 31622 public use file)
#'
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per wave
tidy_BPIPD_31 <- function(raw_dataset, spec) {
  raw <- raw_dataset$data[[1]]

  constants <- bp31_constant_columns()
  wave_map <- bp31_wave_columns()

  fixed <- tibble::tibble(
    participant_id = as.character(raw[["IDNUM"]])
  )
  for (stem in names(constants)) {
    fixed[[stem]] <- bp31_decode(raw[[constants[[stem]]]])
  }

  stacked <- dplyr::bind_rows(lapply(
    names(bp31_waves()),
    function(wave) bp31_wave_frame(raw, fixed, wave, wave_map)
  ))

  stacked$pcg_relationship <- dplyr::if_else(
    stacked$pcg_relationship > 2,
    3,
    stacked$pcg_relationship
  )

  bp31_apply_labels(stacked, raw, constants, wave_map)
}

#' Waves carried into the harmonisation, in collection order
bp31_waves <- function() {
  c(
    y3 = "Year 3",
    y5 = "Year 5",
    y9 = "Year 9",
    y15 = "Year 15"
  )
}

#' Columns that describe the child once, not once per wave
bp31_constant_columns <- function() {
  c(
    sex = "CM1BSEX",
    ethnicity_mother = "CM1ETHRACE",
    ethnicity_youth = "CK6ETHRACE"
  )
}

#' Source column for each tidied stem at each wave
#'
#' `NA` means the wave did not ask the item. Stems are grouped so that every
#' wave contributing to a stem used the same response format; where a construct
#' changes format across waves (weekday screen-time bands at Year 9 versus
#' hours at Year 15) it gets separate stems instead.
bp31_wave_columns <- function() {
  entries <- list(
    # --- provenance and demographics ---------------------------------------
    interview_year = bp31_entry(
      "Year of the interview supplying this wave's screen-use items",
      y3 = "CH3MESYR",
      y5 = "CH4INTYR",
      y9 = "CP5INTYR",
      y15 = "CK6INTYR"
    ),
    age_months = bp31_entry(
      "Child age at interview (months)",
      y3 = "CM3B_AGE",
      y5 = "CM4B_AGE",
      y9 = "CH5AGEM",
      y15 = "CK6YAGEM"
    ),
    age_months_alt = bp31_entry(
      "Child age at the parallel wave component (months), used where age_months is missing",
      y5 = "CH4AGEMOS",
      y9 = "CM5B_AGE",
      y15 = "CP6YAGEM"
    ),
    pcg_relationship = bp31_entry(
      "Primary caregiver's relationship to the child, collapsed to a wave-comparable coding",
      y3 = "CP3PCGREL",
      y5 = "CH4PCGREL",
      y9 = "CP5PCGREL",
      y15 = "CP6PCGREL"
    ),
    poverty_category = bp31_entry(
      "Household income as a percentage of the federal poverty line (banded)",
      y3 = "CM3POVCA",
      y5 = "CM4POVCA",
      y9 = "CM5POVCA",
      y15 = "CP6POVCA"
    ),
    poverty_ratio = bp31_entry(
      "Household income to poverty threshold ratio",
      y3 = "CM3POVCO",
      y5 = "CM4POVCO",
      y9 = "CM5POVCO",
      y15 = "CP6POVCO"
    ),
    mother_education = bp31_entry(
      "Mother's education (Years 3-9; Year 15 has no mother-specific measure)",
      y3 = "CM3EDU",
      y5 = "CM4EDU",
      y9 = "CM5EDU"
    ),
    father_education = bp31_entry(
      "Father's education (Years 3-9; Year 15 has no father-specific measure)",
      y3 = "CF3EDU",
      y5 = "CF4EDU",
      y9 = "CF5EDU"
    ),
    pcg_education = bp31_entry(
      "Primary caregiver's education (Year 15 only); read with pcg_relationship",
      y15 = "CP6EDU"
    ),
    school_grade = bp31_entry(
      "School grade the child is enrolled in",
      y9 = "P5L1",
      y15 = "P6C3"
    ),
    # --- family structure ---------------------------------------------------
    married_to_other_parent = bp31_entry(
      "Reporting parent is married to the child's other biological parent",
      y3 = "CM3MARF",
      y5 = "CM4MARF",
      y9 = "CM5MARF",
      y15 = "CP6PMARB"
    ),
    cohabiting_with_other_parent = bp31_entry(
      "Reporting parent is cohabiting with the child's other biological parent",
      y3 = "CM3COHF",
      y5 = "CM4COHF",
      y9 = "CM5COHF",
      y15 = "CP6PCOHB"
    ),
    married_to_new_partner = bp31_entry(
      "Reporting parent is married to a partner who is not the child's parent",
      y3 = "CM3MARP",
      y5 = "CM4MARP",
      y9 = "CM5MARP",
      y15 = "CP6PMARP"
    ),
    cohabiting_with_new_partner = bp31_entry(
      "Reporting parent is cohabiting with a partner who is not the child's parent",
      y3 = "CM3COHP",
      y5 = "CM4COHP",
      y9 = "CM5COHP",
      y15 = "CP6PCOHP"
    ),
    living_arrangement = bp31_entry(
      "Youth report of who they live with (Year 15 only)",
      y15 = "CK6LIVAR"
    ),
    # --- screen use, primary caregiver report (Years 3, 5, 9) ---------------
    pcg_tv_hours_wd = bp31_entry(
      "PCG report: hours/day child watches TV or videos on a weekday",
      y3 = "P3B1",
      y5 = "P4B1",
      y9 = "P5I3"
    ),
    pcg_tv_hours_we = bp31_entry(
      "PCG report: hours/day child watches TV or videos on a weekend day",
      y3 = "P3B1A",
      y5 = "P4B2",
      y9 = "P5I4"
    ),
    pcg_game_hours_wd = bp31_entry(
      "PCG report: hours/day child plays video or computer games on a weekday",
      y5 = "P4B3"
    ),
    pcg_game_hours_we = bp31_entry(
      "PCG report: hours/day child plays video or computer games on a weekend day",
      y5 = "P4B4"
    ),
    pcg_computer_hours = bp31_entry(
      "PCG report: hours/day child uses a computer",
      y9 = "P5I18B"
    ),
    pcg_has_computer = bp31_entry(
      "PCG report: household has a computer (gates pcg_computer_hours)",
      y9 = "P5I17"
    ),
    # --- screen use, child self-report, weekday bands (Year 9) --------------
    child_band_tv_wd = bp31_entry(
      "Child self-report band: weekday time watching TV and movies",
      y9 = "K5D1G"
    ),
    child_band_game_wd = bp31_entry(
      "Child self-report band: weekday time playing computer games on computer or TV",
      y9 = "K5D1F"
    ),
    child_band_schoolwork_wd = bp31_entry(
      "Child self-report band: weekday time doing school work on the computer",
      y9 = "K5D1D"
    ),
    child_band_chat_wd = bp31_entry(
      "Child self-report band: weekday time chatting with friends on the computer",
      y9 = "K5D1E"
    ),
    # --- screen use, youth self-report, weekday hours (Year 15) -------------
    youth_tv_hours_wd = bp31_entry(
      "Youth self-report: hours/day watching TV, videos or movies on a weekday",
      y15 = "K6D36G"
    ),
    youth_game_hours_wd = bp31_entry(
      "Youth self-report: hours/day playing games on computer, TV or device on a weekday",
      y15 = "K6D36E"
    ),
    youth_internet_hours_wd = bp31_entry(
      "Youth self-report: hours/day visiting websites or shopping online on a weekday",
      y15 = "K6D36F"
    ),
    youth_ecomm_hours_wd = bp31_entry(
      "Youth self-report: hours/day communicating electronically with friends on a weekday",
      y15 = "K6D36D"
    ),
    # --- cognitive and academic assessments ---------------------------------
    ppvt_standard = bp31_entry(
      "PPVT-III receptive vocabulary standard score",
      y3 = "CH3PPVTSTD",
      y5 = "CH4PPVTSTD",
      y9 = "CH5PPVTSS"
    ),
    wj_reading_standard = bp31_entry(
      "Woodcock-Johnson reading standard score (Letter-Word Y5, Passage Comprehension Y9)",
      y5 = "CH4WJSS22",
      y9 = "CH5WJ9SS"
    ),
    wj_maths_standard = bp31_entry(
      "Woodcock-Johnson Applied Problems standard score",
      y9 = "CH5WJ10SS"
    ),
    digit_span_scaled = bp31_entry(
      "WISC-IV Digit Span scaled score",
      y9 = "CH5DSSS"
    ),
    grade_english = bp31_entry(
      "Youth self-report: most recent grade in English or language arts",
      y15 = "K6B20A"
    ),
    grade_maths = bp31_entry(
      "Youth self-report: most recent grade in math",
      y15 = "K6B20B"
    ),
    grade_history = bp31_entry(
      "Youth self-report: most recent grade in history or social studies",
      y15 = "K6B20C"
    ),
    grade_science = bp31_entry(
      "Youth self-report: most recent grade in science",
      y15 = "K6B20D"
    ),
    # --- peer victimisation and fighting ------------------------------------
    bullied_mean = bp31_entry(
      "Self-report: how often other kids picked on you or said mean things (Year 9 asks about the past month, Year 15 about the school year)",
      y9 = "K5E2A",
      y15 = "K6B32A"
    ),
    bullied_hit = bp31_entry(
      "Self-report: how often other kids hit you or threatened to hurt you (Year 9 asks about hitting only; Year 15 adds threats)",
      y9 = "K5E2B",
      y15 = "K6B32B"
    ),
    bullied_took = bp31_entry(
      "Self-report: how often other kids took your things (Year 9 asks about the past month, Year 15 about the school year)",
      y9 = "K5E2C",
      y15 = "K6B32E"
    ),
    bullied_excluded = bp31_entry(
      "Self-report: how often other kids purposely left you out of activities (Year 9 asks about the past month, Year 15 about the school year)",
      y9 = "K5E2D",
      y15 = "K6B32F"
    ),
    fist_fight = bp31_entry(
      "Child self-report: ever had a fist fight with another person (Year 9 only)",
      y9 = "K5F1E"
    ),
    serious_fight = bp31_entry(
      "Youth self-report: times gotten into a serious physical fight (Year 15 only)",
      y15 = "K6D61D"
    ),
    homework_trouble = bp31_entry(
      "Youth self-report: how often had trouble getting homework done (Year 15 only)",
      y15 = "K6B21C"
    ),
    follows_through = bp31_entry(
      "Child self-report: I follow things through to the end (Year 9 only)",
      y9 = "K5G1E"
    ),
    makes_friends_easily = bp31_entry(
      "Youth self-report: I make friends easily (Year 15 only)",
      y15 = "K6D1E"
    ),
    # --- Year 15 teen CES-D short form (5 items, 1 strongly agree - 4 strongly disagree)
    cesd_blues = bp31_entry(
      "CES-D: I feel I cannot shake off the blues, even with help",
      y15 = "K6D2C"
    ),
    cesd_sad = bp31_entry(
      "CES-D: I feel sad",
      y15 = "K6D2N"
    ),
    cesd_happy = bp31_entry(
      "CES-D: I feel happy (positively worded, reverse scored)",
      y15 = "K6D2S"
    ),
    cesd_not_worth_living = bp31_entry(
      "CES-D: I feel life is not worth living",
      y15 = "K6D2X"
    ),
    cesd_depressed = bp31_entry(
      "CES-D: I feel depressed",
      y15 = "K6D2AC"
    )
  )

  waves <- names(bp31_waves())
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

#' One row of the column map: a human-readable label plus the source column
#' at each wave that measured it
bp31_entry <- function(
  label,
  y3 = NA_character_,
  y5 = NA_character_,
  y9 = NA_character_,
  y15 = NA_character_
) {
  list(label = label, y3 = y3, y5 = y5, y9 = y9, y15 = y15)
}
#' Build one wave's slice of the tidied table
bp31_wave_frame <- function(raw, fixed, wave, wave_map) {
  sources <- wave_map[[wave]]
  stems <- wave_map$stem

  present <- !is.na(sources)
  wave_data <- tibble::as_tibble(stats::setNames(
    lapply(sources[present], function(nm) bp31_decode(raw[[nm]])),
    stems[present]
  ))

  markers <- intersect(
    c("interview_year", bp31_screen_use_stems()),
    names(wave_data)
  )
  observed <- Reduce(`|`, lapply(wave_data[markers], function(x) !is.na(x)))

  out <- dplyr::bind_cols(
    fixed,
    tibble::tibble(wave = unname(bp31_waves()[[wave]])),
    wave_data
  )
  out[observed, , drop = FALSE]
}

#' Stems holding a reported quantity of screen use
bp31_screen_use_stems <- function() {
  c(
    "pcg_tv_hours_wd",
    "pcg_tv_hours_we",
    "pcg_game_hours_wd",
    "pcg_game_hours_we",
    "pcg_computer_hours",
    "child_band_tv_wd",
    "child_band_game_wd",
    "child_band_schoolwork_wd",
    "child_band_chat_wd",
    "youth_tv_hours_wd",
    "youth_game_hours_wd",
    "youth_internet_hours_wd",
    "youth_ecomm_hours_wd"
  )
}

#' Turn an ICPSR factor back into its original numeric code
bp31_decode <- function(x) {
  if (is.factor(x)) {
    x <- sub("^\\((-?)0*([0-9]+)\\).*$", "\\1\\2", as.character(x))
  }
  x <- suppressWarnings(as.numeric(x))
  x[!is.na(x) & x < 0] <- NA_real_
  x
}

#' Recover the value labels of an ICPSR factor as a named code vector
bp31_value_labels <- function(x) {
  if (!is.factor(x)) {
    return(NULL)
  }

  parts <- regmatches(
    levels(x),
    regexec(
      "^\\((-?[0-9]+)\\)\\s*(?:-?[0-9]+\\s+)?(.*)$",
      levels(x),
      perl = TRUE
    )
  )
  parsed <- parts[lengths(parts) == 3L]
  if (length(parsed) == 0L) {
    return(NULL)
  }

  codes <- as.numeric(vapply(parsed, `[[`, character(1), 2L))
  labels <- trimws(vapply(parsed, `[[`, character(1), 3L))
  keep <- !is.na(codes) & codes >= 0 & nzchar(labels)
  if (!any(keep)) {
    return(NULL)
  }

  stats::setNames(codes[keep], labels[keep])
}

#' Re-attach variable and value labels after the waves have been stacked
bp31_apply_labels <- function(tidied, raw, constants, wave_map) {
  attr(tidied$participant_id, "label") <- "Encrypted FFCWS family ID"
  attr(tidied$wave, "label") <- "FFCWS follow-up wave"

  constant_labels <- c(
    sex = "Focal child's sex, recorded at baseline",
    ethnicity_mother = "Mother's race/ethnicity, own report at baseline",
    ethnicity_youth = "Youth's self-described race/ethnicity at Year 15"
  )
  for (stem in names(constants)) {
    tidied[[stem]] <- bp31_label_column(
      tidied[[stem]],
      constant_labels[[stem]],
      bp31_value_labels(raw[[constants[[stem]]]])
    )
  }

  wave_ids <- names(bp31_waves())
  for (i in seq_len(nrow(wave_map))) {
    stem <- wave_map$stem[[i]]
    sources <- unlist(wave_map[i, wave_ids], use.names = FALSE)
    sources <- sources[!is.na(sources)]

    value_labels <- NULL
    for (nm in rev(sources)) {
      value_labels <- bp31_value_labels(raw[[nm]])
      if (!is.null(value_labels)) {
        break
      }
    }
    if (stem == "pcg_relationship") {
      value_labels <- c(
        "biological mother" = 1,
        "biological father" = 2,
        "other caregiver" = 3
      )
    }

    tidied[[stem]] <- bp31_label_column(
      tidied[[stem]],
      wave_map$label[[i]],
      value_labels
    )
  }

  tidied
}

bp31_label_column <- function(x, label, value_labels) {
  if (is.null(value_labels)) {
    attr(x, "label") <- label
    return(x)
  }
  haven::labelled(x, labels = value_labels, label = label)
}
