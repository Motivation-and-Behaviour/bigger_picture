#' Tidier for BPIPD-31 (FFCWS, ICPSR 31622 public use file)
#'
#' Slices each wave out of the single wide release using the column map in
#' `bp31_wave_columns()`, stacks the slices long, fills parent education and
#' clears Woodcock-Johnson floor scores, then `bp31_apply_labels()` re-attaches
#' labels.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per wave
tidy_BPIPD_31 <- function(raw_dataset, spec) {
  if (length(raw_dataset$data) != 1L) {
    stop(
      "BPIPD-31: expected one data resource, the ICPSR .rda release.",
      call. = FALSE
    )
  }
  raw <- bp31_add_cbcl_scores(raw_dataset$data[[1]])
  raw <- bp31_add_parentstress_mother(raw)

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
  stacked <- bp31_fill_parent_education(stacked)
  stacked <- bp31_drop_wj_floor(stacked)

  if (anyDuplicated(stacked[c("participant_id", "wave")]) > 0L) {
    stop(
      "BPIPD-31: `participant_id` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  bp31_apply_labels(stacked, raw, constants, wave_map)
}

#' Waves carried into the harmonisation, in collection order
#'
#' Baseline/Year 1 ask no screen-use item; Year 22 respondents are adults,
#' outside the 0-19 range.
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
    ethnicity_youth = "CK6ETHRACE",
    mother_born_us = "M1H2",
    father_born_us = "F1H2"
  )
}

#' Source column for each tidied stem at each wave
#'
#' `NA` means the wave didn't ask the item. Stems are grouped so every wave
#' contributing to one used the same response format; a construct that
#' changes format across waves (e.g. weekday bands at Year 9 vs hours at
#' Year 15) gets separate stems instead.
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
      "Mother's education (Years 3-9; Year 15 from the PCG's education where the PCG is the biological mother; missing waves carry the latest earlier value)",
      y3 = "CM3EDU",
      y5 = "CM4EDU",
      y9 = "CM5EDU"
    ),
    father_education = bp31_entry(
      "Father's education (Years 3-9; Year 15 from the PCG's education where the PCG is the biological father; missing waves carry the latest earlier value)",
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
    # --- CBCL subscales, mean item score 0-2 (scored in bp31_cbcl_mean) -----
    cbcl_anxdep = bp31_entry(
      "CBCL Anxious/Depressed syndrome scale, caregiver report, mean item score",
      y3 = "BP31CBCL_Y3_ANXDEP",
      y5 = "BP31CBCL_Y5_ANXDEP",
      y15 = "BP31CBCL_Y15_ANXDEP"
    ),
    cbcl_withdrawn = bp31_entry(
      "CBCL Withdrawn syndrome scale, caregiver report, mean item score",
      y3 = "BP31CBCL_Y3_WITHDRAWN",
      y5 = "BP31CBCL_Y5_WITHDRAWN",
      y15 = "BP31CBCL_Y15_WITHDRAWN"
    ),
    cbcl_attention = bp31_entry(
      "CBCL Attention Problems syndrome scale, caregiver report, mean item score",
      y3 = "BP31CBCL_Y3_ATTENTION",
      y5 = "BP31CBCL_Y5_ATTENTION",
      y15 = "BP31CBCL_Y15_ATTENTION"
    ),
    cbcl_aggressive = bp31_entry(
      "CBCL Aggressive Behavior syndrome scale, caregiver report, mean item score",
      y3 = "BP31CBCL_Y3_AGGRESSIVE",
      y5 = "BP31CBCL_Y5_AGGRESSIVE",
      y15 = "BP31CBCL_Y15_AGGRESSIVE"
    ),
    cbcl_rulebreak = bp31_entry(
      "CBCL Rule-Breaking (Delinquent) syndrome scale, caregiver report, mean item score",
      y5 = "BP31CBCL_Y5_RULEBREAK",
      y15 = "BP31CBCL_Y15_RULEBREAK"
    ),
    cbcl_internalising = bp31_entry(
      "CBCL internalising score, caregiver report, mean item score across the anxious/depressed and withdrawn items",
      y3 = "BP31CBCL_Y3_INTERNALISING",
      y5 = "BP31CBCL_Y5_INTERNALISING",
      y15 = "BP31CBCL_Y15_INTERNALISING"
    ),
    cbcl_externalising = bp31_entry(
      "CBCL externalising score, caregiver report, mean item score across the aggressive and rule-breaking items (Year 3: aggressive and attention items)",
      y3 = "BP31CBCL_Y3_EXTERNALISING",
      y5 = "BP31CBCL_Y5_EXTERNALISING",
      y15 = "BP31CBCL_Y15_EXTERNALISING"
    ),
    # --- Year 15 teen BSI-18 anxiety (1 strongly agree - 4 strongly disagree)
    bsi_terror = bp31_entry(
      "BSI-18 anxiety: I have spells of terror or panic",
      y15 = "K6D2D"
    ),
    bsi_tense = bp31_entry(
      "BSI-18 anxiety: I feel tense or keyed up",
      y15 = "K6D2J"
    ),
    bsi_scared = bp31_entry(
      "BSI-18 anxiety: I get suddenly scared for no reason",
      y15 = "K6D2T"
    ),
    bsi_nervous = bp31_entry(
      "BSI-18 anxiety: I feel nervous or shaky inside",
      y15 = "K6D2AG"
    ),
    bsi_fearful = bp31_entry(
      "BSI-18 anxiety: I feel fearful",
      y15 = "K6D2AI"
    ),
    bsi_restless = bp31_entry(
      "BSI-18 anxiety: I feel so restless I can't sit still",
      y15 = "K6D2AK"
    ),
    # --- cognitive and academic assessments ---------------------------------
    ppvt_standard = bp31_entry(
      "PPVT-III receptive vocabulary standard score",
      y3 = "CH3PPVTSTD",
      y5 = "CH4PPVTSTD",
      y9 = "CH5PPVTSS"
    ),
    tvip_standard = bp31_entry(
      "TVIP (Spanish) receptive vocabulary standard score, child",
      y3 = "CH3TVIPSTD"
    ),
    wj_reading_standard = bp31_entry(
      "Woodcock-Johnson reading standard score (Letter-Word Y5, Passage Comprehension Y9); NA where the Y9 raw score is 0",
      y5 = "CH4WJSS22",
      y9 = "CH5WJ9SS"
    ),
    wj_reading_raw = bp31_entry(
      "Woodcock-Johnson III Passage Comprehension raw score (Year 9 only)",
      y9 = "CH5WJ9RAW"
    ),
    wj_maths_standard = bp31_entry(
      "Woodcock-Johnson Applied Problems standard score; NA where the raw score is 0",
      y9 = "CH5WJ10SS"
    ),
    wj_maths_raw = bp31_entry(
      "Woodcock-Johnson III Applied Problems raw score (Year 9 only)",
      y9 = "CH5WJ10RAW"
    ),
    teacher_rating_literacy = bp31_entry(
      "Teacher report: child's language and literacy skills against peers (1 far below to 5 far above average)",
      y5 = "T4A5",
      y9 = "T5C13A"
    ),
    teacher_rating_science = bp31_entry(
      "Teacher report: child's science and social studies against peers (1 far below to 5 far above average)",
      y5 = "T4A6",
      y9 = "T5C13B"
    ),
    teacher_rating_maths = bp31_entry(
      "Teacher report: child's mathematical skills against peers (1 far below to 5 far above average)",
      y5 = "T4A7",
      y9 = "T5C13C"
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
      "Self-report: how often other kids picked on you or said mean things. Year 9 asks about the past month (0 not once, 1 1-2 times, 2 once a week, 3 several times per week, 4 every day); Year 15 asks about the school year (0 never, 1 less than once a week, 2 once a week, 3 several times a week, 4 about every day, 7 n/a homeschooled), so the column carries no value labels",
      y9 = "K5E2A",
      y15 = "K6B32A"
    ),
    bullied_hit = bp31_entry(
      "Self-report: how often other kids hit you or threatened to hurt you (Year 9 asks about hitting only; Year 15 adds threats). Year 9 asks about the past month (0 not once, 1 1-2 times, 2 once a week, 3 several times per week, 4 every day); Year 15 asks about the school year (0 never, 1 less than once a week, 2 once a week, 3 several times a week, 4 about every day, 7 n/a homeschooled), so the column carries no value labels",
      y9 = "K5E2B",
      y15 = "K6B32B"
    ),
    bullied_took = bp31_entry(
      "Self-report: how often other kids took your things. Year 9 asks about the past month (0 not once, 1 1-2 times, 2 once a week, 3 several times per week, 4 every day); Year 15 asks about the school year (0 never, 1 less than once a week, 2 once a week, 3 several times a week, 4 about every day, 7 n/a homeschooled), so the column carries no value labels",
      y9 = "K5E2C",
      y15 = "K6B32E"
    ),
    bullied_excluded = bp31_entry(
      "Self-report: how often other kids purposely left you out of activities. Year 9 asks about the past month (0 not once, 1 1-2 times, 2 once a week, 3 several times per week, 4 every day); Year 15 asks about the school year (0 never, 1 less than once a week, 2 once a week, 3 several times a week, 4 about every day, 7 n/a homeschooled), so the column carries no value labels",
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
    # --- delinquency: Year 9 yes/no with no time frame; Year 15 past 12 months
    skipped_school = bp31_entry(
      "Self-report: skipped school without an excuse (Year 9 yes/no, no time frame; Year 15 a full day, this or last school year)",
      y9 = "K5F1I",
      y15 = "K6B27"
    ),
    ever_stole = bp31_entry(
      "Child self-report: taken or stolen something from another person or a store (Year 9 only)",
      y9 = "K5F1B"
    ),
    ever_took_money_home = bp31_entry(
      "Child self-report: taken money at home (Year 9 only)",
      y9 = "K5F1C"
    ),
    ever_damaged_property = bp31_entry(
      "Child self-report: purposely damaged or destroyed property that wasn't yours (Year 9 only)",
      y9 = "K5F1A"
    ),
    ever_graffiti = bp31_entry(
      "Child self-report: written things or sprayed paint on walls, sidewalks or cars (Year 9 only)",
      y9 = "K5F1N"
    ),
    ever_set_fire = bp31_entry(
      "Child self-report: purposely set fire to a building, car or other property, or tried to (Year 9 only)",
      y9 = "K5F1O"
    ),
    graffiti = bp31_entry(
      "Youth self-report: times painted graffiti on someone else's property or in a public place (Year 15 only)",
      y15 = "K6D61A"
    ),
    damaged_property = bp31_entry(
      "Youth self-report: times deliberately damaged property that didn't belong to you (Year 15 only)",
      y15 = "K6D61B"
    ),
    shoplifted = bp31_entry(
      "Youth self-report: times taken something from a store without paying (Year 15 only)",
      y15 = "K6D61C"
    ),
    stole_over_50 = bp31_entry(
      "Youth self-report: times stolen something worth more than $50 (Year 15 only)",
      y15 = "K6D61G"
    ),
    stole_under_50 = bp31_entry(
      "Youth self-report: times stolen something worth less than $50 (Year 15 only)",
      y15 = "K6D61K"
    ),
    burgled = bp31_entry(
      "Youth self-report: times gone into a house or building to steal something (Year 15 only)",
      y15 = "K6D61H"
    ),
    # --- health and wellbeing -----------------------------------------------
    self_rated_health = bp31_entry(
      "Self-report: in general, how is your health",
      y9 = "K5H1",
      y15 = "K6D3"
    ),
    school_happy_freq = bp31_entry(
      "Child self-report: how often felt happy to be at your school, past month (Year 9 only)",
      y9 = "K5E1C"
    ),
    school_happy_agree = bp31_entry(
      "Youth self-report: I am happy to be at my school (Year 15 only)",
      y15 = "K6B1C"
    ),
    pcg_dx_depanx = bp31_entry(
      "PCG report: a doctor or health professional has ever said the youth has depression or anxiety (Year 15 only)",
      y15 = "P6B5"
    ),
    pcg_rx_any = bp31_entry(
      "PCG report: youth regularly takes prescription medicine (Year 15 only; gates pcg_rx_depanx)",
      y15 = "P6B26"
    ),
    pcg_rx_depanx = bp31_entry(
      "PCG report: youth regularly takes prescription medicine for depression or anxiety (Year 15 only)",
      y15 = "P6B27_5"
    ),
    # --- survey weight, parenting stress, school connectedness, health, ------
    # --- substance use (dataschema consistency-check rows) -------------------
    nat_weight = bp31_entry(
      "Primary caregiver/family national weight for the wave",
      y3 = "P3NATWT",
      y5 = "P4NATWT",
      y9 = "P5NATWT",
      y15 = "P6NATWT"
    ),
    pcg_rated_health = bp31_entry(
      "PCG report: in general, would you say child's/youth's health is excellent to poor",
      y3 = "P3A1",
      y5 = "P4A1",
      y9 = "P5H1",
      y15 = "P6B1"
    ),
    pcg_tv_decides = bp31_entry(
      "PCG report: who decides what kinds of TV shows and movies the youth can watch (respondent, youth, or jointly; Year 15 only)",
      y15 = "P6D19"
    ),
    pcg_tv_in_bedroom = bp31_entry(
      "PCG report: child/youth has a television in the bedroom (1 yes, 2 no; Years 9 and 15 only)",
      y9 = "P5I14",
      y15 = "P6D7"
    ),
    # --- sleep duration: PCG hours (Years 5, 9); youth clock times (Year 15:
    # --- hour 1-12, minutes 0-59, 1 am / 2 pm) -------------------------------
    pcg_sleep_hours_usual = bp31_entry(
      "PCG report: hours of sleep the child usually gets (Year 5 only)",
      y5 = "P4B23"
    ),
    pcg_sleep_hours_weeknight = bp31_entry(
      "PCG report: hours the child sleeps on weeknights (Year 9 only)",
      y9 = "P5I12"
    ),
    youth_wake_school_hour = bp31_entry(
      "Youth self-report: time usually wake up on school-day mornings, hour 1-12 (Year 15 only)",
      y15 = "K6D12"
    ),
    youth_wake_school_min = bp31_entry(
      "Youth self-report: time usually wake up on school-day mornings, minutes (Year 15 only)",
      y15 = "K6D13"
    ),
    youth_wake_school_ampm = bp31_entry(
      "Youth self-report: time usually wake up on school-day mornings, am/pm (Year 15 only)",
      y15 = "K6D13A"
    ),
    youth_bed_school_hour = bp31_entry(
      "Youth self-report: time usually go to bed on school nights, hour 1-12 (Year 15 only)",
      y15 = "K6D14"
    ),
    youth_bed_school_min = bp31_entry(
      "Youth self-report: time usually go to bed on school nights, minutes (Year 15 only)",
      y15 = "K6D15"
    ),
    youth_bed_school_ampm = bp31_entry(
      "Youth self-report: time usually go to bed on school nights, am/pm (Year 15 only)",
      y15 = "K6D15A"
    ),
    youth_wake_weekend_hour = bp31_entry(
      "Youth self-report: time usually wake up on weekend mornings, hour 1-12 (Year 15 only)",
      y15 = "K6D17"
    ),
    youth_wake_weekend_min = bp31_entry(
      "Youth self-report: time usually wake up on weekend mornings, minutes (Year 15 only)",
      y15 = "K6D18"
    ),
    youth_wake_weekend_ampm = bp31_entry(
      "Youth self-report: time usually wake up on weekend mornings, am/pm (Year 15 only)",
      y15 = "K6D18A"
    ),
    youth_bed_weekend_hour = bp31_entry(
      "Youth self-report: time usually go to bed on weekend nights, hour 1-12 (Year 15 only)",
      y15 = "K6D19"
    ),
    youth_bed_weekend_min = bp31_entry(
      "Youth self-report: time usually go to bed on weekend nights, minutes (Year 15 only)",
      y15 = "K6D20"
    ),
    youth_bed_weekend_ampm = bp31_entry(
      "Youth self-report: time usually go to bed on weekend nights, am/pm (Year 15 only)",
      y15 = "K6D20A"
    ),
    parentstress_a = bp31_entry(
      "Aggravation in Parenting scale item a, 1 strongly agree - 4 strongly disagree: mother report (resident else non-resident) at Years 3 and 5, PCG report at Years 9 and 15",
      y3 = "BP31PSTRESS_MOM_Y3_A",
      y5 = "BP31PSTRESS_MOM_Y5_A",
      y9 = "P5K1A",
      y15 = "P6D32"
    ),
    parentstress_b = bp31_entry(
      "Aggravation in Parenting scale item b, 1 strongly agree - 4 strongly disagree: mother report (resident else non-resident) at Years 3 and 5, PCG report at Years 9 and 15",
      y3 = "BP31PSTRESS_MOM_Y3_B",
      y5 = "BP31PSTRESS_MOM_Y5_B",
      y9 = "P5K1B",
      y15 = "P6D33"
    ),
    parentstress_c = bp31_entry(
      "Aggravation in Parenting scale item c, 1 strongly agree - 4 strongly disagree: mother report (resident else non-resident) at Years 3 and 5, PCG report at Years 9 and 15",
      y3 = "BP31PSTRESS_MOM_Y3_C",
      y5 = "BP31PSTRESS_MOM_Y5_C",
      y9 = "P5K1C",
      y15 = "P6D34"
    ),
    parentstress_d = bp31_entry(
      "Aggravation in Parenting scale item d, 1 strongly agree - 4 strongly disagree: mother report (resident else non-resident) at Years 3 and 5, PCG report at Years 9 and 15",
      y3 = "BP31PSTRESS_MOM_Y3_D",
      y5 = "BP31PSTRESS_MOM_Y5_D",
      y9 = "P5K1D",
      y15 = "P6D35"
    ),
    school_connect_a = bp31_entry(
      "Connectedness at School scale item a (Year 9 'part of school', 0-4 past-month frequency; Year 15 'close to people at school', 1-4 agreement, code 7 = homeschooled)",
      y9 = "K5E1A",
      y15 = "K6B1A"
    ),
    school_connect_b = bp31_entry(
      "Connectedness at School scale item b (Year 9 'close to people at school', 0-4 past-month frequency; Year 15 'part of school', 1-4 agreement, code 7 = homeschooled)",
      y9 = "K5E1B",
      y15 = "K6B1B"
    ),
    school_connect_d = bp31_entry(
      "Connectedness at School scale item d: feel safe at school (Year 9 0-4 past-month frequency; Year 15 1-4 agreement, code 7 = homeschooled)",
      y9 = "K5E1D",
      y15 = "K6B1D"
    ),
    teen_tobacco_ever = bp31_entry(
      "Youth self-report: ever smoked a whole cigarette (1 yes, 2 no; Year 15 only)",
      y15 = "K6D40"
    ),
    teen_tobacco_freq30d = bp31_entry(
      "Youth self-report: frequency of smoking in the past 30 days, gated on teen_tobacco_ever (1 never - 4 6-7 days a week; Year 15 only)",
      y15 = "K6D42"
    ),
    teen_alcohol_ever = bp31_entry(
      "Youth self-report: ever had an alcoholic drink more than two or three times without a parent present (1 yes, 2 no; Year 15 only)",
      y15 = "K6D48"
    ),
    teen_alcohol_freq30d = bp31_entry(
      "Youth self-report: frequency of drinking in the past 30 days, gated on teen_alcohol_ever (1 never - 5 every day or nearly every day; Year 15 only)",
      y15 = "K6D50"
    ),
    teen_cannabis_ever = bp31_entry(
      "Youth self-report: ever tried marijuana (1 yes, 2 no; Year 15 only)",
      y15 = "K6F63"
    ),
    teen_cannabis_freqmonth = bp31_entry(
      "Youth self-report: frequency of marijuana use in the past month, gated on teen_cannabis_ever (1 never - 4 3+ days a week; Year 15 only)",
      y15 = "K6F66"
    ),
    # --- Year 15 teen EPOCH happiness items (the fourth, I feel happy, is cesd_happy)
    epoch_love_life = bp31_entry(
      "EPOCH happiness: I love life",
      y15 = "K6D2B"
    ),
    epoch_cheerful = bp31_entry(
      "EPOCH happiness: I am a cheerful person",
      y15 = "K6D2F"
    ),
    epoch_fun = bp31_entry(
      "EPOCH happiness: I have a lot of fun",
      y15 = "K6D2AA"
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
    ),
    # --- items reviewed for rows they cannot fill, kept so notes can name them
    cbcl_item_bullies = bp31_entry(
      "CBCL item, caregiver report: cruel, bullies, or shows meanness to others (0 not true, 1 sometimes, 2 often; Years 9 and 15 recoded from 1-3)",
      y5 = "BP31CBCLITEM_Y5_BULLIES",
      y9 = "BP31CBCLITEM_Y9_BULLIES",
      y15 = "BP31CBCLITEM_Y15_BULLIES"
    ),
    cbcl_item_argues = bp31_entry(
      "CBCL item, caregiver report: argues a lot (0 not true, 1 sometimes, 2 often; Years 9 and 15 recoded from 1-3)",
      y5 = "BP31CBCLITEM_Y5_ARGUES",
      y9 = "BP31CBCLITEM_Y9_ARGUES",
      y15 = "BP31CBCLITEM_Y15_ARGUES"
    ),
    cbcl_item_proud = bp31_entry(
      "CBCL-block item, caregiver report: child tends to be proud of things he/she does (0 not true, 1 sometimes, 2 often; Year 3 18 cities only)",
      y3 = "BP31CBCLITEM_Y3_PROUD",
      y5 = "BP31CBCLITEM_Y5_PROUD"
    ),
    selfdesc_angry_learning = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I feel angry when I have trouble learning something (Year 9 only)",
      y9 = "K5G2A"
    ),
    selfdesc_argue = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I often argue with other kids (Year 9 only)",
      y9 = "K5G2B"
    ),
    selfdesc_worry_tests = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I worry about taking tests (Year 9 only)",
      y9 = "K5G2C"
    ),
    selfdesc_hard_attention = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): it's hard for me to pay attention (Year 9 only)",
      y9 = "K5G2D"
    ),
    selfdesc_lonely = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I often feel lonely (Year 9 only)",
      y9 = "K5G2E"
    ),
    selfdesc_distracted = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I get distracted easily (Year 9 only)",
      y9 = "K5G2F"
    ),
    selfdesc_sad = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I feel sad a lot of the time (Year 9 only)",
      y9 = "K5G2G"
    ),
    selfdesc_hard_finish_schoolwork = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): it's hard for me to finish my schoolwork (Year 9 only)",
      y9 = "K5G2H"
    ),
    selfdesc_worry_doing_well = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I worry about doing well in school (Year 9 only)",
      y9 = "K5G2I"
    ),
    selfdesc_worry_finishing = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I worry about finishing my work (Year 9 only)",
      y9 = "K5G2J"
    ),
    selfdesc_worry_playmate = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I worry about having someone to play with (Year 9 only)",
      y9 = "K5G2K"
    ),
    selfdesc_ashamed_mistakes = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I feel ashamed when I make mistakes at school (Year 9 only)",
      y9 = "K5G2L"
    ),
    selfdesc_trouble_talking = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I get in trouble for talking and disturbing others (Year 9 only)",
      y9 = "K5G2M"
    ),
    selfdesc_trouble_fighting = bp31_entry(
      "Child self-description (0 not at all true - 3 very true): I get in trouble for fighting with other kids (Year 9 only)",
      y9 = "K5G2N"
    ),
    try_best_work = bp31_entry(
      "Child self-report: I try to do my best on all my work (0 never - 3 always; Year 9 only)",
      y9 = "K5G1D"
    ),
    teacher_works_best = bp31_entry(
      "Teacher report: how often the child works to the best of his/her ability (1 never - 4 always)",
      y5 = "T4A8",
      y9 = "T5C16"
    ),
    teacher_not_finish_homework = bp31_entry(
      "Teacher report: child does not follow through on instructions and fails to finish homework (0 not true - 3 very much true; Year 9 only)",
      y9 = "T5B4Z"
    ),
    epoch_involved = bp31_entry(
      "EPOCH engagement: I get so involved in activities that I forget about everything else",
      y15 = "K6D2E"
    ),
    epoch_absorbed = bp31_entry(
      "EPOCH engagement: I get completely absorbed in what I am doing",
      y15 = "K6D2H"
    ),
    epoch_lose_track_activity = bp31_entry(
      "EPOCH engagement: when I do an activity, I enjoy it so much that I lose track of time",
      y15 = "K6D2U"
    ),
    epoch_lose_track_learning = bp31_entry(
      "EPOCH engagement: when I am learning something new, I lose track of time",
      y15 = "K6D2AD"
    ),
    epoch_keep_at_schoolwork = bp31_entry(
      "EPOCH perseverance: I keep at my schoolwork until I am done with it",
      y15 = "K6D2I"
    ),
    epoch_stick_to_plan = bp31_entry(
      "EPOCH perseverance: once I make a plan to get something done, I stick to it",
      y15 = "K6D2K"
    ),
    epoch_finish_begin = bp31_entry(
      "EPOCH perseverance: I finish whatever I begin",
      y15 = "K6D2M"
    ),
    epoch_hard_worker = bp31_entry(
      "EPOCH perseverance: I am a hard worker",
      y15 = "K6D2V"
    ),
    epoch_good_things = bp31_entry(
      "EPOCH optimism: I think good things are going to happen to me",
      y15 = "K6D2O"
    ),
    epoch_work_out = bp31_entry(
      "EPOCH optimism: I believe that things will work out, no matter how difficult",
      y15 = "K6D2W"
    ),
    epoch_expect_best = bp31_entry(
      "EPOCH optimism: in uncertain times, I expect the best",
      y15 = "K6D2AE"
    ),
    epoch_optimistic_future = bp31_entry(
      "EPOCH optimism: I am optimistic about my future",
      y15 = "K6D2AH"
    ),
    epoch_friends_care = bp31_entry(
      "EPOCH connectedness: I have friends that I really care about",
      y15 = "K6D2G"
    ),
    epoch_people_care = bp31_entry(
      "EPOCH connectedness: there are people in my life who really care about me",
      y15 = "K6D2L"
    ),
    epoch_someone_there = bp31_entry(
      "EPOCH connectedness: when I have a problem, I have someone who will be there for me",
      y15 = "K6D2Y"
    ),
    epoch_share_news = bp31_entry(
      "EPOCH connectedness: when something good happens to me, I have people to share news with",
      y15 = "K6D2AF"
    ),
    pcg_youth_internet_phone = bp31_entry(
      "PCG report: youth accesses the internet through a mobile phone (1 yes, 2 no; Year 15 only)",
      y15 = "P6D11"
    ),
    pcg_youth_internet_tablet = bp31_entry(
      "PCG report: youth accesses the internet through an iPad or tablet (1 yes, 2 no; Year 15 only)",
      y15 = "P6D10"
    ),
    cbcl_item_selfharm = bp31_entry(
      "CBCL item, caregiver report: deliberately harms self or attempts suicide (1 not true - 3 very true or often true; Year 9 only)",
      y9 = "P5Q3Q"
    ),
    sleep_trouble_falling_nights = bp31_entry(
      "Youth self-report: nights during the week with problems falling asleep (0-7; Year 15 only)",
      y15 = "K6D22"
    ),
    sleep_trouble_staying_nights = bp31_entry(
      "Youth self-report: nights during the week with problems staying asleep (0-7; Year 15 only)",
      y15 = "K6D23"
    ),
    youth_aian_specified = bp31_entry(
      "Youth specified American Indian or Alaska Native in the race item (1 yes; Year 15 coding flag)",
      y15 = "K6C00AC"
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
  absent <- setdiff(sources[present], names(raw))
  if (length(absent) > 0L) {
    stop(
      paste0(
        "BPIPD-31: column map names ",
        paste(absent, collapse = ", "),
        " at ",
        wave,
        ", not present in the release."
      ),
      call. = FALSE
    )
  }
  wave_data <- tibble::as_tibble(stats::setNames(
    lapply(sources[present], function(nm) bp31_decode(raw[[nm]])),
    stems[present]
  ))

  # Keep a participant-wave only if it recorded an interview year or a
  # reported quantity of screen use
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

#' Parent education at every wave
#'
#' Year 15 asks only the PCG's own education, which counts for the parent who
#' is the PCG. Any wave still missing a parent's education takes the child's
#' latest earlier value.
bp31_fill_parent_education <- function(tidied) {
  at_y15 <- tidied$wave == bp31_waves()[["y15"]]
  parent_codes <- c(mother_education = 1, father_education = 2)
  sorted <- order(tidied$participant_id, match(tidied$wave, bp31_waves()))

  for (stem in names(parent_codes)) {
    is_pcg <- at_y15 & tidied$pcg_relationship %in% parent_codes[[stem]]
    tidied[[stem]][is_pcg] <- tidied$pcg_education[is_pcg]

    carried <- tibble::tibble(
      id = tidied$participant_id[sorted],
      value = tidied[[stem]][sorted]
    ) |>
      dplyr::group_by(id) |>
      tidyr::fill(value, .direction = "down") |>
      dplyr::ungroup()
    tidied[[stem]][sorted] <- carried$value
  }

  tidied
}

#' Woodcock-Johnson III standard scores are not interpretable at a raw score
#' of 0, so those are cleared
bp31_drop_wj_floor <- function(tidied) {
  tidied$wj_reading_standard[tidied$wj_reading_raw %in% 0] <- NA_real_
  tidied$wj_maths_standard[tidied$wj_maths_raw %in% 0] <- NA_real_
  tidied
}

#' Turn an ICPSR factor back into its original numeric code
#'
#' FFCWS codes missingness negatively throughout (-10 n/a through -1 refuse).
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
    ethnicity_youth = "Youth's self-described race/ethnicity at Year 15",
    mother_born_us = "Mother born in the US, own report at baseline",
    father_born_us = "Father born in the US, own report at baseline (missing where the father was not interviewed)"
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

    value_labels <- bp31_agreed_value_labels(raw[sources])
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

#' Value labels for a stem
bp31_agreed_value_labels <- function(columns) {
  labels <- Filter(Negate(is.null), lapply(columns, bp31_value_labels))
  if (length(labels) == 0L) {
    return(NULL)
  }
  if (length(labels) > 1L) {
    shared <- Reduce(intersect, lapply(labels, unname))
    wordings <- lapply(labels, function(codes) {
      bp31_normalise_label(names(codes)[match(shared, unname(codes))])
    })
    if (length(shared) == 0L || length(unique(wordings)) > 1L) {
      return(NULL)
    }
  }
  labels[[length(labels)]]
}

#' Reduce a value label to the wording alone, for comparison across waves
bp31_normalise_label <- function(x) {
  trimws(gsub("[^a-z0-9]+", " ", tolower(x)))
}

bp31_label_column <- function(x, label, value_labels) {
  if (is.null(value_labels)) {
    attr(x, "label") <- label
    return(x)
  }
  haven::labelled(x, labels = value_labels, label = label)
}

#' CBCL items making up each subscale, by wave
#'
#' Item tables from the user guides: Year 3 Table 27 (CBCL/1.5-5, 2000), Year
#' 5 Table 27 (CBCL/4-18, 1991), Year 15 Table 18 (CBCL/6-18). Year 9
#' administered the most CBCL items (111) but its user guide prints only
#' three example items per subscale and points researchers to
#' FFData@princeton.edu for the full list, so it's deliberately absent here
#' rather than guessed at.
#'
#' Subscales available differ by age form: the Year 3 preschool form has no
#' rule-breaking scale, and FFCWS asked 3 of its 5 attention problems items
#' (18 cities only; the user guide files them under 'ADHD').
bp31_cbcl_items <- function() {
  list(
    y3 = list(
      attention = c("P3M2A", "P3M2B", "P3M28A"),
      anxdep = c(
        "P3M3",
        "P3M16",
        "P3M19",
        "P3M22",
        "P3M25",
        "P3M32",
        "P3M42",
        "P3M46"
      ),
      withdrawn = c(
        "P3M1",
        "P3M2",
        "P3M9",
        "P3M29",
        "P3M31",
        "P3M35",
        "P3M36",
        "P3M50"
      ),
      aggressive = c(
        "P3M2C",
        "P3M5",
        "P3M6",
        "P3M6B",
        "P3M7",
        "P3M13",
        "P3M14",
        "P3M18",
        "P3M21",
        "P3M21A",
        "P3M23",
        "P3M26A",
        "P3M28",
        "P3M30",
        "P3M33",
        "P3M39",
        "P3M41",
        "P3M44",
        "P3M48"
      )
    ),
    y5 = list(
      anxdep = c(
        "P4L5",
        "M4B4B4",
        "P4L17",
        "P4L18",
        "P4L19",
        "P4L20",
        "M4B4B18",
        "M4B4B9",
        "M4B4B14",
        "P4L29",
        "P4L43",
        "P4L53",
        "M4B4B15",
        "P4L65"
      ),
      withdrawn = c(
        "P4L25",
        "P4L38",
        "P4L42",
        "P4L46",
        "P4L47",
        "P4L52",
        "P4L61",
        "M4B4B15",
        "M4B4B17"
      ),
      attention = c(
        "M4B4B19",
        "M4B4B1",
        "M4B4B2",
        "P4L6",
        "P4L8",
        "P4L24",
        "M4B4B9",
        "P4L27",
        "P4L34",
        "P4L35",
        "P4L47"
      ),
      aggressive = c(
        "P4L1",
        "P4L2",
        "P4L7",
        "M4B4B16",
        "P4L9",
        "P4L10",
        "P4L12",
        "P4L13",
        "P4L16",
        "P4L21",
        "P4L33",
        "P4L40",
        "P4L45",
        "M4B4B11",
        "M4B4B12",
        "P4L56",
        "P4L57",
        "M4B4B13",
        "P4L59",
        "P4L62"
      ),
      rulebreak = c(
        "M4B4B7",
        "P4L23",
        "P4L26",
        "P4L36",
        "P4L39",
        "P4L44",
        "P4L49",
        "P4L50",
        "P4L54",
        "P4L64"
      )
    ),
    y15 = list(
      anxdep = c("P6B36", "P6B40", "P6B52", "P6B53", "P6B54", "P6B68"),
      withdrawn = c("P6B65", "P6B66"),
      attention = c("P6B46", "P6B47", "P6B48"),
      aggressive = c(
        "P6B35",
        "P6B37",
        "P6B38",
        "P6B39",
        "P6B41",
        "P6B42",
        "P6B43",
        "P6B44",
        "P6B45",
        "P6B57",
        "P6B59"
      ),
      rulebreak = c(
        "P6B49",
        "P6B50",
        "P6B51",
        "P6B60",
        "P6B61",
        "P6B62",
        "P6B63",
        "P6B64",
        "P6B67"
      )
    )
  )
}

#' Single CBCL items carried on their own, by wave
bp31_cbcl_single_items <- function() {
  list(
    bullies = c(y5 = "P4L7", y9 = "P5Q3O", y15 = "P6B35"),
    argues = c(y5 = "P4L1", y9 = "P5Q3C", y15 = "P6B59"),
    proud = c(y3 = "P3M27", y5 = "P4L60")
  )
}

#' Subscales making up each CBCL composite on a form
#'
#' Externalising follows the form's grouping: aggressive with rule-breaking,
#' or with attention problems on the preschool form, which has no
#' rule-breaking scale.
bp31_cbcl_composites <- function(subscales) {
  externalising <- if (is.null(subscales$rulebreak)) {
    c("aggressive", "attention")
  } else {
    c("aggressive", "rulebreak")
  }
  list(
    internalising = c("anxdep", "withdrawn"),
    externalising = externalising
  )
}

#' Score the CBCL subscales and composites and append them to the raw table
bp31_add_cbcl_scores <- function(raw) {
  absent <- setdiff(
    unique(c(unlist(bp31_cbcl_items()), unlist(bp31_cbcl_single_items()))),
    names(raw)
  )
  if (length(absent) > 0L) {
    stop(
      paste0(
        "BPIPD-31: CBCL items not present in the release: ",
        paste(absent, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  for (wave in names(bp31_cbcl_items())) {
    subscales <- bp31_cbcl_items()[[wave]]

    for (subscale in names(subscales)) {
      column <- paste("BP31CBCL", toupper(wave), toupper(subscale), sep = "_")
      raw[[column]] <- bp31_cbcl_mean(raw, subscales[[subscale]])
    }

    composites <- bp31_cbcl_composites(subscales)
    for (composite in names(composites)) {
      column <- paste("BP31CBCL", toupper(wave), toupper(composite), sep = "_")
      raw[[column]] <- bp31_cbcl_composite(
        raw,
        subscales[composites[[composite]]]
      )
    }
  }

  single <- bp31_cbcl_single_items()
  for (item in names(single)) {
    for (wave in names(single[[item]])) {
      column <- paste("BP31CBCLITEM", toupper(wave), toupper(item), sep = "_")
      raw[[column]] <- bp31_cbcl_item(raw, single[[item]][[wave]])
    }
  }

  raw
}

#' Mother's own Aggravation in Parenting items at Years 3 and 5
#'
#' Years 3 and 5 ask resident and non-resident mothers (and fathers)
#' separately rather than one PCG item; the mother's own report (resident
#' else non-resident) fills `parentstress_a`-`parentstress_d` at these two
#' waves. Fathers are not used here.
bp31_add_parentstress_mother <- function(raw) {
  items <- list(
    y3 = list(
      a = c("M3B6A", "M3B34A"),
      b = c("M3B6B", "M3B34B"),
      c = c("M3B6C", "M3B34C"),
      d = c("M3B6D", "M3B34D")
    ),
    y5 = list(
      a = c("M4B6A", "M4B31A"),
      b = c("M4B6B", "M4B31B"),
      c = c("M4B6C", "M4B31C"),
      d = c("M4B6D", "M4B31D")
    )
  )

  for (wave in names(items)) {
    for (item in names(items[[wave]])) {
      cols <- items[[wave]][[item]]
      column <- paste(
        "BP31PSTRESS_MOM",
        toupper(wave),
        toupper(item),
        sep = "_"
      )
      raw[[column]] <- dplyr::coalesce(
        bp31_decode(raw[[cols[[1]]]]),
        bp31_decode(raw[[cols[[2]]]])
      )
    }
  }

  raw
}

#' CBCL items on the standard 0-2 item scale, one column per item
bp31_cbcl_matrix <- function(raw, item_names) {
  do.call(
    cbind,
    lapply(item_names, function(name) bp31_cbcl_item(raw, name))
  )
}

#' Mean item score for one CBCL subscale, on the standard 0-2 item scale
bp31_cbcl_mean <- function(raw, item_names) {
  items <- bp31_cbcl_matrix(raw, item_names)

  scores <- rowMeans(items, na.rm = TRUE)
  answered <- rowSums(!is.na(items))
  scores[answered < ceiling(0.8 * length(item_names))] <- NA_real_
  scores
}

#' Mean over the union of the component subscales' items, NA unless every
#' component meets its own 80% threshold
bp31_cbcl_composite <- function(raw, components) {
  scores <- rowMeans(
    bp31_cbcl_matrix(raw, unique(unlist(components))),
    na.rm = TRUE
  )
  for (items in components) {
    scores[is.na(bp31_cbcl_mean(raw, items))] <- NA_real_
  }
  scores
}

#' One CBCL item, rescaled to 0-2 and joined across the Year 5 survey versions
bp31_cbcl_item <- function(raw, name) {
  values <- bp31_cbcl_rescale(raw[[name]])

  alternate <- sub("^M4B4B", "M4B29A", name)
  if (alternate != name) {
    values <- dplyr::coalesce(values, bp31_cbcl_rescale(raw[[alternate]]))
  }

  values
}

#' Shift a CBCL item onto 0-2 scoring, reading the offset from its value labels
bp31_cbcl_rescale <- function(column) {
  values <- bp31_decode(column)
  codes <- bp31_value_labels(column)

  if (!is.null(codes) && min(codes) == 1) {
    values <- values - 1
  }

  values
}
