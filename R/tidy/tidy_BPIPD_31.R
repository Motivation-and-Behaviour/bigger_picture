#' Tidier for BPIPD-31 (FFCWS, ICPSR 31622 public use file)
#'
#' Slices each wave out of the single wide release using the column map in
#' `bp31_wave_columns()`, stacks the slices long, fills parent education and
#' the mother's Year 15 age and work, clears Woodcock-Johnson floor scores,
#' carries a repeated grade forward, then `bp31_apply_labels()` re-attaches
#' labels. Scales whose item sets change by wave or need documented item
#' lists (CBCL, teacher SSRS and Conners, ASBI, delinquency) are scored on
#' the raw table first, and the CTSPC items are put on one coding.
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
  raw <- bp31_add_told_conditions(raw)
  raw$BP31RX_Y15_P6B27_1 <- dplyr::if_else(
    bp31_decode(raw$P6B27_1) == 0,
    2,
    bp31_decode(raw$P6B27_1)
  )
  raw <- bp31_add_teacher_scales(raw)
  raw <- bp31_add_asbi_scores(raw)
  raw <- bp31_add_delinquency_scores(raw)
  raw <- bp31_add_cts_items(raw)

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
  stacked <- bp31_fill_mother_y15(stacked)
  stacked <- bp31_drop_wj_floor(stacked)
  stacked <- bp31_carry_grade_repeated(stacked)

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
    father_born_us = "F1H2",
    city_sample = "CM1CITSM",
    prenatal_cigarettes = "M1G4",
    breastfed_mother = "M2B12",
    breastfed_father = "F2B12"
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
    school_type = bp31_entry(
      "PCG report: type of school (Year 9 the school attended last school year; Year 15 the school attends): 1 public, 2 private, 3 parochial or religious, 4 home school, 5 not attending; the wordings differ, so the column carries no value labels",
      y9 = "P5L1A",
      y15 = "P6C1"
    ),
    mother_age = bp31_entry(
      "Mother's age in completed years (Years 3-9 constructed; Year 15 the PCG's age where the PCG is the biological mother, else her Year 9 age plus the child's change in age)",
      y3 = "CM3AGE",
      y5 = "CM4AGE",
      y9 = "CM5AGE",
      y15 = "CP6AGE"
    ),
    mother_worked_last_week = bp31_entry(
      "Mother did any regular work for pay last week (her own report at Years 3-9; Year 15 the PCG's report where the PCG is the biological mother)",
      y3 = "M3K4",
      y5 = "M4K4",
      y9 = "M5I4",
      y15 = "P6K6"
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
      y9 = "BP31CBCL_Y9_ANXDEP",
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
      y9 = "BP31CBCL_Y9_ATTENTION",
      y15 = "BP31CBCL_Y15_ATTENTION"
    ),
    cbcl_aggressive = bp31_entry(
      "CBCL Aggressive Behavior syndrome scale, caregiver report, mean item score",
      y3 = "BP31CBCL_Y3_AGGRESSIVE",
      y5 = "BP31CBCL_Y5_AGGRESSIVE",
      y9 = "BP31CBCL_Y9_AGGRESSIVE",
      y15 = "BP31CBCL_Y15_AGGRESSIVE"
    ),
    cbcl_rulebreak = bp31_entry(
      "CBCL Rule-Breaking (Delinquent) syndrome scale, caregiver report, mean item score",
      y5 = "BP31CBCL_Y5_RULEBREAK",
      y9 = "BP31CBCL_Y9_RULEBREAK",
      y15 = "BP31CBCL_Y15_RULEBREAK"
    ),
    cbcl_internalising = bp31_entry(
      "CBCL internalising score, caregiver report, mean item score across the anxious/depressed and withdrawn items (Year 9: and somatic complaints items)",
      y3 = "BP31CBCL_Y3_INTERNALISING",
      y5 = "BP31CBCL_Y5_INTERNALISING",
      y9 = "BP31CBCL_Y9_INTERNALISING",
      y15 = "BP31CBCL_Y15_INTERNALISING"
    ),
    cbcl_externalising = bp31_entry(
      "CBCL externalising score, caregiver report, mean item score across the aggressive and rule-breaking items (Year 3: aggressive and attention items)",
      y3 = "BP31CBCL_Y3_EXTERNALISING",
      y5 = "BP31CBCL_Y5_EXTERNALISING",
      y9 = "BP31CBCL_Y9_EXTERNALISING",
      y15 = "BP31CBCL_Y15_EXTERNALISING"
    ),
    cbcl_withdrawn_dep_y9 = bp31_entry(
      "CBCL/6-18 Withdrawn/Depressed syndrome scale, caregiver report, mean item score (Year 9 only; scored for the internalising composite, carried here for its own row)",
      y9 = "BP31CBCL_Y9_WITHDRAWN"
    ),
    cbcl_somatic = bp31_entry(
      "CBCL Somatic Complaints syndrome scale, caregiver report, mean item score (Year 9 only; the other waves did not ask the scale)",
      y9 = "BP31CBCL_Y9_SOMATIC"
    ),
    cbcl_emotreact = bp31_entry(
      "CBCL/1.5-5 Emotionally Reactive items, caregiver report, mean item score over the 4 of 9 items asked (Year 3 only)",
      y3 = "BP31CBCL_Y3_EMOTREACT"
    ),
    cbcl_socprob = bp31_entry(
      "CBCL Social Problems syndrome scale, caregiver report, mean item score (Year 5 CBCL/4-18, Year 9 CBCL/6-18)",
      y5 = "BP31CBCL_Y5_SOCPROB",
      y9 = "BP31CBCL_Y9_SOCPROB"
    ),
    cbcl_total = bp31_entry(
      "CBCL/6-18 Total Problems, caregiver report, mean item score over the 111 problem items asked (Year 9 only)",
      y9 = "BP31CBCL_Y9_TOTAL"
    ),
    # --- Year 9 teacher survey scales (scored in bp31_add_teacher_scales) ---
    teacher_ssrs_social_skills = bp31_entry(
      "Teacher report, Social Skills Rating System total social skills (cooperation, assertion and self-control, 25 items 0-3, sum)",
      y9 = "BP31TCH_Y9_SSRS_SOCIAL"
    ),
    teacher_ssrs_externalising = bp31_entry(
      "Teacher report, Social Skills Rating System externalizing problem behaviours (6 items 0-3, sum)",
      y9 = "BP31TCH_Y9_SSRS_EXT"
    ),
    teacher_ssrs_internalising = bp31_entry(
      "Teacher report, Social Skills Rating System internalizing problem behaviours (6 items 0-3, sum)",
      y9 = "BP31TCH_Y9_SSRS_INT"
    ),
    teacher_conners_cognitive = bp31_entry(
      "Teacher report, Conners' Teacher Rating Scale-Revised short form cognitive problems/inattention (5 items 0-3, sum)",
      y9 = "BP31TCH_Y9_CONNERS_COGNITIVE"
    ),
    teacher_conners_hyperactivity = bp31_entry(
      "Teacher report, Conners' Teacher Rating Scale-Revised short form hyperactivity (7 items 0-3, sum)",
      y9 = "BP31TCH_Y9_CONNERS_HYPERACTIVITY"
    ),
    teacher_conners_adhd_index = bp31_entry(
      "Teacher report, Conners' Teacher Rating Scale-Revised short form ADHD index (12 items 0-3, sum)",
      y9 = "BP31TCH_Y9_CONNERS_ADHD"
    ),
    teacher_comforts_helps = bp31_entry(
      "Teacher report: child comforts or helps other children (1 never - 4 very often; Year 9 only)",
      y9 = "T5B1AD"
    ),
    # --- caregiver-rated social skills (scored in bp31_add_asbi_scores) ---
    pcg_asbi_mean = bp31_entry(
      "PCG report, Adaptive Social Behavior Inventory Express items (Year 9: with Social Skills Rating System assertion items), mean item score 0-2",
      y3 = "BP31ASBI_Y3",
      y5 = "BP31ASBI_Y5",
      y9 = "BP31ASBI_Y9"
    ),
    # --- youth self-report social skills, Year 15 (1 not true - 3 often true)
    youth_ssk_understands_feelings = bp31_entry(
      "Youth self-report: I understand others' feelings like when they are happy, sad, or mad",
      y15 = "K6D1A"
    ),
    youth_ssk_comforts_others = bp31_entry(
      "Youth self-report: I try to comfort others when they are upset",
      y15 = "K6D1B"
    ),
    youth_ssk_open_direct = bp31_entry(
      "Youth self-report: I am open and direct about what I want",
      y15 = "K6D1C"
    ),
    youth_ssk_joins_groups = bp31_entry(
      "Youth self-report: I join group activities without being told to",
      y15 = "K6D1D"
    ),
    youth_ssk_self_confident = bp31_entry(
      "Youth self-report: I am self-confident in social situations",
      y15 = "K6D1F"
    ),
    youth_ssk_changes_activity = bp31_entry(
      "Youth self-report: I easily change from one activity to another",
      y15 = "K6D1G"
    ),
    youth_ssk_interests = bp31_entry(
      "Youth self-report: I show interest in a variety of things",
      y15 = "K6D1H"
    ),
    youth_ssk_starts_conversations = bp31_entry(
      "Youth self-report: I start conversations rather than waiting for others to talk first",
      y15 = "K6D1I"
    ),
    youth_ssk_liked = bp31_entry(
      "Youth self-report: I am liked by others",
      y15 = "K6D1J"
    ),
    youth_ssk_invites_others = bp31_entry(
      "Youth self-report: I invite others to my home",
      y15 = "K6D1K"
    ),
    youth_ssk_reports_accidents = bp31_entry(
      "Youth self-report: I report accidents to appropriate persons",
      y15 = "K6D1L"
    ),
    # --- youth self-report impulsivity, Year 15 (Dickman dysfunctional
    # --- impulsivity items, 1 strongly agree - 4 strongly disagree) ---------
    dickman_not_thinking_over = bp31_entry(
      "Dickman impulsivity: I don't spend enough time thinking over a situation before I act",
      y15 = "K6D2A"
    ),
    dickman_without_consequences = bp31_entry(
      "Dickman impulsivity: I often say and do things without considering the consequences",
      y15 = "K6D2P"
    ),
    dickman_plans_fail = bp31_entry(
      "Dickman impulsivity: the plans I make don't work out because I haven't gone over them carefully enough",
      y15 = "K6D2R"
    ),
    dickman_mind_up_quickly = bp31_entry(
      "Dickman impulsivity: I often make up my mind without taking the time to consider the situation from all angles",
      y15 = "K6D2Z"
    ),
    dickman_says_whatever = bp31_entry(
      "Dickman impulsivity: I often say whatever comes into my head without thinking first",
      y15 = "K6D2AB"
    ),
    dickman_trouble_not_thinking = bp31_entry(
      "Dickman impulsivity: I often get into trouble because I don't think before I act",
      y15 = "K6D2AJ"
    ),
    # --- self-reported delinquency (scored in bp31_add_delinquency_scores) --
    youth_delinquency = bp31_entry(
      "Self-reported delinquency: Year 9 count of 17 'Things That You Have Done' acts ever done (yes/no); Year 15 sum of 13 Add Health acts in the past 12 months (0 never - 3 five or more times)",
      y9 = "BP31DELINQ_Y9",
      y15 = "BP31DELINQ_Y15"
    ),
    youth_other_drugs_ever = bp31_entry(
      "Youth self-report: ever tried illegal drugs other than marijuana (1 yes, 2 no; gates the drug-type items; Year 15 only)",
      y15 = "K6F68"
    ),
    youth_tried_hallucinogens = bp31_entry(
      "Youth self-report: tried hallucinogens (1 yes, 2 no; Year 15 only)",
      y15 = "K6F69A"
    ),
    youth_tried_cocaine = bp31_entry(
      "Youth self-report: tried cocaine or crack (1 yes, 2 no; Year 15 only)",
      y15 = "K6F69B"
    ),
    youth_tried_heroin = bp31_entry(
      "Youth self-report: tried heroin (1 yes, 2 no; Year 15 only)",
      y15 = "K6F69C"
    ),
    youth_tried_meth = bp31_entry(
      "Youth self-report: tried methamphetamine (1 yes, 2 no; Year 15 only)",
      y15 = "K6F69D"
    ),
    youth_tried_cough_medicine = bp31_entry(
      "Youth self-report: tried non-prescription cough or cold medicine (1 yes, 2 no; Year 15 only)",
      y15 = "K6F69E"
    ),
    youth_tried_inhalants = bp31_entry(
      "Youth self-report: tried glue, aerosol sprays, gases, fumes or sprays (1 yes, 2 no; Year 15 only)",
      y15 = "K6F69F"
    ),
    youth_tried_ecstasy = bp31_entry(
      "Youth self-report: tried ecstasy or MDMA (1 yes, 2 no; Year 15 only)",
      y15 = "K6F69F1"
    ),
    youth_tried_other_drugs = bp31_entry(
      "Youth self-report: tried other types of illegal drugs (1 yes, 2 no; Year 15 only)",
      y15 = "K6F69G"
    ),
    # --- school ---------------------------------------------------------------
    grade_repeated = bp31_entry(
      "PCG report: child has repeated a grade (Year 9 ever; Year 15 since the last interview); 1 yes, 2 no; a yes is carried to every later wave",
      y9 = "P5L15",
      y15 = "P6C25"
    ),
    youth_suspended = bp31_entry(
      "Self-report: suspended or expelled from school (Year 9 ever, no time frame; Year 15 in the past 2 years, code 7 = homeschooled)",
      y9 = "K5F1M",
      y15 = "K6B29"
    ),
    youth_homework_hours_wd = bp31_entry(
      "Youth self-report: hours spent doing homework each weekday (Year 15 only)",
      y15 = "K6B6"
    ),
    youth_homework_hours_we = bp31_entry(
      "Youth self-report: hours spent doing homework each weekend day (Year 15 only)",
      y15 = "K6B7"
    ),
    # --- doctor-diagnosed developmental conditions, PCG report (1 yes, 2 no;
    # --- Year 5 rebased by bp31_add_told_conditions) --------------------------
    dx_adhd = bp31_entry(
      "PCG report: a doctor or health professional diagnosed the child with ADD/ADHD",
      y5 = "BP31DX_Y5_P4A2_A",
      y9 = "P5H2A",
      y15 = "P6B10"
    ),
    rx_adhd = bp31_entry(
      "PCG report: child takes medication for ADD/ADHD, 1 yes, 2 no (asked of children taking prescribed medication, see rx_any; Year 15 rebased from 0 no, 1 yes)",
      y9 = "P5H3B1_1",
      y15 = "BP31RX_Y15_P6B27_1"
    ),
    rx_any = bp31_entry(
      "PCG report: child takes medicine for which a prescription was needed (Year 9) or regularly takes prescription medicine (Year 15); 1 yes, 2 no; gates rx_adhd",
      y9 = "P5H3A1",
      y15 = "P6B26"
    ),
    dx_autism = bp31_entry(
      "PCG report: a doctor or health professional diagnosed the child with autism",
      y5 = "BP31DX_Y5_P4A2_F",
      y9 = "P5H2F",
      y15 = "P6B11"
    ),
    dx_speech_language = bp31_entry(
      "PCG report: a doctor or health professional diagnosed the child with a speech or language problem (Years 5 and 9)",
      y5 = "BP31DX_Y5_P4A2_M",
      y9 = "P5H2L"
    ),
    dx_learning_disability = bp31_entry(
      "PCG report: a doctor diagnosed the youth with another learning disability (Year 15 only; specified as speech problem, developmental delay, dyslexia, reading or math difficulty, or other)",
      y15 = "P6B12"
    ),
    # --- cognitive tasks ----------------------------------------------------
    leiter_attention_scaled = bp31_entry(
      "Leiter-R Attention Sustained adjusted total correct scaled score (mean 10, SD 3; Year 5 in-home assessment)",
      y5 = "CH4LR_ADJSCOR"
    ),
    walk_line_baseline = bp31_entry(
      "Walk-a-Line motor-control task, baseline trial at normal speed, seconds (Year 3 in-home study, 18 cities only)",
      y3 = "CH3WALKC1B"
    ),
    walk_line_slow1 = bp31_entry(
      "Walk-a-Line motor-control task, first trial walking the line very slowly, seconds (Year 3 in-home study, 18 cities only)",
      y3 = "CH3WALKC2ST"
    ),
    walk_line_slow2 = bp31_entry(
      "Walk-a-Line motor-control task, second trial walking the line very slowly, seconds (Year 3 in-home study, 18 cities only)",
      y3 = "CH3WALKC3ST"
    ),
    # --- family: siblings ----------------------------------------------------
    mother_hh_children = bp31_entry(
      "Number of children under 18 in the mother's household, the focal child included where the child lives with her (Years 3-9)",
      y3 = "CM3KIDS",
      y5 = "CM4KIDS",
      y9 = "CM5KIDS"
    ),
    youth_full_siblings = bp31_entry(
      "Youth self-report: number of full siblings living with youth (Year 15 only)",
      y15 = "K6C42"
    ),
    youth_half_siblings = bp31_entry(
      "Youth self-report: number of half siblings living with youth (Year 15 only)",
      y15 = "K6C43"
    ),
    youth_step_siblings = bp31_entry(
      "Youth self-report: number of step siblings living with youth (Year 15 only)",
      y15 = "K6C44"
    ),
    # --- family: co-viewing, reading and books ------------------------------
    mother_coview_tv_days = bp31_entry(
      "Mother's own report: days per week you watch TV or a video together with the child (0-7; Year 5 only)",
      y5 = "M4B4A8"
    ),
    pcg_coview_tv_freq = bp31_entry(
      "PCG report: how often you watched TV or videos with the child in the past month (1 not once - 5 every day; Year 9 only)",
      y9 = "P5I1C"
    ),
    pcg_coplay_games_freq = bp31_entry(
      "PCG report: how often you played video or computer games with the child in the past month (1 not once - 5 every day; Year 9 only)",
      y9 = "P5I1D"
    ),
    mother_read_days = bp31_entry(
      "Mother's own report: days per week you read stories to the child (0-7; Years 3 and 5)",
      y3 = "M3B4F",
      y5 = "M4B4A2"
    ),
    mother_father_read_days = bp31_entry(
      "Mother's report: days per week the child's father reads stories to the child (0-7; Years 3 and 5)",
      y3 = "M3C3F",
      y5 = "M4C3B"
    ),
    mother_partner_read_days = bp31_entry(
      "Mother's report: days per week her current partner reads stories to the child (0-7; Years 3 and 5)",
      y3 = "M3E18F",
      y5 = "M4E18B"
    ),
    pcg_read_freq = bp31_entry(
      "PCG report: how often you read books or talked about books with the child in the past month (1 not once - 5 every day; Year 9 only)",
      y9 = "P5I1E"
    ),
    pcg_father_read_freq = bp31_entry(
      "PCG report: how often the child's father read books or talked about books with the child in the past month (1 not once - 5 every day; Year 9 only)",
      y9 = "P5I31E"
    ),
    pcg_partner_read_freq = bp31_entry(
      "PCG report: how often the PCG's partner read books or talked about books with the child in the past month (1 not once - 5 every day; Year 9 only)",
      y9 = "P5I40E"
    ),
    child_books_y3 = bp31_entry(
      "PCG report: how many books the child has (1 none, 2 1-2, 3 3-4, 4 5 or more; Year 3 only)",
      y3 = "P3C1F"
    ),
    child_own_books = bp31_entry(
      "PCG report: how many books are the child's own (Year 5, asked where the child has any; Year 9) (1 none, 2 1-10, 3 11-20, 4 more than 20)",
      y5 = "P4C12",
      y9 = "P5I20B"
    ),
    child_books_for_age_any = bp31_entry(
      "PCG report: are there any books for the child's age (0 no, 1 yes; gates child_has_own_books; Year 5 only)",
      y5 = "P4C9"
    ),
    child_has_own_books = bp31_entry(
      "PCG report: are any of the books for the child's age the child's own (0 no, 1 yes; gates child_own_books; Year 5 only)",
      y5 = "P4C11"
    ),
    # --- family: parenting --------------------------------------------------
    mother_hug_days = bp31_entry(
      "Mother's own report: days per week you hug or show physical affection to the child (0-7; Year 3 only)",
      y3 = "M3B4B"
    ),
    mother_say_love_days = bp31_entry(
      "Mother's own report: days per week you tell the child that you love him/her (0-7; Year 3 only)",
      y3 = "M3B4C"
    ),
    mother_appreciate_days = bp31_entry(
      "Mother's own report: days per week you tell the child you appreciate something he/she did (0-7; Years 3 and 5)",
      y3 = "M3B4I",
      y5 = "M4B4A5"
    ),
    cts_shouted = bp31_entry(
      "PCG's Conflict Tactics Scale (CTSPC) item: shouted, yelled or screamed at the child; past-year times band (0 not in the past year, 1 once, 2 twice, 3 3-5, 4 6-10, 5 11-20, 6 more than 20)",
      y3 = "BP31CTS_Y3_SHOUTED",
      y5 = "BP31CTS_Y5_SHOUTED",
      y9 = "BP31CTS_Y9_SHOUTED"
    ),
    cts_swore = bp31_entry(
      "PCG's CTSPC item: swore or cursed at the child; past-year times band (0 not in the past year - 6 more than 20)",
      y3 = "BP31CTS_Y3_SWORE",
      y5 = "BP31CTS_Y5_SWORE",
      y9 = "BP31CTS_Y9_SWORE"
    ),
    cts_send_away = bp31_entry(
      "PCG's CTSPC item: said would send the child away or kick the child out of the house; past-year times band (0 not in the past year - 6 more than 20)",
      y3 = "BP31CTS_Y3_SEND_AWAY",
      y5 = "BP31CTS_Y5_SEND_AWAY",
      y9 = "BP31CTS_Y9_SEND_AWAY"
    ),
    cts_threatened_spank = bp31_entry(
      "PCG's CTSPC item: threatened to spank or hit the child but did not; past-year times band (0 not in the past year - 6 more than 20)",
      y3 = "BP31CTS_Y3_THREATENED_SPANK",
      y5 = "BP31CTS_Y5_THREATENED_SPANK",
      y9 = "BP31CTS_Y9_THREATENED_SPANK"
    ),
    cts_called_names = bp31_entry(
      "PCG's CTSPC item: called the child dumb, lazy or a similar name; past-year times band (0 not in the past year - 6 more than 20)",
      y3 = "BP31CTS_Y3_CALLED_NAMES",
      y5 = "BP31CTS_Y5_CALLED_NAMES",
      y9 = "BP31CTS_Y9_CALLED_NAMES"
    ),
    cts_shook = bp31_entry(
      "PCG's CTSPC item: shook the child; past-year times band (0 not in the past year - 6 more than 20)",
      y3 = "BP31CTS_Y3_SHOOK",
      y5 = "BP31CTS_Y5_SHOOK",
      y9 = "BP31CTS_Y9_SHOOK"
    ),
    cts_hit_object = bp31_entry(
      "PCG's CTSPC item: hit the child on the bottom with a belt or other hard object; past-year times band (0 not in the past year - 6 more than 20)",
      y3 = "BP31CTS_Y3_HIT_OBJECT",
      y5 = "BP31CTS_Y5_HIT_OBJECT",
      y9 = "BP31CTS_Y9_HIT_OBJECT"
    ),
    cts_spanked = bp31_entry(
      "PCG's CTSPC item: spanked the child on the bottom with a bare hand; past-year times band (0 not in the past year - 6 more than 20)",
      y3 = "BP31CTS_Y3_SPANKED",
      y5 = "BP31CTS_Y5_SPANKED",
      y9 = "BP31CTS_Y9_SPANKED"
    ),
    cts_slapped = bp31_entry(
      "PCG's CTSPC item: slapped the child on the hand, arm or leg; past-year times band (0 not in the past year - 6 more than 20)",
      y3 = "BP31CTS_Y3_SLAPPED",
      y5 = "BP31CTS_Y5_SLAPPED",
      y9 = "BP31CTS_Y9_SLAPPED"
    ),
    cts_pinched = bp31_entry(
      "PCG's CTSPC item: pinched the child; past-year times band (0 not in the past year - 6 more than 20)",
      y3 = "BP31CTS_Y3_PINCHED",
      y5 = "BP31CTS_Y5_PINCHED",
      y9 = "BP31CTS_Y9_PINCHED"
    ),
    youth_pcg_hit_slapped = bp31_entry(
      "Youth self-report: PCG hit or slapped you (1 never, 2 sometimes, 3 often; Year 15 only)",
      y15 = "K6C9D"
    ),
    pcg_hit_slapped = bp31_entry(
      "PCG report: hit or slapped the youth in the past year (1 never, 2 sometimes, 3 often; Year 15 only)",
      y15 = "P6D29"
    ),
    # --- family: monitoring, relationships and media rules ------------------
    child_pcg_knows_free_time = bp31_entry(
      "Child self-report: how often the PCG knows what you do during your free time (0 never - 3 always; Year 9 only)",
      y9 = "K5A1A"
    ),
    child_pcg_knows_friends = bp31_entry(
      "Child self-report: how often the PCG knows which friends you hang out with during free time (0 never - 3 always; Year 9 only)",
      y9 = "K5A1B"
    ),
    child_pcg_asks = bp31_entry(
      "Child self-report: how often the PCG asks about things that happened when you are not with her/him (0 never - 3 always; Year 9 only)",
      y9 = "K5A1C"
    ),
    child_pcg_where_going = bp31_entry(
      "Child self-report: how often the PCG makes you tell where you are going and with whom (0 never - 3 always; Year 9 only)",
      y9 = "K5A1D"
    ),
    child_pcg_knows_spending = bp31_entry(
      "Child self-report: how often the PCG knows what you spend money on (0 never - 3 always; Year 9 only)",
      y9 = "K5A1E"
    ),
    youth_pcg_knows_free_time = bp31_entry(
      "Youth self-report: PCG knows what you do during your free time (1 never, 2 sometimes, 3 often; Year 15 only)",
      y15 = "K6C6A"
    ),
    youth_pcg_knows_spending = bp31_entry(
      "Youth self-report: PCG knows what you spend money on (1 never, 2 sometimes, 3 often; Year 15 only)",
      y15 = "K6C6B"
    ),
    pcg_knows_free_time = bp31_entry(
      "PCG report: how often knows what the youth does during free time (1 never, 2 sometimes, 3 often; Year 15 only)",
      y15 = "P6D24"
    ),
    pcg_knows_spending = bp31_entry(
      "PCG report: how often knows what the youth spends money on (1 never, 2 sometimes, 3 often; Year 15 only)",
      y15 = "P6D25"
    ),
    close_to_mother = bp31_entry(
      "Child/youth self-report: how close you feel to your (biological) mother (1 extremely close - 4 not very close)",
      y9 = "K5A2E",
      y15 = "K6C17"
    ),
    share_ideas_mother = bp31_entry(
      "Child/youth self-report: how well you and your mother share ideas or talk about things that matter (1 extremely well - 4 not very well)",
      y9 = "K5A2F",
      y15 = "K6C18"
    ),
    close_to_father = bp31_entry(
      "Child/youth self-report: how close you feel to your (biological) father (1 extremely close - 4 not very close)",
      y9 = "K5A3E",
      y15 = "K6C28"
    ),
    share_ideas_father = bp31_entry(
      "Child/youth self-report: how well you and your father share ideas or talk about things that matter (1 extremely well - 4 not very well)",
      y9 = "K5A3F",
      y15 = "K6C29"
    ),
    pcg_close_to_child = bp31_entry(
      "PCG report: how close you feel to the child (1 extremely close - 4 not very close; Years 9 and 15)",
      y9 = "P5K1F",
      y15 = "P6D37"
    ),
    pcg_share_ideas_child = bp31_entry(
      "PCG report: how well you and the child share ideas or talk about things that matter (1 extremely well - 4 not very well; Year 9 only)",
      y9 = "P5K1G"
    ),
    youth_tv_decides = bp31_entry(
      "Youth self-report: who decides what kinds of TV shows and movies you can watch (1 parents/guardians, 2 youth, 3 jointly; Year 15 only)",
      y15 = "K6C7B"
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
    ppvt_pct_missing = bp31_entry(
      "PPVT-III, child: percent of items for the total score missing (Years 3 and 5)",
      y3 = "CH3PVPERCOM",
      y5 = "CH4PVPERCOM"
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
    # --- body size, puberty and physical activity ---------------------------
    child_height_cm = bp31_entry(
      "Child's height in cm from the in-home measurements (Year 15: the in-home subsample only)",
      y3 = "CH3CHTCM",
      y5 = "CH4CHTCM",
      y9 = "CH5CHTCM",
      y15 = "CH6CHTCM"
    ),
    child_weight_kg = bp31_entry(
      "Child's weight in kg from the in-home measurements (Year 15: the in-home subsample only)",
      y3 = "CH3CWTKG",
      y5 = "CH4CWTKG",
      y9 = "CH5CWTKG",
      y15 = "CH6CWTKG"
    ),
    child_weight_source = bp31_entry(
      "In-home record of the child's weight: measured, or reported instead of measured (Year 9 only)",
      y9 = "H5W9"
    ),
    child_height_source = bp31_entry(
      "In-home record of the child's height: measured, or reported instead of measured (Year 9 only)",
      y9 = "H5W11"
    ),
    youth_height_ft = bp31_entry(
      "Youth self-report: height, feet (Year 15 only)",
      y15 = "K6D5"
    ),
    youth_height_in = bp31_entry(
      "Youth self-report: height, inches (Year 15 only)",
      y15 = "K6D6"
    ),
    youth_weight_lb = bp31_entry(
      "Youth self-report: weight, pounds (Year 15 only)",
      y15 = "K6D7"
    ),
    pds_growth_spurt = bp31_entry(
      "PCG report, Pubertal Development Scale: growth spurt (1 no - 4 development completed; Year 9 only)",
      y9 = "P5H17"
    ),
    pds_body_hair = bp31_entry(
      "PCG report, Pubertal Development Scale: underarm or pubic hair (1 no - 4 development completed; Year 9 only)",
      y9 = "P5H17A"
    ),
    pds_skin = bp31_entry(
      "PCG report, Pubertal Development Scale: skin changes, especially pimples (1 no - 4 development completed; Year 9 only)",
      y9 = "P5H17B"
    ),
    pds_voice = bp31_entry(
      "PCG report, Pubertal Development Scale, boys: deepening of the voice (1 no - 4 development completed; Year 9 only)",
      y9 = "P5H19"
    ),
    pds_facial_hair = bp31_entry(
      "PCG report, Pubertal Development Scale, boys: facial hair (1 no - 4 development completed; Year 9 only)",
      y9 = "P5H19A"
    ),
    pds_breasts = bp31_entry(
      "PCG report, Pubertal Development Scale, girls: breast growth (1 no - 4 development completed; Year 9 only)",
      y9 = "P5H21"
    ),
    pds_menarche = bp31_entry(
      "PCG report, Pubertal Development Scale, girls: first menstrual period (1 yes, 2 no; Year 9 only)",
      y9 = "P5H22"
    ),
    youth_menarche = bp31_entry(
      "Youth self-report, girls: begun to menstruate (1 yes, 2 no; Year 15 only)",
      y15 = "K6D9"
    ),
    youth_pa_days60 = bp31_entry(
      "Youth self-report: days physically active for 60+ minutes in the past week (0-7; Year 15 only)",
      y15 = "K6D37"
    ),
    # --- doctor-told conditions (Year 5 rebased by bp31_add_told_conditions)
    dx_asthma = bp31_entry(
      "PCG report: a doctor or health professional diagnosed the child with asthma",
      y5 = "BP31DX_Y5_P4A2_H",
      y9 = "P5H1B",
      y15 = "P6B2"
    ),
    dx_cerebral_palsy = bp31_entry(
      "PCG report: a doctor diagnosed the child with cerebral palsy (Years 5 and 9)",
      y5 = "BP31DX_Y5_P4A2_D",
      y9 = "P5H2D"
    ),
    dx_sickle_cell = bp31_entry(
      "PCG report: a doctor diagnosed the child with sickle cell anemia (Years 5 and 9)",
      y5 = "BP31DX_Y5_P4A2_E",
      y9 = "P5H2E"
    ),
    dx_heart = bp31_entry(
      "PCG report: a doctor diagnosed the child with congenital heart disease or a heart condition",
      y5 = "BP31DX_Y5_P4A2_G",
      y9 = "P5H2G",
      y15 = "P6B4"
    ),
    dx_limbs = bp31_entry(
      "PCG report: a doctor diagnosed the child with problems with limbs",
      y5 = "BP31DX_Y5_P4A2_N",
      y9 = "P5H2M",
      y15 = "P6B7"
    ),
    dx_anemia = bp31_entry(
      "PCG report: a doctor diagnosed the youth with anemia (Year 15 only)",
      y15 = "P6B3"
    ),
    dx_diabetes = bp31_entry(
      "PCG report: a doctor diagnosed the youth with diabetes (Year 15 only)",
      y15 = "P6B6"
    ),
    dx_seizures = bp31_entry(
      "PCG report: a doctor diagnosed the youth with seizures (Year 15 only)",
      y15 = "P6B8"
    ),
    dx_other = bp31_entry(
      "PCG report: a doctor diagnosed the youth with another condition (Year 15 only)",
      y15 = "P6B9"
    ),
    dx_other_headache = bp31_entry(
      "Other diagnosed condition specified: headache or migraines (Year 15 only)",
      y15 = "P6B9_101"
    ),
    dx_other_overweight = bp31_entry(
      "Other diagnosed condition specified: overweight or obese (Year 15 only)",
      y15 = "P6B9_102"
    ),
    dx_other_allergies = bp31_entry(
      "Other diagnosed condition specified: allergies (Year 15 only)",
      y15 = "P6B9_103"
    ),
    dx_other_eczema = bp31_entry(
      "Other diagnosed condition specified: eczema (Year 15 only)",
      y15 = "P6B9_104"
    ),
    dx_other_blood_pressure = bp31_entry(
      "Other diagnosed condition specified: blood pressure or cholesterol (Year 15 only)",
      y15 = "P6B9_105"
    ),
    dx_other_scoliosis = bp31_entry(
      "Other diagnosed condition specified: scoliosis (Year 15 only)",
      y15 = "P6B9_106"
    ),
    dx_other_unspecified = bp31_entry(
      "Other diagnosed condition specified: unspecified (Year 15 only)",
      y15 = "P6B9_107"
    ),
    # --- disability and activity limitation ---------------------------------
    physical_disability = bp31_entry(
      "PCG report: child has any physical disabilities (Year 3 only; disab_* follow up a yes)",
      y3 = "P3A2"
    ),
    disab_cerebral_palsy = bp31_entry(
      "PCG report: child has cerebral palsy (Year 3 only)",
      y3 = "P3A3_1"
    ),
    disab_blind_total = bp31_entry(
      "PCG report: child has total blindness (Year 3 only)",
      y3 = "P3A3_2"
    ),
    disab_blind_partial = bp31_entry(
      "PCG report: child has partial blindness (Year 3 only)",
      y3 = "P3A3_3"
    ),
    disab_deaf_total = bp31_entry(
      "PCG report: child has total deafness (Year 3 only)",
      y3 = "P3A3_4"
    ),
    disab_deaf_partial = bp31_entry(
      "PCG report: child has partial deafness (Year 3 only)",
      y3 = "P3A3_5"
    ),
    disab_downs = bp31_entry(
      "PCG report: child has Down's syndrome (Year 3 only)",
      y3 = "P3A3_6"
    ),
    disab_limbs = bp31_entry(
      "PCG report: child has a problem with limbs (Year 3 only)",
      y3 = "P3A3_7"
    ),
    disab_other = bp31_entry(
      "PCG report: child has another physical disability (Year 3 only)",
      y3 = "P3A3_0"
    ),
    disab_other_breathing = bp31_entry(
      "Other physical disability specified: asthma or other breathing problem (Year 3 only)",
      y3 = "P3A3_101"
    ),
    disab_other_speech = bp31_entry(
      "Other physical disability specified: speech problem (Year 3 only)",
      y3 = "P3A3_102"
    ),
    disab_other_problem = bp31_entry(
      "Other physical disability specified: other problem (Year 3 only)",
      y3 = "P3A3_103"
    ),
    health_limits_activities = bp31_entry(
      "PCG report: health problems limit the youth's usual activities (1 yes, 2 no; Year 15 only)",
      y15 = "P6B20"
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

#' Mother's age and paid work at Year 15
#'
#' Year 15 asks the PCG about herself, which describes the mother only where
#' the PCG is the biological mother; otherwise her age is projected from Year 9.
bp31_fill_mother_y15 <- function(tidied) {
  waves <- bp31_waves()
  not_mother <- tidied$wave == waves[["y15"]] &
    !tidied$pcg_relationship %in% 1
  tidied$mother_worked_last_week[not_mother] <- NA_real_

  child_age <- dplyr::coalesce(tidied$age_months, tidied$age_months_alt) / 12
  at_y9 <- which(tidied$wave == waves[["y9"]])
  y9 <- at_y9[match(tidied$participant_id, tidied$participant_id[at_y9])]
  projected <- tidied$mother_age[y9] + child_age - child_age[y9]
  tidied$mother_age[not_mother] <- projected[not_mother]

  tidied
}

#' Year 5 doctor-told items recoded from 0 no, 1 yes to the 1 yes, 2 no of
#' Years 9 and 15
bp31_add_told_conditions <- function(raw) {
  items <- c(
    "P4A2_A",
    "P4A2_D",
    "P4A2_E",
    "P4A2_F",
    "P4A2_G",
    "P4A2_H",
    "P4A2_M",
    "P4A2_N"
  )
  for (item in items) {
    values <- bp31_decode(raw[[item]])
    raw[[paste0("BP31DX_Y5_", item)]] <- dplyr::if_else(values == 0, 2, values)
  }
  raw
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
    father_born_us = "Father born in the US, own report at baseline (missing where the father was not interviewed)",
    city_sample = "In the 20-city core sample (0 = one of the 109 baseline cases not randomly selected for the core sample, some for the TLC3 study)",
    prenatal_cigarettes = "Mother's own report at baseline: cigarettes smoked a day during the pregnancy",
    breastfed_mother = "Mother's report at Year 1: child was ever breastfed",
    breastfed_father = "Father's report at Year 1: child was ever breastfed"
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
#' 5 Table 27 (CBCL/4-18, 1991), Year 15 Table 18 (CBCL/6-18). Year 9 asked
#' 111 of the CBCL/6-18's 120 items, every syndrome item included, but its
#' user guide prints only example items, so its lists are ASEBA's CBCL/6-18
#' sample profile (aseba.org/wp-content/uploads/cbclprofile.pdf), matched to
#' the P5Q3 items by wording.
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
      ),
      # 4 of the 9 CBCL/1.5-5 Emotionally Reactive items were asked (P3M7A
      # and P3M40A in 18 cities only); scored only for its own row
      emotreact = c("P3M7A", "P3M40", "P3M40A", "P3M49")
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
      ),
      # CBCL/4-18 (1991) Social Problems, 8 of 8 items (Year 5 Table 27)
      socprob = c(
        "M4B4B19",
        "M4B4B3",
        "M4B4B6",
        "P4L22",
        "P4L28",
        "P4L31",
        "P4L35",
        "P4L37"
      )
    ),
    # Somatic complaints are scored only here, for the CBCL/6-18 internalising
    # grouping; item 105 (P5Q3CX) also asks about alcohol, unlike the form.
    y9 = list(
      anxdep = c(
        "P5Q3M",
        "P5Q3AB",
        "P5Q3AC",
        "P5Q3AD",
        "P5Q3AE",
        "P5Q3AF",
        "P5Q3AH",
        "P5Q3AQ",
        "P5Q3AV",
        "P5Q3AX",
        "P5Q3BQ",
        "P5Q3CK",
        "P5Q3DB"
      ),
      withdrawn = c(
        "P5Q3E",
        "P5Q3AO",
        "P5Q3BK",
        "P5Q3BO",
        "P5Q3BU",
        "P5Q3CU",
        "P5Q3CV",
        "P5Q3DA"
      ),
      somatic = c(
        "P5Q3AS",
        "P5Q3AU",
        "P5Q3AW",
        "P5Q3AZ",
        "P5Q3BB1",
        "P5Q3BB2",
        "P5Q3BB3",
        "P5Q3BB4",
        "P5Q3BB5",
        "P5Q3BB6",
        "P5Q3BB7"
      ),
      attention = c(
        "P5Q3A",
        "P5Q3D",
        "P5Q3G",
        "P5Q3I",
        "P5Q3L",
        "P5Q3P",
        "P5Q3AN",
        "P5Q3BG",
        "P5Q3BW",
        "P5Q3BY"
      ),
      aggressive = c(
        "P5Q3C",
        "P5Q3O",
        "P5Q3R",
        "P5Q3S",
        "P5Q3T",
        "P5Q3U",
        "P5Q3V",
        "P5Q3AJ",
        "P5Q3BC",
        "P5Q3BN",
        "P5Q3CF",
        "P5Q3CG",
        "P5Q3CH",
        "P5Q3CI",
        "P5Q3CN",
        "P5Q3CO",
        "P5Q3CQ",
        "P5Q3CW"
      ),
      rulebreak = c(
        "P5Q3B",
        "P5Q3X",
        "P5Q3AA",
        "P5Q3AL",
        "P5Q3AP",
        "P5Q3BI",
        "P5Q3BM",
        "P5Q3BR",
        "P5Q3BS",
        "P5Q3BZ",
        "P5Q3CA",
        "P5Q3CJ",
        "P5Q3CP",
        "P5Q3CR",
        "P5Q3CT",
        "P5Q3CX",
        "P5Q3CY"
      ),
      # CBCL/6-18 Social Problems, 11 of 11 items, matched to the sample
      # profile by wording as the other Year 9 lists
      socprob = c(
        "P5Q3J",
        "P5Q3K",
        "P5Q3W",
        "P5Q3Y",
        "P5Q3AG",
        "P5Q3AI",
        "P5Q3AK",
        "P5Q3AT",
        "P5Q3BH",
        "P5Q3BJ",
        "P5Q3BX"
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

#' Year 9 CBCL Total Problems items: all 111 CBCL/6-18 problem items asked
#'
#' The user guide's Total CBCL (Table 32, note 3) leaves out only the 8 items
#' Year 9 did not ask; its Table 32 statistics also count the 13 ASBI items
#' (P5Q3DC-P5Q3DO), which are not CBCL items and are left out here.
bp31_cbcl_total_items_y9 <- function() {
  c(
    unlist(bp31_cbcl_items()$y9, use.names = FALSE),
    # Problem items on no syndrome scale scored above, Thought Problems included
    "P5Q3F",
    "P5Q3H",
    "P5Q3N",
    "P5Q3Q",
    "P5Q3AM",
    "P5Q3AR",
    "P5Q3AY",
    "P5Q3BA",
    "P5Q3BB8",
    "P5Q3BD",
    "P5Q3BE",
    "P5Q3BF",
    "P5Q3BL",
    "P5Q3BP",
    "P5Q3BT",
    "P5Q3BV",
    "P5Q3CB",
    "P5Q3CD",
    "P5Q3CE",
    "P5Q3CL",
    "P5Q3CM",
    "P5Q3CS",
    "P5Q3CZ"
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
#' rule-breaking scale. Internalising adds somatic complaints only where a
#' wave's lists score them (Year 9).
bp31_cbcl_composites <- function(subscales) {
  externalising <- if (is.null(subscales$rulebreak)) {
    c("aggressive", "attention")
  } else {
    c("aggressive", "rulebreak")
  }
  list(
    internalising = intersect(
      c("anxdep", "withdrawn", "somatic"),
      names(subscales)
    ),
    externalising = externalising
  )
}

#' Score the CBCL subscales and composites and append them to the raw table
bp31_add_cbcl_scores <- function(raw) {
  absent <- setdiff(
    unique(c(
      unlist(bp31_cbcl_items()),
      unlist(bp31_cbcl_single_items()),
      bp31_cbcl_total_items_y9()
    )),
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

  raw$BP31CBCL_Y9_TOTAL <- bp31_cbcl_mean(raw, bp31_cbcl_total_items_y9())

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

#' Year 9 teacher survey scales, scored by the user guide (sections 18.4 and
#' 18.5)
#'
#' The guide prints only example items, so the item lists follow the published
#' forms by wording and reproduce every N, mean and SD of the guide's Tables
#' 77, 78 and 83: the Social Skills Rating System elementary teacher form
#' (25 of its 30 social skills items, 12 problem behaviour items) and the
#' Conners' Teacher Rating Scale-Revised short form. Items are rescaled to
#' start at 0 and summed, complete cases only, as the guide scores them;
#' scored here because the item lists need this documentation.
bp31_teacher_scale_items <- function() {
  ssrs <- function(x) paste0("T5B1", x)
  problems <- function(x) paste0("T5B3", x)
  conners <- function(n) paste0("T5B4", c(LETTERS, "AA", "AB")[n])
  list(
    SSRS_SOCIAL = ssrs(c(
      # cooperation
      "F",
      "G",
      "L",
      "M",
      "P",
      "Q",
      "U",
      "V",
      "W",
      "X",
      # assertion
      "D",
      "E",
      "H",
      "O",
      "S",
      # self-control
      "A",
      "B",
      "C",
      "I",
      "J",
      "K",
      "N",
      "R",
      "T",
      "Y"
    )),
    SSRS_EXT = problems(c("A", "C", "G", "H", "I", "J")),
    SSRS_INT = problems(c("B", "D", "E", "F", "K", "L")),
    CONNERS_COGNITIVE = conners(c(4, 8, 13, 18, 22)),
    CONNERS_HYPERACTIVITY = conners(c(3, 7, 11, 17, 21, 24, 27)),
    CONNERS_ADHD = conners(c(1, 5, 9, 12, 14, 16, 19, 23, 25, 26, 27, 28))
  )
}

#' Score the Year 9 teacher scales and append them to the raw table
bp31_add_teacher_scales <- function(raw) {
  scales <- bp31_teacher_scale_items()
  for (scale in names(scales)) {
    items <- do.call(
      cbind,
      lapply(scales[[scale]], function(name) bp31_cbcl_rescale(raw[[name]]))
    )
    raw[[paste0("BP31TCH_Y9_", scale)]] <- rowSums(items)
  }
  raw
}

#' Caregiver-rated Adaptive Social Behavior Inventory Express items, mean
#' item score on 0-2, complete cases only
#'
#' The item set changes with the wave (user guides: Year 3 Table 30, 9 items;
#' Year 5 Table 29, 13 items; Year 9 Table 33, 3 ASBI items with 10 Social
#' Skills Rating System assertion items, coded 1-3), so it is scored here.
#' Year 5 'in social activities, tends to just watch others' (P4L32) is
#' worded against the scale and correlates negatively with every other item,
#' so it is reversed.
bp31_add_asbi_scores <- function(raw) {
  items <- list(
    Y3 = paste0("P3M", c(4, 8, 12, 15, 20, 24, 27, 34, 43)),
    Y5 = paste0("P4L", c(3, 11, 15, 30, 32, 41, 48, 51, 55, 58, 60, 63, 66)),
    Y9 = paste0("P5Q3D", LETTERS[3:15])
  )
  reversed <- "P4L32"
  for (wave in names(items)) {
    scored <- do.call(
      cbind,
      lapply(items[[wave]], function(name) {
        values <- bp31_cbcl_rescale(raw[[name]])
        if (name %in% reversed) 2 - values else values
      })
    )
    raw[[paste0("BP31ASBI_", wave)]] <- rowMeans(scored)
  }
  raw
}

#' Self-reported delinquency scores, complete cases only
#'
#' Year 9: count of the 17 'Things That You Have Done' acts answered yes
#' (K5F1A-K5F1Q, 1 yes, 2 no; user guide Table 42). Year 15: sum of the 13
#' Add Health acts in the past 12 months (K6D61A-K6D61M, 1 never - 4 five or
#' more times, scored 0-3; user guide section 2.4). The item sets differ by
#' wave, so they are scored here.
bp31_add_delinquency_scores <- function(raw) {
  y9 <- do.call(
    cbind,
    lapply(paste0("K5F1", LETTERS[1:17]), function(name) {
      as.numeric(bp31_decode(raw[[name]]) == 1)
    })
  )
  raw$BP31DELINQ_Y9 <- rowSums(y9)

  y15 <- do.call(
    cbind,
    lapply(paste0("K6D61", LETTERS[1:13]), function(name) {
      bp31_decode(raw[[name]]) - 1
    })
  )
  raw$BP31DELINQ_Y15 <- rowSums(y15)
  raw
}

#' PCG's Parent-Child Conflict Tactics Scales items on one past-year coding
#'
#' Years 3 and 5 code 0 never, 1-6 times bands and 7 'not in the past year';
#' Year 9 codes 1-6, 7 'not in the past year' and 8 never. Both become the
#' past-year times band 0-6, 0 = not in the past year.
bp31_add_cts_items <- function(raw) {
  items <- list(
    SHOUTED = c(Y3 = "P3J6", Y5 = "P4G6", Y9 = "P5Q1F"),
    SWORE = c(Y3 = "P3J8", Y5 = "P4G8", Y9 = "P5Q1H"),
    SEND_AWAY = c(Y3 = "P3J9", Y5 = "P4G9", Y9 = "P5Q1I"),
    THREATENED_SPANK = c(Y3 = "P3J10", Y5 = "P4G10", Y9 = "P5Q1J"),
    CALLED_NAMES = c(Y3 = "P3J14", Y5 = "P4G14", Y9 = "P5Q1N"),
    SHOOK = c(Y3 = "P3J3", Y5 = "P4G3", Y9 = "P5Q1C"),
    HIT_OBJECT = c(Y3 = "P3J4", Y5 = "P4G4", Y9 = "P5Q1D"),
    SPANKED = c(Y3 = "P3J7", Y5 = "P4G7", Y9 = "P5Q1G"),
    SLAPPED = c(Y3 = "P3J11", Y5 = "P4G11", Y9 = "P5Q1K"),
    PINCHED = c(Y3 = "P3J13", Y5 = "P4G13", Y9 = "P5Q1M")
  )
  for (item in names(items)) {
    for (wave in names(items[[item]])) {
      values <- bp31_decode(raw[[items[[item]][[wave]]]])
      values[values %in% c(7, 8)] <- 0
      raw[[paste("BP31CTS", wave, item, sep = "_")]] <- values
    }
  }
  raw
}

#' A grade repeated stays repeated: a yes is carried to every later wave the
#' child attended, over a later no too; a no is never carried
bp31_carry_grade_repeated <- function(tidied) {
  order_in_wave <- match(tidied$wave, bp31_waves())
  repeated <- tidied$grade_repeated %in% 1
  first_yes <- tapply(
    ifelse(repeated, order_in_wave, NA_real_),
    tidied$participant_id,
    function(x) suppressWarnings(min(x, na.rm = TRUE))
  )
  later <- order_in_wave > first_yes[tidied$participant_id]
  tidied$grade_repeated[!is.na(later) & later] <- 1
  tidied
}
