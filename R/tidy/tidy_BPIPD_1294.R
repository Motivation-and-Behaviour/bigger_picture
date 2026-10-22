#' Tidier for BPIPD-1294 (ELS:2002, Education Longitudinal Study)
#'
#' One wide student file holds every round; the two screen rounds are sliced out
#' through a column map, stacked, and labelled from the record layout. The
#' school file adds the 2004 school's control and locale.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per sample member per round attended (2002, 2004)
tidy_BPIPD_1294 <- function(raw_dataset, spec) {
  # Replicate-weight and institution files are not sample-member grain.
  raw <- raw_dataset$data$els_byf3pststu

  fixed <- tibble::as_tibble(stats::setNames(
    lapply(bp1294_fixed(), function(nm) raw[[nm]]),
    names(bp1294_fixed())
  ))

  tidied <- dplyr::bind_rows(lapply(
    names(bp1294_rounds()),
    function(round) bp1294_round_frame(raw, fixed, round)
  ))

  # Nothing is carried: parent education is complete on every kept row, and the
  # rounds are 18-31 months apart, beyond the 12-month SES and family window.
  if (anyDuplicated(tidied[c("STU_ID", "wave")]) > 0L) {
    stop(
      "BPIPD-1294: `STU_ID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  tidied <- bp1294_apply_labels(
    tidied,
    grep("BYF3PSTSTU", raw_dataset$docs, value = TRUE)
  )
  bp1294_add_f1_school(tidied, raw_dataset$data$els_byf1sch)
}

#' Fill the 2004 rows' school control and locale from the first follow-up school
bp1294_add_f1_school <- function(tidied, school) {
  # The student file classifies only the 2002 school; the school file holds the
  # same frame codes for every school `F1SCH_ID` links to (-8 = no such school).
  f1 <- tidied$wave == bp1294_rounds()$f1$label
  at <- match(haven::zap_labels(tidied$school_id_f1[f1]), school$SCH_ID)
  columns <- c(school_control = "bysctrl", school_urbanicity = "byurban")
  for (stem in names(columns)) {
    tidied[[stem]][f1] <- school[[columns[[stem]]]][at]
    attr(tidied[[stem]], "label") <- paste(
      attr(tidied[[stem]], "label"),
      "(2004 rows: first follow-up school, from the school file)"
    )
  }
  tidied
}

#' Rounds kept, each with its questionnaire-status (participation) column
bp1294_rounds <- function() {
  # 2006 and 2012 ask no screen-use item, so they are not kept.
  list(
    by = list(label = "Base year 2002", status = "bysqstat"),
    f1 = list(label = "First follow-up 2004", status = "F1QSTAT")
  )
}

#' Columns the release records once per sample member rather than once per round
bp1294_fixed <- function() {
  c(
    STU_ID = "STU_ID",
    STRAT_ID = "STRAT_ID",
    PSU = "psu",
    G10COHRT = "G10COHRT",
    G12COHRT = "G12COHRT",
    by_questionnaire_status = "bysqstat",
    f1_questionnaire_status = "F1QSTAT"
  )
}

#' Source column for each tidied stem at each round
bp1294_columns <- function() {
  # A coding that changes between rounds (hours vs bands) gets one stem each.
  # Dropout-form screen items (F1D*) are copies of the F1S columns, so omitted.
  list(
    # provenance and participation
    questionnaire_date = c(by = "byqxdatp", f1 = "F1QXDATP"),
    questionnaire_mode = c(f1 = "F1QMODE"),
    grade_attended = c(f1 = "F1GRADE"),
    test_status_by = c(by = "bytxstat"),
    test_status_f1 = c(f1 = "F1TSTAT"),
    student_weight = c(by = "bystuwt", f1 = "F1QWT"),
    panel_weight = c(f1 = "F1PNLWT"),
    school_id_f1 = c(f1 = "F1SCH_ID"),
    # demographics
    sex = c(by = "bysex", f1 = "F1SEX"),
    race = c(by = "byrace", f1 = "F1RACE"),
    birth_year_month = c(by = "BYDOB_P", f1 = "F1DOB_P"),
    english_native_language = c(by = "bystlang", f1 = "F1STLANG"),
    home_language = c(by = "byhomlng", f1 = "F1HOMLNG"),
    generational_status = c(by = "bygnstat"),
    family_composition = c(by = "byfcomp", f1 = "F1FCOMP"),
    siblings_in_home = c(by = "bysibhom"),
    birthplace_parent_report = c(by = "BYP23"),
    mother_birthplace = c(by = "BYP17"),
    father_birthplace = c(by = "BYP20"),
    sibling_structure = c(by = "bysibstr"),
    # socio-economic position
    ses_composite_v1 = c(by = "BYSES1", f1 = "F1SES1"),
    ses_quartile_v1 = c(by = "BYSES1QU", f1 = "F1SES1QU"),
    ses_composite_v2 = c(by = "BYSES2", f1 = "F1SES2"),
    ses_quartile_v2 = c(by = "BYSES2QU", f1 = "F1SES2QU"),
    family_income = c(by = "byincome"),
    parental_education = c(by = "bypared", f1 = "F1PARED"),
    mother_education = c(by = "bymothed", f1 = "F1MOTHED"),
    father_education = c(by = "byfathed", f1 = "F1FATHED"),
    mother_occupation = c(by = "byoccum", f1 = "F1OCCUM"),
    father_occupation = c(by = "byoccuf", f1 = "F1OCCUF"),
    grandparent_education = c(by = "bygpared"),
    home_literacy_resources = c(by = "byhomlit"),
    # base-year school context; bp1294_add_f1_school() fills 2004 control, locale
    school_urbanicity = c(by = "byurban"),
    school_region = c(by = "byregion"),
    school_control = c(by = "bysctrl"),
    school_pct_free_lunch = c(by = "BY10FLP"),
    school_enrolment_band = c(by = "byscenp"),
    school_academic_climate = c(by = "byacclim"),
    school_safety_principal = c(by = "BYSCSAF1"),
    school_safety_student = c(by = "BYSCSAF2"),
    # screen use reported in whole hours (base year)
    tv_wd_hours = c(by = "BYS48A"),
    tv_we_hours = c(by = "BYS48B"),
    game_wd_hours = c(by = "BYS49A"),
    game_we_hours = c(by = "BYS49B"),
    computer_schoolwork_hours = c(by = "BYS46A"),
    computer_other_hours = c(by = "BYS46B"),
    # screen use reported in hour bands (first follow-up)
    tv_wd_band = c(f1 = "F1S34A"),
    tv_we_band = c(f1 = "F1S34B"),
    game_wd_band = c(f1 = "F1S35A"),
    game_we_band = c(f1 = "F1S35B"),
    computer_schoolwork_band = c(f1 = "F1S36A"),
    computer_other_band = c(f1 = "F1S36B"),
    # NCES composites, both already expressed in hours per day
    tv_game_wd_hours_combined = c(by = "bytvvigm", f1 = "F1TVVIGM"),
    computer_total_hours = c(f1 = "F1COMPHR"),
    # screen use reported as a frequency
    computer_freq_fun_5pt = c(by = "BYS45A"),
    computer_freq_schoolwork_5pt = c(by = "BYS45B"),
    computer_freq_learn_5pt = c(by = "BYS45C"),
    computer_freq_fun_4pt = c(f1 = "F1S38A"),
    computer_freq_schoolwork_4pt = c(f1 = "F1S38B"),
    computer_freq_learn_4pt = c(f1 = "F1S38C"),
    computer_freq_at_home = c(by = "BYS47A", f1 = "F1S37A"),
    computer_freq_at_school = c(by = "BYS47B"),
    computer_freq_school_library = c(f1 = "F1S37B"),
    computer_freq_school_elsewhere = c(f1 = "F1S37C"),
    computer_freq_public_library = c(by = "BYS47C", f1 = "F1S37D"),
    computer_freq_at_friends = c(by = "BYS47D", f1 = "F1S37E"),
    computer_freq_elsewhere = c(by = "BYS47E", f1 = "F1S37F"),
    phone_freq_with_friends = c(by = "BYS44E", f1 = "F1S39E"),
    internet_freq_with_others = c(f1 = "F1S39I"),
    library_internet_school = c(by = "BYS51I", f1 = "F1S29I"),
    library_internet_public = c(f1 = "F1S30I"),
    # parental screen rules (base year, student and parent report)
    parents_limit_tv_games = c(by = "BYS85F"),
    family_rule_tv_parent = c(by = "BYP69D"),
    # achievement and school engagement
    test_math_standardised = c(by = "bytxmstd", f1 = "F1TXMSTD"),
    test_math_quartile = c(by = "bytxmqu", f1 = "F1TXMQU"),
    # IRT number-right scores use a 72-item pool at BY and 85 items at F1.
    test_math_irt_72item = c(by = "bytxmirr"),
    test_math_irt_85item = c(f1 = "F1TXM1IR"),
    test_reading_standardised = c(by = "bytxrstd"),
    test_reading_quartile = c(by = "bytxrqu"),
    test_reading_irt = c(by = "bytxrirr"),
    test_composite_standardised = c(by = "bytxcstd"),
    test_composite_quartile = c(by = "bytxcqu"),
    gpa_9_12_categorical = c(f1 = "F1RGPP2"),
    transcript_coverage_flag = c(f1 = "F1RTRFLG"),
    homework_hours_week = c(by = "byhmwrk"),
    homework_hours_week_band = c(f1 = "F1S31"),
    reading_hours_week_band = c(f1 = "F1S33"),
    education_expectation_by = c(by = "bystexp"),
    education_expectation_f1 = c(f1 = "F1STEXP"),
    extracurricular_count_by = c(by = "byxtracu"),
    extracurricular_count_f1 = c(f1 = "F1XTRACU"),
    work_hours_week = c(by = "bywrkhrs", f1 = "F1WRKHRS"),
    academic_risk_factors = c(by = "byriskfc"),
    grades_repeated = c(by = "bygrdrpt"),
    # motivation and academic self-beliefs
    math_self_efficacy = c(by = "bymathse", f1 = "F1MATHSE"),
    english_self_efficacy = c(by = "byenglse"),
    control_expectation = c(by = "byconexp"),
    instrumental_motivation = c(by = "byinstmo"),
    action_control_effort = c(by = "byactctl"),
    class_preparation = c(by = "bystprep"),
    teacher_student_relations = c(by = "bytstrel"),
    teacher_quality_perception = c(by = "byteaqua"),
    # school climate, victimisation and conduct
    climate_put_down_teachers = c(by = "BYS20H"),
    climate_put_down_students = c(by = "BYS20I"),
    climate_not_safe = c(by = "BYS20J"),
    climate_disruptions = c(by = "BYS20K"),
    victim_theft = c(by = "BYS22A", f1 = "F1S25A"),
    victim_offered_drugs = c(by = "BYS22B", f1 = "F1S25B"),
    victim_threatened = c(by = "BYS22C", f1 = "F1S25C"),
    victim_hit = c(by = "BYS22E", f1 = "F1S25D"),
    victim_coerced = c(by = "BYS22F"),
    victim_property_damaged = c(by = "BYS22G"),
    victim_bullied = c(by = "BYS22H"),
    conduct_physical_fight = c(by = "BYS22D"),
    conduct_late_for_school = c(by = "BYS24A"),
    conduct_cut_class = c(by = "BYS24B"),
    conduct_absent = c(by = "BYS24C"),
    conduct_in_trouble = c(by = "BYS24D"),
    conduct_in_school_suspension = c(by = "BYS24E"),
    conduct_suspended = c(by = "BYS24F"),
    engage_classes_interesting = c(by = "BYS27A"),
    engage_satisfied_in_class = c(by = "BYS27B"),
    engage_no_pencil_paper = c(by = "BYS38A"),
    engage_no_books = c(by = "BYS38B"),
    engage_no_homework_done = c(by = "BYS38C"),
    likes_school = c(by = "BYS28"),
    # maths teacher's ratings of the student (base year)
    teacher_math_homework = c(by = "BYTM13"),
    teacher_math_attentive = c(by = "BYTM16"),
    teacher_math_disruptive = c(by = "BYTM17")
  )
}

#' Build one round's slice of the tidied table
bp1294_round_frame <- function(raw, fixed, round) {
  sources <- vapply(
    bp1294_columns(),
    function(entry) unname(entry[round]),
    character(1)
  )
  sources <- sources[!is.na(sources)]

  slice <- tibble::as_tibble(stats::setNames(
    lapply(sources, function(nm) raw[[nm]]),
    names(sources)
  ))
  out <- dplyr::bind_cols(
    fixed,
    tibble::tibble(wave = bp1294_rounds()[[round]]$label),
    slice
  )

  # Status 0 means no questionnaire that round (codebook `BYSQSTAT`, `F1QSTAT`).
  out[raw[[bp1294_rounds()[[round]]$status]] > 0L, , drop = FALSE]
}

#' Re-attach the labels the .rdata files do not carry
bp1294_apply_labels <- function(tidied, layout_file) {
  # Each stem takes its earliest round's labels, with the round prefix trimmed.
  layout <- bp1294_layout(layout_file)
  sources <- c(
    bp1294_fixed(),
    vapply(bp1294_columns(), function(entry) entry[[1]], character(1))
  )

  attr(tidied$wave, "label") <- "ELS:2002 round the row was measured in"
  for (stem in names(sources)) {
    name <- toupper(sources[[stem]])
    tidied[[stem]] <- bp1294_label_column(
      tidied[[stem]],
      sub("^(BY|F1) ", "", layout$labels[[name]]),
      layout$values[[name]]
    )
  }
  tidied
}

#' Variable and value labels for the student file, read from its record layout
bp1294_layout <- function(path) {
  lines <- sub("\r$", "", readLines(path, warn = FALSE))
  break_at <- grep("^/\\* Variable Value Labels \\*/$", lines)
  described <- lines[seq_len(break_at - 1L)]
  coded <- lines[seq(break_at + 1L, length(lines))]

  parts <- regmatches(
    described,
    regexec("^([A-Za-z][A-Za-z0-9_]*) +[0-9]+-[0-9]+ +(.*[^ ]) *$", described)
  )
  parts <- Filter(function(part) length(part) == 3L, parts)
  labels <- stats::setNames(
    vapply(parts, `[`, character(1), 3L),
    toupper(vapply(parts, `[`, character(1), 2L))
  )

  is_name <- grepl("^[A-Za-z][A-Za-z0-9_]*$", coded)
  owner <- toupper(coded[is_name][cumsum(is_name)])
  pairs <- regmatches(coded, regexec("^ +(-?[0-9]+) = \"(.*)\"$", coded))
  keep <- vapply(pairs, length, integer(1)) == 3L
  codes <- stats::setNames(
    as.numeric(vapply(pairs[keep], `[`, character(1), 2L)),
    vapply(pairs[keep], `[`, character(1), 3L)
  )

  list(labels = labels, values = split(codes, owner[keep]))
}

#' Label one column, attaching value labels only where the layout records them
bp1294_label_column <- function(x, label, value_labels) {
  if (is.null(value_labels)) {
    attr(x, "label") <- label
    return(x)
  }
  codes <- stats::setNames(
    as.vector(value_labels, mode = typeof(x)),
    names(value_labels)
  )
  haven::labelled(x, labels = codes, label = label)
}
