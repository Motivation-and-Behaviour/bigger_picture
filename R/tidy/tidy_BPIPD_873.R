#' Tidier for BPIPD-873 (China Family Panel Studies)
#'
#' Each wave joins the child questionnaire to the adult (2010-2016) or person
#' (2018-2022) questionnaire on `pid`, then joins the family roster and the
#' household economy file onto the result.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per person per wave (`pid` x `wave`), aged 19 or under
tidy_BPIPD_873 <- function(raw_dataset, spec) {
  # 2012 fielded no screen-use duration item, so it's absent from
  # `bp873_waves()`; this guards any wave map that likewise contributes none.
  asked <- Filter(
    function(wave) {
      canonical <- c(names(wave$child_vars), names(wave$individual_vars))
      any(grepl("_(hours|mins)_(self|parent)$", canonical))
    },
    bp873_waves()
  )

  waves <- lapply(asked, function(wave) {
    child <- bp873_take(
      raw_dataset,
      wave$child,
      c(wave$child_vars, wave = ".wave"),
      "pid"
    )
    individual <- bp873_take(
      raw_dataset,
      wave$individual,
      c(wave$individual_vars, wave = ".wave"),
      "pid"
    )
    roster <- bp873_take(raw_dataset, wave$famconf, wave$famconf_vars, "pid")
    economy <- bp873_take(raw_dataset, wave$famecon, wave$famecon_vars, "fid")

    # The child and self-report files overlap only from 2018
    bp873_merge(child, individual, "pid") |>
      bp873_add_roster(roster) |>
      dplyr::left_join(economy, by = "fid", relationship = "many-to-one")
  })

  df <- bp873_relabel(bp873_bind_waves(waves), raw_dataset)

  if (anyDuplicated(df[c("pid", "wave")]) > 0) {
    stop(
      "BPIPD-873: `pid` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  # Keep children/adolescents only: `age_years` caps at 19, and CFPS routes
  # 16-19s to the adult/person questionnaire, not the child one. Rows with
  # neither a reported age nor a birth year go too.
  age <- dplyr::coalesce(df$age_reported, as.numeric(df$wave) - df$birth_year)
  df[!is.na(age) & age <= 19, ]
}

#' Per-wave source files and their column maps
#'
#' Screen items and their filter (gate) questions carry the responder in their
#' name (`_self` for the child's own report, `_parent` for the proxy module),
#' so the child/self-report merge never coalesces one responder over the
#' other. Interview dates are kept per responder for the same reason.
#'
#' Rosenberg self-esteem items keep the CFPS item order (`rse_1`-`rse_10`,
#' 1 = totally disagree ... 4 = totally agree); the 2014 files run the other
#' way (1 = totally agree), so they bind as `rse14_*`. K6-style distress items
#' (2010, 2014) share `k6_*` names across waves.
#'
#' Some items are carried only so that incompatible rows in `variables.csv`
#' can name them (frequency or yes/no screen items, agreement-rated behaviour
#' items, parent grade ratings and the like).
bp873_waves <- function() {
  list(
    list(
      child = "cfps2010child",
      child_vars = c(
        pid = "pid",
        fid = "fid",
        province = "provcd",
        county = "countyid",
        urban = "urban",
        weight_cross = "rswt_nat",
        psu = "psu",
        subpopulation = "subpopulation",
        sleep_wd_hours = "kt101_a_1",
        sleep_we_hours = "kt101_a_2",
        # 2010 anchors (Healthy...Very unhealthy) differ from the
        # Excellent-Poor wording used from 2014, so it keeps its own column.
        self_rated_health_2010 = "wl1",
        activity_1 = "wk8_s_1",
        activity_2 = "wk8_s_2",
        activity_3 = "wk8_s_3",
        activity_4 = "wk8_s_4",
        activity_5 = "wk8_s_5",
        activity_6 = "wk8_s_6",
        # 2010 frequency bands (6-7, 2-3, 1-2 times a week) differ from later waves
        tv_restrict_2010 = "wf605",
        academic_pressure = "ks502",
        age_reported = "wa1age",
        # One interview date covers the proxy and self modules in 2010
        interview_year_self = "cyear",
        interview_month_self = "cmonth",
        interview_year_parent = "cyear",
        interview_month_parent = "cmonth",
        school_grade = "wf302",
        # 2010 codes stage one step lower than later waves (1 = kindergarten
        # here, 1 = nursery from 2014), so it keeps its own column instead of
        # binding under mismatched labels.
        school_stage_2010 = "wf301",
        ethnicity_code = "wa6code",
        hukou = "wa4",
        birth_province = "wa107acode",
        internet_any_self = "ku2",
        media_wd_hours_self = "kt401_a_1",
        media_we_hours_self = "kt401_a_2",
        tv_wd_hours_self = "kt402_a_1",
        tv_we_hours_self = "kt402_a_2",
        internet_wd_hours_self = "kt403_a_1",
        internet_we_hours_self = "kt403_a_2",
        phone_messages_freq = "ku104_a_2",
        phone_games_freq = "ku104_a_3",
        email_days_week = "ku231",
        internet_place = "ku260",
        # 2010/2014 fold leave-of-absence and cutting class into one
        # four-option item; from 2016 `class_cut` is its own yes/no item, so
        # the two codings keep separate columns.
        class_absence = "kr430",
        school_satisfaction = "ks701",
        schoolwork_satisfaction = "ks501",
        study_hard = "ks601",
        grade_chinese_parent = "wf501",
        grade_maths_parent = "wf502",
        good_friends_n = "wk301",
        word_test = "wordtest",
        math_test = "mathtest",
        rse_1 = "wm101",
        rse_2 = "wm102",
        rse_3 = "wm103",
        rse_4 = "wm104",
        rse_5 = "wm105",
        rse_6 = "wm106",
        rse_7 = "wm107",
        rse_8 = "wm108",
        rse_9 = "wm109",
        rse_10 = "wm110",
        # The 2010 child form has its own frequency anchors (2-3 times a
        # week, 2-3 times a month, once a month), so it keeps its own columns.
        k6_child2010_depressed = "wn401",
        k6_child2010_nervous = "wn402",
        k6_child2010_restless = "wn403",
        k6_child2010_hopeless = "wn404",
        k6_child2010_effort = "wn405",
        k6_child2010_meaningless = "wn406"
      ),
      individual = "cfps2010adult",
      individual_vars = c(
        pid = "pid",
        fid = "fid",
        province = "provcd",
        county = "countyid",
        urban = "urban",
        weight_cross = "rswt_nat",
        psu = "psu",
        subpopulation = "subpopulation",
        sleep_wd_hours = "kt101_a_1",
        sleep_we_hours = "kt101_a_2",
        smoked_month = "qq2",
        academic_pressure = "ks502",
        age_reported = "qa1age",
        interview_year_self = "cyear",
        interview_month_self = "cmonth",
        ethnicity_code = "qa5code",
        hukou = "qa2",
        birth_province = "qa102acode",
        internet_any_self = "ku2",
        media_wd_hours_self = "kt401_a_1",
        media_we_hours_self = "kt401_a_2",
        tv_wd_hours_self = "kt402_a_1",
        tv_we_hours_self = "kt402_a_2",
        internet_wd_hours_self = "kt403_a_1",
        internet_we_hours_self = "kt403_a_2",
        internet_day_hours_self = "ku250",
        phone_messages_freq = "ku104_a_2",
        phone_games_freq = "ku104_a_3",
        email_days_week = "ku231",
        internet_place = "ku260",
        class_absence = "kr430",
        school_satisfaction = "ks701",
        schoolwork_satisfaction = "ks501",
        study_hard = "ks601",
        word_test = "wordtest",
        math_test = "mathtest",
        life_satisfaction = "qm403",
        k6_depressed = "qq601",
        k6_nervous = "qq602",
        k6_restless = "qq603",
        k6_hopeless = "qq604",
        k6_effort = "qq605",
        k6_meaningless = "qq606"
      ),
      famconf = "cfps2010famconf",
      famconf_vars = c(
        pid = "pid",
        fid = "fid",
        sex = "tb2_a_p",
        birth_year = "tb1y_a_p",
        birth_month = "tb1m_a_p",
        education_child = "tb4_a_p",
        education_father = "tb4_a_f",
        education_mother = "tb4_a_m",
        father_resident = "tb6_a_f",
        mother_resident = "tb6_a_m",
        father_alive = "alive_a_f",
        mother_alive = "alive_a_m"
      ),
      famecon = "cfps2010famecon",
      famecon_vars = c(
        fid = "fid",
        household_income = "faminc_net",
        household_income_pc = "indinc_net",
        household_size = "familysize",
        household_savings = "savings"
      )
    ),
    list(
      child = "cfps2014child",
      child_vars = c(
        pid = "pid",
        fid = "fid14",
        province = "provcd14",
        county = "countyid14",
        urban = "urban14",
        weight_cross = "rswt_natcs14",
        subpopulation = "subpopulation10",
        self_rated_health = "wl1",
        activity_1 = "wk8_s_1",
        activity_2 = "wk8_s_2",
        activity_3 = "wk8_s_3",
        activity_4 = "wk8_s_4",
        activity_5 = "wk8_s_5",
        tv_restrict = "wf605m",
        academic_pressure = "ks502",
        birth_address_2014 = "wa107m",
        age_reported = "cfps2014_age",
        interview_year_self = "cyear",
        interview_month_self = "cmonth",
        interview_year_parent = "cyear",
        interview_month_parent = "cmonth",
        school_grade = "wf302",
        school_stage = "wf301m",
        ethnicity_code = "wa6code",
        internet_any_self = "ku2",
        tv_week_hours_parent = "wb9",
        internet_week_hours_self = "ku250m",
        socialise_freq = "ku703",
        internet_work_freq = "ku702",
        email_days_week = "ku231m",
        internet_place = "ku260",
        class_absence = "kr430",
        class_rank = "kr425",
        school_satisfaction = "ks701",
        schoolwork_satisfaction = "ks501",
        grade_chinese_parent = "wf501",
        grade_maths_parent = "wf502",
        word_test = "wordtest14",
        math_test = "mathtest14",
        happiness = "wm302",
        rse14_1 = "wm101m",
        rse14_2 = "wm102m",
        rse14_3 = "wm103m",
        rse14_4 = "wm104m",
        rse14_5 = "wm105m",
        rse14_6 = "wm106m",
        rse14_7 = "wm107m",
        rse14_8 = "wm108m",
        rse14_9 = "wm109m",
        rse14_10 = "wm110m",
        k6_depressed = "wq601",
        k6_nervous = "wq602",
        k6_restless = "wq603",
        k6_hopeless = "wq604",
        k6_effort = "wq605",
        k6_meaningless = "wq606"
      ),
      individual = "cfps2014adult",
      individual_vars = c(
        pid = "pid",
        fid = "fid14",
        province = "provcd14",
        county = "countyid14",
        urban = "urban14",
        weight_cross = "rswt_natcs14",
        subpopulation = "subpopulation10",
        smoked_month = "qq201",
        academic_pressure = "ks502",
        sleep_day_hours = "qq4010",
        sleep_wd_hours = "qq4011",
        sleep_we_hours = "qq4012",
        birth_address_2014 = "qa401",
        age_reported = "cfps2014_age",
        interview_year_self = "cyear",
        interview_month_self = "cmonth",
        ethnicity_code = "qa701code",
        internet_any_self = "ku2",
        self_rated_health = "qp201",
        tv_week_hours_self = "qq1001",
        internet_week_hours_self = "ku250m",
        socialise_freq = "ku703",
        internet_work_freq = "ku702",
        email_days_week = "ku231m",
        internet_place = "ku260",
        class_absence = "kr430",
        class_rank = "kr425",
        school_satisfaction = "ks701",
        schoolwork_satisfaction = "ks501",
        word_test = "wordtest14",
        math_test = "mathtest14",
        happiness = "qm2012",
        life_satisfaction = "qn12012",
        family_satisfaction = "qn12013",
        # The adult form has nine items: no 'useless' (item 9)
        rse14_1 = "qm1011",
        rse14_2 = "qm1012",
        rse14_3 = "qm1013",
        rse14_4 = "qm1014",
        rse14_5 = "qm1015",
        rse14_6 = "qm1016",
        rse14_7 = "qm1017",
        rse14_8 = "qm1018",
        rse14_10 = "qm1019",
        k6_depressed = "qq601",
        k6_nervous = "qq602",
        k6_restless = "qq603",
        k6_hopeless = "qq604",
        k6_effort = "qq605",
        k6_meaningless = "qq606"
      ),
      famconf = "cfps2014famconf",
      famconf_vars = c(
        pid = "pid",
        fid = "fid14",
        sex = "tb2_a_p",
        birth_year = "tb1y_a_p",
        birth_month = "tb1m_a_p",
        education_child = "tb4_a14_p",
        education_father = "tb4_a14_f",
        education_mother = "tb4_a14_m",
        hukou = "qa301_a14_p",
        father_resident = "tb6_a14_f",
        mother_resident = "tb6_a14_m",
        father_alive = "alive_a14_f",
        mother_alive = "alive_a14_m"
      ),
      famecon = "cfps2014famecon",
      famecon_vars = c(
        fid = "fid14",
        household_income = "fincome1",
        household_income_pc = "fincome1_per",
        household_size = "familysize",
        household_savings = "savings"
      )
    ),
    list(
      child = "cfps2016child",
      child_vars = c(
        pid = "pid",
        fid = "fid16",
        province = "provcd16",
        county = "countyid16",
        urban = "urban16",
        weight_cross = "rswt_natcs16",
        subpopulation = "subpopulation",
        self_rated_health = "wl1",
        tv_restrict = "wf605m",
        academic_pressure = "ks502",
        drank_12m = "wk813",
        smoked_12m = "wk814",
        birth_address_2014 = "pa401",
        age_reported = "cfps_age",
        interview_year_self = "self_cyear",
        interview_month_self = "self_cmonth",
        interview_year_parent = "proxy_cyear",
        interview_month_parent = "proxy_cmonth",
        school_grade = "ppc5_b_1",
        school_stage = "pc3_b_1",
        ethnicity_code = "pa701code",
        internet_mobile_user = "ku201",
        internet_pc_user = "ku202",
        tv_week_hours_parent = "wb9",
        internet_week_hours_self = "ku250m",
        socialise_freq = "ku703",
        internet_work_freq = "ku702",
        email_days_week = "ku231m",
        class_cut = "kr4302",
        class_rank = "kr425",
        school_satisfaction = "ks701",
        schoolwork_satisfaction = "ks501",
        study_hard = "ks601n",
        grade_chinese_parent = "wf501",
        grade_maths_parent = "wf502",
        cesd20 = "cesd20sc",
        cesd_sleep = "pn411",
        cesd_happy = "pn412",
        lonely = "pn414",
        cesd_enjoyed = "pn416",
        sad = "pn418",
        cesd_notgoing = "pn420",
        happiness = "qm2014",
        number_series_w = "ns_w",
        rse_1 = "pm101m",
        rse_2 = "pm102m",
        rse_3 = "pm103m",
        rse_4 = "pm104m",
        rse_5 = "pm105m",
        rse_6 = "pm106m",
        rse_7 = "pm107m",
        rse_8 = "pm108m",
        rse_9 = "pm109m",
        rse_10 = "pm110m"
      ),
      individual = "cfps2016adult",
      individual_vars = c(
        pid = "pid",
        fid = "fid16",
        province = "provcd16",
        county = "countyid16",
        urban = "urban16",
        weight_cross = "rswt_natcs16",
        subpopulation = "subpopulation",
        smoked_month = "qq201",
        academic_pressure = "ks502",
        sleep_day_hours = "qq4010",
        sleep_wd_hours = "qq4011",
        sleep_we_hours = "qq4012",
        birth_address_2014 = "pa401",
        age_reported = "cfps_age",
        interview_year_self = "cyear",
        interview_month_self = "cmonth",
        ethnicity_code = "pa701code",
        internet_mobile_user = "ku201",
        internet_pc_user = "ku202",
        self_rated_health = "qp201",
        tv_week_hours_self = "qq1001",
        internet_week_hours_self = "ku250m",
        socialise_freq = "ku703",
        internet_work_freq = "ku702",
        email_days_week = "ku231m",
        class_cut = "kr4302",
        class_rank = "kr425",
        school_satisfaction = "ks701",
        schoolwork_satisfaction = "ks501",
        study_hard = "ks601n",
        cesd20 = "cesd20sc",
        cesd_sleep = "pn411",
        cesd_happy = "pn412",
        lonely = "pn414",
        cesd_enjoyed = "pn416",
        sad = "pn418",
        cesd_notgoing = "pn420",
        happiness = "qm2014",
        life_satisfaction = "qn12012",
        number_series_w = "ns_w",
        # The adult form has no 'useless' item (pm109m)
        rse_1 = "pm101m",
        rse_2 = "pm102m",
        rse_3 = "pm103m",
        rse_4 = "pm104m",
        rse_5 = "pm105m",
        rse_6 = "pm106m",
        rse_7 = "pm107m",
        rse_8 = "pm108m",
        rse_10 = "pm110m"
      ),
      famconf = "cfps2016famconf",
      famconf_vars = c(
        pid = "pid",
        fid = "fid16",
        sex = "tb2_a_p",
        birth_year = "tb1y_a_p",
        birth_month = "tb1m_a_p",
        education_child = "tb4_a16_p",
        education_father = "tb4_a16_f",
        education_mother = "tb4_a16_m",
        hukou = "hukou_a16_p",
        father_resident = "tb6_a16_f",
        mother_resident = "tb6_a16_m",
        father_alive = "alive_a16_f",
        mother_alive = "alive_a16_m"
      ),
      famecon = "cfps2016famecon",
      famecon_vars = c(
        fid = "fid16",
        household_income = "fincome1",
        household_income_pc = "fincome1_per",
        household_size = "familysize16",
        household_savings = "savings"
      )
    ),
    list(
      child = "cfps2018childproxy",
      child_vars = c(
        pid = "pid",
        fid = "fid18",
        province = "provcd18",
        county = "countyid18",
        urban = "urban18",
        weight_cross = "rswt_natcs18n",
        psu = "psu",
        subpopulation = "subpopulation",
        # 2018 and 2020 reverse the coding (1 = never ... 5 = very often)
        tv_restrict_2018 = "wf605m",
        birth_address = "wa401",
        age_reported = "age",
        interview_year_parent = "cyear",
        interview_month_parent = "cmonth",
        school_grade = "wc5_b_2",
        school_stage = "wc3_b_2",
        ethnicity_code = "wa701code",
        tv_week_hours_parent = "wb9",
        grade_chinese_parent = "wf501",
        grade_maths_parent = "wf502"
      ),
      individual = "cfps2018person",
      individual_vars = c(
        pid = "pid",
        fid = "fid18",
        province = "provcd18",
        county = "countyid18",
        urban = "urban18",
        weight_cross = "rswt_natcs18n",
        psu = "psu",
        subpopulation = "subpopulation",
        smoked_month = "qq201",
        academic_pressure = "qs502",
        sleep_day_hours = "qq4010",
        sleep_wd_hours = "qq4011",
        sleep_we_hours = "qq4012",
        birth_address = "qa401",
        age_reported = "age",
        interview_year_self = "cyear",
        interview_month_self = "cmonth",
        ethnicity_code = "qa701code",
        internet_mobile_user = "qu201",
        internet_pc_user = "qu202",
        self_rated_health = "qp201",
        tv_week_hours_self = "qq1001",
        internet_week_hours_self = "qu250m",
        socialise_freq = "qu703",
        internet_work_freq = "qu702",
        email_days_week = "qu231m",
        class_cut = "kr4302",
        class_rank = "kr425",
        school_satisfaction = "qs701_b_2",
        schoolwork_satisfaction = "qs501_b_2",
        study_hard = "qs601n",
        cesd20 = "cesd20sc",
        cesd8 = "cesd8",
        cesd_sleep = "qn411",
        cesd_happy = "qn412",
        lonely = "qn414",
        cesd_enjoyed = "qn416",
        sad = "qn418",
        cesd_notgoing = "qn420",
        happiness = "qm2016",
        life_satisfaction = "qn12012",
        word_test = "wordtest18",
        math_test = "mathtest18",
        rse_1 = "qm101m",
        rse_2 = "qm102m",
        rse_3 = "qm103m",
        rse_4 = "qm104m",
        rse_5 = "qm105m",
        rse_6 = "qm106m",
        rse_7 = "qm107m",
        rse_8 = "qm108m",
        rse_9 = "qm109m",
        rse_10 = "qm110m",
        # Youth (10-15) applicability items, 1 = totally inapplicable ...
        # 5 = totally applicable
        int_angry_study = "qint001",
        ext_argue = "qext002",
        ext_attention = "qext004",
        ext_distracted = "qext006",
        ext_homework = "qext008",
        int_worry_school = "qint009",
        ext_fight = "qext013"
      ),
      famconf = "cfps2018famconf",
      famconf_vars = c(
        pid = "pid",
        fid = "fid18",
        sex = "tb2_a_p",
        birth_year = "tb1y_a_p",
        birth_month = "tb1m_a_p",
        education_child = "tb4_a18_p",
        education_father = "tb4_a18_f",
        education_mother = "tb4_a18_m",
        hukou = "hukou_a18_p",
        father_resident = "tb6_a18_f",
        mother_resident = "tb6_a18_m",
        father_alive = "alive_a18_f",
        mother_alive = "alive_a18_m"
      ),
      famecon = "cfps2018famecon",
      famecon_vars = c(
        fid = "fid18",
        household_income = "fincome1",
        household_income_pc = "fincome1_per",
        household_size = "familysize18",
        household_savings = "savings"
      )
    ),
    list(
      child = "cfps2020child",
      child_vars = c(
        pid = "pid",
        fid = "fid20",
        province = "provcd20",
        county = "countyid20",
        urban = "urban20",
        weight_cross = "rswt_natcs20n",
        psu = "psu",
        subpopulation = "subpopulation",
        # Reversed coding, as in 2018
        tv_restrict_2018 = "wf605mn",
        birth_address = "wa401",
        age_reported = "age",
        interview_year_parent = "cyear",
        interview_month_parent = "cmonth",
        school_grade = "wc5",
        school_stage = "wc3",
        ethnicity_code = "wa701code",
        internet_any_parent = "wu4",
        learning_online_user_parent = "wu5",
        tv_week_hours_parent = "wb9",
        internet_day_mins_parent = "wu401",
        learning_online_day_mins_parent = "wu501",
        grade_chinese_parent = "wf501",
        grade_maths_parent = "wf502"
      ),
      individual = "cfps2020person",
      individual_vars = c(
        pid = "pid",
        fid = "fid20",
        province = "provcd20",
        county = "countyid20",
        urban = "urban20",
        weight_cross = "rswt_natcs20n",
        psu = "psu",
        subpopulation = "subpopulation",
        smoked_month = "qq201",
        academic_pressure = "qs502",
        sleep_day_hours = "qq4010",
        sleep_wd_hours = "qq4011",
        sleep_we_hours = "qq4012",
        birth_address = "qa401",
        age_reported = "age",
        interview_year_self = "cyear",
        interview_month_self = "cmonth",
        ethnicity_code = "qa701code",
        internet_mobile_user = "qu201",
        internet_pc_user = "qu202",
        self_rated_health = "qp201",
        tv_week_hours_self = "qq1001",
        internet_mobile_day_mins_self = "qu201a",
        internet_pc_day_mins_self = "qu202a",
        online_games_week = "qu91",
        online_shopping_week = "qu92",
        wechat_moments_freq = "qu111",
        class_cut = "kr4302",
        class_rank = "kr425",
        school_satisfaction = "qs701_b_2",
        schoolwork_satisfaction = "qs501_b_2",
        study_hard = "qs601n",
        cesd20 = "cesd20sc",
        cesd8 = "cesd8",
        cesd_sleep = "qn411",
        cesd_happy = "qn412",
        lonely = "qn414",
        cesd_enjoyed = "qn416",
        sad = "qn418",
        cesd_notgoing = "qn420",
        happiness = "qm2016",
        life_satisfaction = "qn12012",
        number_series_w = "ns_w"
      ),
      famconf = "cfps2020famconf",
      famconf_vars = c(
        pid = "pid",
        fid = "fid20",
        sex = "tb2_a_p",
        birth_year = "tb1y_a_p",
        birth_month = "tb1m_a_p",
        education_child = "tb4_a20_p",
        education_father = "tb4_a20_f",
        education_mother = "tb4_a20_m",
        hukou = "hukou_a20_p",
        father_resident = "tb6_a20_f",
        mother_resident = "tb6_a20_m",
        father_alive = "alive_a20_f",
        mother_alive = "alive_a20_m"
      ),
      famecon = "cfps2020famecon",
      famecon_vars = c(
        fid = "fid20",
        household_income = "fincome1",
        household_income_pc = "fincome1_per",
        household_size = "familysize20",
        household_savings = "savings"
      )
    ),
    list(
      child = "cfps2022child",
      child_vars = c(
        pid = "pid",
        fid = "fid22",
        province = "provcd22",
        county = "countyid22",
        urban = "urban22",
        weight_cross = "rswt_natcs22n",
        psu = "psu",
        subpopulation = "subpopulation",
        birth_address = "wa401",
        age_reported = "age",
        interview_year_parent = "cyear",
        interview_month_parent = "cmonth",
        school_grade = "wc5",
        school_stage = "wc3",
        ethnicity_code = "wa701code",
        internet_any_parent = "wu4",
        learning_online_user_parent = "wu5",
        tv_week_hours_parent = "wb9",
        internet_day_mins_parent = "wu401",
        learning_online_day_mins_parent = "wu501",
        grade_chinese_parent = "wf501",
        grade_maths_parent = "wf502"
      ),
      individual = "cfps2022person",
      individual_vars = c(
        pid = "pid",
        fid = "fid22",
        province = "provcd22",
        county = "countyid22",
        urban = "urban22",
        weight_cross = "rswt_natcs22n",
        psu = "psu",
        subpopulation = "subpopulation",
        smoked_month = "qq201",
        academic_pressure = "qs502",
        sleep_day_hours = "qq4010",
        sleep_wd_hours = "qq4011",
        sleep_we_hours = "qq4012",
        birth_address = "qa401",
        age_reported = "age",
        interview_year_self = "cyear",
        interview_month_self = "cmonth",
        ethnicity_code = "qa701code",
        internet_mobile_user = "qu201",
        internet_pc_user = "qu202",
        learning_online_user_self = "qu5",
        self_rated_health = "qp201",
        tv_week_hours_self = "qq1001",
        internet_mobile_day_mins_self = "qu201a",
        internet_pc_day_mins_self = "qu202a",
        learning_online_day_mins_self = "qu501",
        online_games_week = "qu91",
        online_shopping_week = "qu92",
        wechat_moments_freq = "qu111",
        class_cut = "kr4302",
        class_rank = "kr425",
        school_satisfaction = "qs701",
        schoolwork_satisfaction = "qs501_b_2",
        study_hard = "qs601n",
        cesd20 = "cesd20sc",
        cesd8 = "cesd8",
        cesd_sleep = "qn411",
        cesd_happy = "qn412",
        lonely = "qn414",
        cesd_enjoyed = "qn416",
        sad = "qn418",
        cesd_notgoing = "qn420",
        happiness = "qm2016",
        life_satisfaction = "qn12012",
        life_meaningful = "qm3n",
        word_test = "wordtest22",
        math_test = "mathtest22",
        int_angry_study = "qint001",
        ext_argue = "qext002",
        ext_attention = "qext004",
        ext_distracted = "qext006",
        ext_homework = "qext008",
        int_worry_school = "qint009",
        ext_fight = "qext013"
      ),
      famconf = "cfps2022famconf",
      famconf_vars = c(
        pid = "pid",
        fid = "fid22",
        sex = "tb2_a_p",
        birth_year = "tb1y_a_p",
        birth_month = "tb1m_a_p",
        education_child = "tb4_a22_p",
        education_father = "tb4_a22_f",
        education_mother = "tb4_a22_m",
        hukou = "hukou_a22_p",
        father_resident = "tb6_a22_f",
        mother_resident = "tb6_a22_m",
        father_alive = "alive_a22_f",
        mother_alive = "alive_a22_m"
      ),
      famecon = "cfps2022famecon",
      famecon_vars = c(
        fid = "fid22",
        household_income = "fincome1",
        household_income_pc = "fincome1_per",
        household_size = "familysize22",
        household_savings = "savings"
      )
    )
  )
}

#' Pull one resource, keeping and renaming only the mapped columns
bp873_take <- function(raw_dataset, resource, vars, key) {
  tbl <- raw_dataset$data[[resource]]
  if (is.null(tbl)) {
    stop("BPIPD-873: resource `", resource, "` is missing.", call. = FALSE)
  }

  absent <- setdiff(unname(vars), names(tbl))
  if (length(absent) > 0) {
    stop(
      "BPIPD-873: `",
      resource,
      "` is missing: ",
      paste(absent, collapse = ", "),
      call. = FALSE
    )
  }

  tbl <- dplyr::select(tibble::as_tibble(tbl), dplyr::all_of(vars))

  # CFPS reserves -1 to -10 as non-response codes in every file
  tbl[] <- lapply(tbl, function(x) {
    codes <- attr(x, "labels", exact = TRUE)
    if (!is.numeric(codes)) {
      return(x)
    }
    reserved <- codes[codes >= -10 & codes <= -1]
    x[unclass(x) %in% reserved] <- NA
    kept <- codes[!codes %in% reserved]
    if (length(kept) == 0) {
      return(haven::zap_labels(x))
    }
    attr(x, "labels") <- kept
    x
  })

  # Rows without a key are empty shells
  tbl[!is.na(tbl[[key]]), ]
}

#' Reduce a lookup table to one row per key, keeping the fullest row
#'
#' `key` may name several columns, because a person's roster record is only
#' well defined within one family.
bp873_one_per_key <- function(tbl, key) {
  ordering <- c(unname(as.list(tbl[key])), list(-rowSums(!is.na(tbl))))
  tbl <- tbl[do.call(order, ordering), ]
  tbl[!duplicated(tbl[key]), ]
}

#' Attach the family roster to the children of that family
bp873_add_roster <- function(tbl, roster) {
  columns <- setdiff(names(roster), c("pid", "fid"))

  per_family <- bp873_one_per_key(roster, c("pid", "fid"))
  per_family$.on_roster <- TRUE
  joined <- dplyr::left_join(
    tbl,
    per_family,
    by = c("pid", "fid"),
    relationship = "many-to-one"
  )

  per_person <- bp873_one_per_key(roster, "pid")
  missed <- is.na(joined$.on_roster)
  joined[missed, columns] <- per_person[
    match(joined$pid[missed], per_person$pid),
    columns
  ]

  joined[setdiff(names(joined), ".on_roster")]
}

#' Restore one variable label per mapped column after binding
bp873_label_wording_ok <- c(
  "education_child",
  "education_father",
  "education_mother",
  "father_resident",
  "mother_resident",
  "father_alive",
  "mother_alive",
  "internet_any_self",
  "school_stage",
  "class_rank",
  "study_hard",
  "grade_chinese_parent",
  "grade_maths_parent",
  "int_angry_study",
  "ext_argue",
  "ext_attention",
  "ext_distracted",
  "ext_homework",
  "int_worry_school",
  "ext_fight",
  "subpopulation",
  "smoked_month",
  "birth_address_2014"
)

#' Bind the waves, allowing only the reviewed value-label wording differences
bp873_bind_waves <- function(waves) {
  conflicts <- character(0)
  df <- withCallingHandlers(
    dplyr::bind_rows(waves),
    warning = function(w) {
      message_text <- conditionMessage(w)
      if (grepl("conflicting\\s+value\\s+labels", message_text)) {
        conflicts <<- c(
          conflicts,
          sub("^.*\\$([A-Za-z0-9_.]+)`.*$", "\\1", message_text)
        )
        invokeRestart("muffleWarning")
      }
    }
  )
  unreviewed <- setdiff(unique(conflicts), bp873_label_wording_ok)
  if (length(unreviewed) > 0) {
    stop(
      "BPIPD-873: value labels for the same code differ between waves in: ",
      paste(unreviewed, collapse = ", "),
      ". Give the coding its own column (as for `school_stage_2010`) or, if ",
      "only the wording differs, add it to `bp873_label_wording_ok`.",
      call. = FALSE
    )
  }
  df
}

bp873_relabel <- function(df, raw_dataset) {
  labels <- list()
  for (wave in bp873_waves()) {
    for (part in c("child", "individual", "famconf", "famecon")) {
      tbl <- raw_dataset$data[[wave[[part]]]]
      vars <- wave[[paste0(part, "_vars")]]
      if (is.null(tbl)) {
        next
      }
      for (i in seq_along(vars)) {
        label <- attr(tbl[[vars[[i]]]], "label", exact = TRUE)
        name <- names(vars)[[i]]
        if (!is.null(label) && is.null(labels[[name]])) {
          labels[[name]] <- label
        }
      }
    }
  }

  for (name in intersect(names(labels), names(df))) {
    if (is.null(attr(df[[name]], "label", exact = TRUE))) {
      attr(df[[name]], "label") <- labels[[name]]
    }
  }
  df
}

#' Join two tables that may both supply the same column
bp873_merge <- function(x, y, by) {
  shared <- setdiff(intersect(names(x), names(y)), by)
  joined <- dplyr::full_join(
    x,
    y,
    by = by,
    suffix = c("", ".y"),
    relationship = "one-to-one"
  )

  # As in `bp873_bind_waves()`, only reviewed wording differences may pass.
  for (nm in shared) {
    joined[[nm]] <- withCallingHandlers(
      dplyr::coalesce(joined[[nm]], joined[[paste0(nm, ".y")]]),
      warning = function(w) {
        if (grepl("conflicting\\s+value\\s+labels", conditionMessage(w))) {
          if (!nm %in% bp873_label_wording_ok) {
            stop(
              "BPIPD-873: value labels for `",
              nm,
              "` differ between the merged files; if only the wording ",
              "differs, add it to `bp873_label_wording_ok`.",
              call. = FALSE
            )
          }
          invokeRestart("muffleWarning")
        }
      }
    )
  }

  joined[setdiff(names(joined), paste0(shared, ".y"))]
}
