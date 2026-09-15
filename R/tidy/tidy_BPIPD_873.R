#' Tidier for BPIPD-873 (China Family Panel Studies)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble
tidy_BPIPD_873 <- function(raw_dataset, spec) {
  # 2012 asked nothing about screen use, so the whole wave is skipped.
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
      dplyr::left_join(
        bp873_one_per_key(roster, "pid"),
        by = "pid",
        relationship = "many-to-one"
      ) |>
      dplyr::left_join(economy, by = "fid", relationship = "many-to-one")
  })

  df <- dplyr::bind_rows(waves)

  df$ethnicity_code <- stats::ave(
    df$ethnicity_code,
    df$pid,
    FUN = function(x) if (all(is.na(x))) x else x[!is.na(x)][1]
  )

  if (anyDuplicated(df[c("pid", "wave")]) > 0) {
    stop(
      "BPIPD-873: `pid` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  # Keep children and adolescents only
  age <- dplyr::coalesce(df$age_reported, as.numeric(df$wave) - df$birth_year)
  df[!is.na(age) & age <= 19, ]
}

#' Per-wave source files and their column maps
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
        age_reported = "wa1age",
        school_grade = "wf302",
        school_stage = "wf301",
        ethnicity_code = "wa6code",
        internet_any = "ku2",
        media_wd_hours_self = "kt401_a_1",
        media_we_hours_self = "kt401_a_2",
        tv_wd_hours_self = "kt402_a_1",
        tv_we_hours_self = "kt402_a_2",
        internet_wd_hours_self = "kt403_a_1",
        internet_we_hours_self = "kt403_a_2",
        word_test = "wordtest",
        math_test = "mathtest"
      ),
      individual = "cfps2010adult",
      individual_vars = c(
        pid = "pid",
        fid = "fid",
        province = "provcd",
        county = "countyid",
        urban = "urban",
        age_reported = "qa1age",
        ethnicity_code = "qa5code",
        internet_any = "ku2",
        media_wd_hours_self = "kt401_a_1",
        media_we_hours_self = "kt401_a_2",
        tv_wd_hours_self = "kt402_a_1",
        tv_we_hours_self = "kt402_a_2",
        internet_wd_hours_self = "kt403_a_1",
        internet_we_hours_self = "kt403_a_2",
        internet_day_hours_self = "ku250",
        word_test = "wordtest",
        math_test = "mathtest"
      ),
      famconf = "cfps2010famconf",
      famconf_vars = c(
        pid = "pid",
        sex = "tb2_a_p",
        birth_year = "tb1y_a_p",
        birth_month = "tb1m_a_p",
        education_child = "tb4_a_p",
        education_father = "tb4_a_f",
        education_mother = "tb4_a_m",
        hukou = "td8_a_p",
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
        age_reported = "cfps2014_age",
        school_grade = "wf302",
        school_stage = "wf301m",
        ethnicity_code = "wa6code",
        internet_any = "ku2",
        tv_week_hours_parent = "wb9",
        internet_week_hours_self = "ku250m",
        word_test = "wordtest14",
        math_test = "mathtest14"
      ),
      individual = "cfps2014adult",
      individual_vars = c(
        pid = "pid",
        fid = "fid14",
        province = "provcd14",
        county = "countyid14",
        urban = "urban14",
        age_reported = "cfps2014_age",
        ethnicity_code = "qa701code",
        internet_any = "ku2",
        self_rated_health = "qp201",
        tv_week_hours_self = "qq1001",
        internet_week_hours_self = "ku250m",
        word_test = "wordtest14",
        math_test = "mathtest14"
      ),
      famconf = "cfps2014famconf",
      famconf_vars = c(
        pid = "pid",
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
        age_reported = "cfps_age",
        school_grade = "ppc5_b_1",
        school_stage = "pc3_b_1",
        ethnicity_code = "pa701code",
        internet_mobile_user = "ku201",
        internet_pc_user = "ku202",
        tv_week_hours_parent = "wb9",
        internet_week_hours_self = "ku250m",
        cesd20 = "cesd20sc",
        happiness = "qm2014"
      ),
      individual = "cfps2016adult",
      individual_vars = c(
        pid = "pid",
        fid = "fid16",
        province = "provcd16",
        county = "countyid16",
        urban = "urban16",
        age_reported = "cfps_age",
        ethnicity_code = "pa701code",
        internet_mobile_user = "ku201",
        internet_pc_user = "ku202",
        self_rated_health = "qp201",
        tv_week_hours_self = "qq1001",
        internet_week_hours_self = "ku250m",
        cesd20 = "cesd20sc",
        happiness = "qm2014"
      ),
      famconf = "cfps2016famconf",
      famconf_vars = c(
        pid = "pid",
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
        age_reported = "age",
        school_grade = "wc5_b_2",
        school_stage = "wc3_b_2",
        ethnicity_code = "wa701code",
        tv_week_hours_parent = "wb9"
      ),
      individual = "cfps2018person",
      individual_vars = c(
        pid = "pid",
        fid = "fid18",
        province = "provcd18",
        county = "countyid18",
        urban = "urban18",
        age_reported = "age",
        ethnicity_code = "qa701code",
        internet_mobile_user = "qu201",
        internet_pc_user = "qu202",
        self_rated_health = "qp201",
        tv_week_hours_self = "qq1001",
        internet_week_hours_self = "qu250m",
        cesd20 = "cesd20sc",
        cesd8 = "cesd8",
        happiness = "qm2016",
        lonely = "qn414",
        word_test = "wordtest18",
        math_test = "mathtest18"
      ),
      famconf = "cfps2018famconf",
      famconf_vars = c(
        pid = "pid",
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
        age_reported = "age",
        school_grade = "wc5",
        school_stage = "wc3",
        ethnicity_code = "wa701code",
        internet_any = "wu4",
        learning_online_user = "wu5",
        tv_week_hours_parent = "wb9",
        internet_day_mins_parent = "wu401",
        learning_online_day_mins_parent = "wu501"
      ),
      individual = "cfps2020person",
      individual_vars = c(
        pid = "pid",
        fid = "fid20",
        province = "provcd20",
        county = "countyid20",
        urban = "urban20",
        age_reported = "age",
        ethnicity_code = "qa701code",
        internet_mobile_user = "qu201",
        internet_pc_user = "qu202",
        self_rated_health = "qp201",
        tv_week_hours_self = "qq1001",
        internet_mobile_day_mins_self = "qu201a",
        internet_pc_day_mins_self = "qu202a",
        cesd20 = "cesd20sc",
        cesd8 = "cesd8",
        happiness = "qm2016",
        lonely = "qn414"
      ),
      famconf = "cfps2020famconf",
      famconf_vars = c(
        pid = "pid",
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
        age_reported = "age",
        school_grade = "wc5",
        school_stage = "wc3",
        ethnicity_code = "wa701code",
        internet_any = "wu4",
        learning_online_user = "wu5",
        tv_week_hours_parent = "wb9",
        internet_day_mins_parent = "wu401",
        learning_online_day_mins_parent = "wu501"
      ),
      individual = "cfps2022person",
      individual_vars = c(
        pid = "pid",
        fid = "fid22",
        province = "provcd22",
        county = "countyid22",
        urban = "urban22",
        age_reported = "age",
        ethnicity_code = "qa701code",
        internet_mobile_user = "qu201",
        internet_pc_user = "qu202",
        learning_online_user = "qu5",
        self_rated_health = "qp201",
        tv_week_hours_self = "qq1001",
        internet_mobile_day_mins_self = "qu201a",
        internet_pc_day_mins_self = "qu202a",
        learning_online_day_mins_self = "qu501",
        cesd20 = "cesd20sc",
        cesd8 = "cesd8",
        happiness = "qm2016",
        lonely = "qn414",
        word_test = "wordtest22",
        math_test = "mathtest22"
      ),
      famconf = "cfps2022famconf",
      famconf_vars = c(
        pid = "pid",
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

  tbl[] <- lapply(tbl, function(x) {
    codes <- attr(x, "labels", exact = TRUE)
    reserved <- if (is.numeric(codes)) codes[codes >= -10 & codes <= -1]
    x <- haven::zap_labels(x)
    x[x %in% reserved] <- NA
    x
  })

  tbl[!is.na(tbl[[key]]), ]
}

#' Reduce a lookup table to one row per key, keeping the fullest row
bp873_one_per_key <- function(tbl, key) {
  tbl <- tbl[order(tbl[[key]], -rowSums(!is.na(tbl))), ]
  tbl[!duplicated(tbl[[key]]), ]
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

  for (nm in shared) {
    joined[[nm]] <- dplyr::coalesce(joined[[nm]], joined[[paste0(nm, ".y")]])
  }

  joined[setdiff(names(joined), paste0(shared, ".y"))]
}
