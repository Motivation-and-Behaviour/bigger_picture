#' Tidier for BPIPD-535 (SWITCH)
#'
#' One merged SPSS file holds each child's four assessments side by side; this
#' stacks them and carries the parent's background items across waves.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per wave attended
tidy_BPIPD_535 <- function(raw_dataset, spec) {
  wide <- tibble::as_tibble(raw_dataset$data[[1]])

  # No source identifier covers every child (`UniqueID` is missing for 23), so
  # the row position in the merged file is what links a child's four wave rows.
  wide$participant_id <- sprintf("%04d", seq_len(nrow(wide)))

  # Names, the birth date, teacher names and free text stay out of the tidied
  # table; the birth date is kept aside only to derive age below.
  birth <- stats::setNames(as.Date(wide$Birthdate), wide$participant_id)
  wide <- wide[setdiff(names(wide), bp535_identifying())]

  wide$ObsDateW1 <- as.Date(wide$ObsDateW1)

  # One wave-4 Tuesday cell holds a free-text note instead of a step count.
  wide$Tues <- readr::parse_number(wide$Tues)

  waves <- bp535_waves()
  wide <- bp535_unify_value_labels(wide, bp535_label_groups(waves, names(wide)))
  tables <- Map(bp535_take, names(waves), waves, MoreArgs = list(wide = wide))
  long <- dplyr::bind_rows(tables)

  # A child with nothing recorded in a wave is not an observation of that wave;
  # blank strings are how this file writes missing into its character columns.
  measured <- setdiff(
    names(long),
    c("participant_id", "wave", bp535_constant(), bp535_administrative())
  )
  seen <- lapply(long[measured], function(x) {
    if (is.character(x)) !is.na(x) & trimws(x) != "" else !is.na(x)
  })
  long <- long[Reduce(`|`, seen), , drop = FALSE]

  # Binding and slicing drop the variable labels of plain and time columns.
  long <- bp535_relabel_items(bp535_restore_labels(long, tables))

  attr(long$Date, "label") <-
    "Survey date as MMDDYYYY (Date, Date2, Date3, Date4)"
  date <- as.Date(ISOdate(
    long$Date %% 1e4,
    long$Date %/% 1e6,
    (long$Date %/% 1e4) %% 100
  ))
  long$age_dob <- as.numeric(date - birth[long$participant_id]) / 365.25
  attr(long$age_dob, "label") <-
    "Decimal age from date of birth to the child survey date (Date)"

  # Parent items 160-164 were asked at waves 1 and 3 only; `q162` is the
  # answering parent's own education, so its respondent travels with it.
  source_row <- carry_within(
    ifelse(is.na(long$q162), NA_integer_, seq_len(nrow(long))),
    long$participant_id,
    as.integer(long$wave)
  )
  long$q159_q162 <- long$q159[source_row]
  attr(long$q159_q162, "label") <- "Respondent (q159) behind the carried q162"
  long$q162 <- carry_within(
    long$q162,
    long$participant_id,
    as.integer(long$wave)
  )
  attr(long$q162, "label") <- paste0(
    attr(long$q162, "label"),
    "; missing waves carry the earliest observed wave"
  )
  # The parent survey is undated, so the 12-month gap uses the child survey month.
  month <- 12 * as.integer(format(date, "%Y")) + as.integer(format(date, "%m"))
  for (col in c("q160", "q163")) {
    long[[col]] <- bp535_carry_gap(long[[col]], long$participant_id, month)
    attr(long[[col]], "label") <- paste0(
      attr(long[[col]], "label"),
      "; missing waves carry the nearest wave surveyed 12 months or less away"
    )
  }
  long <- bp535_fill_age(long, date)

  if (anyDuplicated(long[c("participant_id", "wave")]) > 0) {
    stop(
      "BPIPD-535: `participant_id` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  long
}

#' Columns that describe the child rather than one assessment
bp535_constant <- function() {
  c(
    "participant_id",
    "UniqueID",
    "SEQNO",
    "ID2",
    "B_ID",
    "b_ID2",
    "ID_3",
    "IDW4",
    "ID2W4",
    "SEQNOw4",
    "PrimaryLast",
    "ComID",
    "Demo",
    "dm_or_ct",
    "DemoorControl",
    "district",
    "Race",
    "Minority",
    "Sex",
    "Consistency"
  )
}

#' Source columns holding names, the birth date or free text, deliberately unmapped
bp535_identifying <- function() {
  c(
    "LastName",
    "FirstName",
    "Birthdate",
    "Teacher_W1",
    "TeacherW2",
    "TeacherW3",
    "TEACHERw4",
    "NOTES_W1",
    "NOTESW3",
    bp535_day_columns("Comments")
  )
}

#' Roster, wear-time and summary columns the file fills in for every child
bp535_administrative <- function() {
  c(
    "Site",
    "SCHOOL",
    "GRADE",
    "TEACHER",
    "Demo_wavefile",
    "Date",
    "tdate",
    "grade_roster",
    "school_roster",
    "Include",
    "T_F",
    "QualWk",
    "QualWkday",
    "QualWkend",
    "MeetVP",
    "MeetTL",
    "Mean_Wk_Time",
    "Mean_Wkday_Time",
    "Mean_Wkend_Time",
    "SD_Wk_Time",
    "SD_Wkday_Time",
    "SD_Wkend_Time",
    "filter_ped",
    "vprec",
    "tlrec",
    "filter_n173",
    bp535_day_columns(c("on_min", "off_min", "Totmin", "Qual"))
  )
}

#' Wave-4 per-day pedometer columns, for the given suffixes
bp535_day_columns <- function(suffixes) {
  days <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
  as.vector(t(outer(days, suffixes, paste, sep = "_")))
}

#' Source column for each wave, keyed by the name the tidied table uses
bp535_waves <- function() {
  # Measurement columns are suffixed `W1`-`W3`; wave 4's carry no suffix.
  list(
    "1" = c(
      Site = "Site",
      SCHOOL = "SCHOOL",
      GRADE = "GRADE",
      TEACHER = "TEACHER",
      Date = "Date",
      tdate = "tdate1",
      grade_roster = "Grade_W1",
      school_roster = "School_W1",
      ObsDate = "ObsDateW1",
      Age_yrs = "ageW1",
      Height = "HeightW1",
      Height_2 = "Height2W1",
      HtTech_1 = "HtTech1W1",
      HtTech_2 = "HtTech2W1",
      SitHt = "SitHtW1",
      SitHt_2 = "SitHt2W1",
      SitHtTech_1 = "SitHtTech1W1",
      SitHtTech_2 = "SitHtTech2W1",
      SitHtBox = "SitHtBoxW1",
      TrueSitHt = "True_SitHtW1",
      LegLength = "Leg_LengthW1",
      Weight_lbs = "Weight_lbsW1",
      Weight_kg = "Weight_kgW1",
      Weight2_lbs = "Weight2_lbsW1",
      Weight2_kg = "Weight2_kgW1",
      WtTech_1 = "WtTech1W1",
      WtTech_2 = "WtTech2W1",
      BMI = "BMI_W1",
      WeightStat = "wtstat_W1",
      WeightStat_Di = "WeightStatW1_Di",
      WC = "WC_W1",
      WC_2 = "WC2_W1",
      WaistStat = "waiststa_W1",
      WaistStat_Di = "WaistStatW1_Di",
      WstTech_1 = "WstTech1W1",
      WstTech_2 = "WstTech2W1",
      MatOffset = "MatOffset_W1",
      estAPHV = "estAPHV_W1",
      Ped_num = "Ped#_W1",
      MeetVP = "MeetVPW1",
      MeetTL = "MeetTLW1",
      Mon = "MonW1",
      Tues = "TuesW1",
      Wed = "WedW1",
      Thurs = "ThursW1",
      Fri = "FriW1",
      Sat = "SatW1",
      Sun = "SunW1",
      Include = "IncludeW1",
      Mean_Wk_Ped = "Mean_Wk_PedW1",
      Mean_Wk_Time = "Mean_Wk_TimeW1",
      Mean_Wkday_Ped = "Mean_Wkday_PedW1",
      Mean_Wkday_Time = "Mean_Wkday_TimeW1",
      Mean_Wkend_Ped = "Mean_Wkend_PedW1",
      Mean_Wkend_Time = "Mean_Wkend_TimeW1",
      SD_Wk_Ped = "SD_Wk_PedW1",
      SD_Wk_Time = "SD_Wk_TimeW1",
      SD_Wkday_Ped = "SD_Wkday_PedW1",
      SD_Wkday_Time = "SD_Wkday_TimeW1",
      SD_Wkend_Ped = "SD_Wkend_PedW1",
      SD_Wkend_Time = "SD_Wkend_TimeW1",
      filter_ped = "filter_$W1",
      vprec = "vprecW1",
      tlrec = "tlrecW1"
    ),
    "2" = c(
      Site = "Site2",
      SCHOOL = "SCHOOL2",
      GRADE = "GRADE2",
      TEACHER = "TEACHER2",
      Demo_wavefile = "Demo2",
      Date = "Date2",
      grade_roster = "GradeW2_",
      school_roster = "SchoolW2",
      Ped_num = "Ped#W2",
      Mon = "MonW2",
      Tues = "TuesW2",
      Wed = "WedW2",
      Thurs = "ThursW2",
      Fri = "FriW2",
      Sat = "SatW2",
      Sun = "SunW2",
      T_F = "T_FW2",
      Include = "IncludeW2",
      Mean_Wk_Ped = "Mean_Wk_PedW2",
      Mean_Wk_Time = "Mean_Wk_TimeW2",
      Mean_Wkday_Ped = "Mean_Wkday_PedW2",
      Mean_Wkday_Time = "Mean_Wkday_TimeW2",
      Mean_Wkend_Ped = "Mean_Wkend_PedW2",
      Mean_Wkend_Time = "Mean_Wkend_TimeW2"
    ),
    "3" = c(
      Site = "Site_3",
      SCHOOL = "SCHOOL_3",
      GRADE = "GRADE_3",
      TEACHER = "TEACHER_3",
      Demo_wavefile = "Demo3",
      Date = "Date3",
      tdate = "tdate3",
      grade_roster = "GradeW3_",
      school_roster = "SchoolW3",
      ObsDate = "ObsDateW3",
      Age_yrs = "Age_yrs_W3",
      Height = "HeightW3",
      Height_2 = "Height_2W3",
      HtTech_1 = "HtTech_1W3",
      SitHt = "SitHtW3",
      SitHt_2 = "SitHt_2W3",
      SitHtTech_1 = "SitHtTech_1W3",
      SitHtBox = "SitHtBoxW3",
      TrueSitHt = "TrueSitHtW3",
      LegLength = "LegLengthW3",
      Weight_lbs = "Weight_lbsW3",
      Weight_kg = "Weight_kgW3",
      Weight2_lbs = "Weight2_lbsW3",
      Weight2_kg = "Weight2_kgW3",
      WtTech_1 = "WtTechW3",
      BMI = "BMI_W3",
      WC = "WCW3",
      WC_2 = "WC_2W3",
      WstTech_1 = "WstTechW3",
      MatOffset = "MatOffsetW3",
      estAPHV = "estAPHV_W3",
      Ped_num = "Ped#W3",
      Mon = "MonW3",
      Tues = "TuesW3",
      Wed = "WedW3",
      Thurs = "ThursW3",
      Fri = "FriW3",
      Sat = "SatW3",
      Sun = "SunW3",
      T_F = "T_FW3",
      Include = "IncludeW3",
      Mean_Wk_Ped = "Mean_Wk_PedW3",
      Mean_Wk_Time = "Mean_Wk_TimeW3",
      Mean_Wkday_Ped = "Mean_Wkday_PedW3",
      Mean_Wkday_Time = "Mean_Wkday_TimeW3",
      Mean_Wkend_Ped = "Mean_Wkend_PedW3",
      Mean_Wkend_Time = "Mean_Wkend_TimeW3",
      filter_n173 = "filter_$",
      VAR00001 = "VAR00001",
      VAR00002 = "VAR00002",
      Child_involvement = "Child_involvement",
      Child_Involvement3 = "Child_Involvement3",
      PChild_involvement = "PChild_involvement",
      PChild_Involvement3 = "PChild_Involvement3"
    ),
    "4" = c(
      Site = "SiteW4",
      SCHOOL = "SCHOOLw4",
      GRADE = "GRADEw4",
      Demo_wavefile = "Demow4",
      Date = "Date4",
      tdate = "tdate4",
      ndteacher = "ndteacher",
      SexW4 = "SexW4",
      Age_yrs = "Age_W4",
      Height = "HeightW4",
      SitHt = "SitW4",
      SitHtBox = "SitHtBxW4",
      TrueSitHt = "SitHeightW4",
      LegLength = "LegLW4",
      Weight_lbs = "Weightlbs_W4W4",
      Weight_kg = "WeightkgW4",
      BMI = "BMI_W4",
      WeightStat = "WeightStat_W4",
      WeightStat_Di = "WeightStatDi_W4",
      WC = "WC_W4",
      WaistStat = "WaistStat_W4",
      WaistStat_Di = "WaistStatDi_W4",
      Ped_num = "Ped#",
      Mon = "Mon",
      Tues = "Tues",
      Wed = "Wed",
      Thurs = "Thurs",
      Fri = "Fri",
      Sat = "Sat",
      Sun = "Sun",
      QualWk = "QualWk",
      QualWkday = "QualWkday",
      QualWkend = "QualWkend",
      T_F = "T_F",
      Include = "Include",
      Mean_Wk_Ped = "Mean_Wk_Ped",
      Mean_Wk_Time = "Mean_Wk_Time",
      Mean_Wkday_Ped = "Mean_Wkday_Ped",
      Mean_Wkday_Time = "Mean_Wkday_Time",
      Mean_Wkend_Ped = "Mean_Wkend_Ped",
      Mean_Wkend_Time = "Mean_Wkend_Time",
      stats::setNames(bp535_wave4_extras(), bp535_wave4_extras())
    )
  )
}

#' Wave-4 only pedometer wear-time detail and late-added survey items
bp535_wave4_extras <- function() {
  c(
    bp535_day_columns(
      c("on", "on_min", "off", "off_min", "Totmin", "Qual")
    ),
    "nd013",
    "nd036",
    "nd051",
    "nd052",
    "nd115",
    "nd145",
    "nd146",
    "nd147",
    "nd148"
  )
}

#' Source columns that end up under one name, grouped by that name
bp535_label_groups <- function(waves, nms) {
  design <- unlist(unname(waves))
  items <- grep("^[vcd][0-9]{3}", nms, value = TRUE)
  scores <- grep("^[CPT][0-9]_", nms, value = TRUE)
  c(
    split(unname(design), names(design)),
    split(items, paste0("q", substring(items, 2L))),
    split(scores, sub("^([CPT])[0-9]_", "\\1_", scores))
  )
}

#' Copy value labels onto the waves of a column that were stored unlabelled
bp535_unify_value_labels <- function(wide, groups) {
  for (cols in groups) {
    labelled <- Filter(
      function(v) !is.null(attr(wide[[v]], "labels", exact = TRUE)),
      cols
    )
    if (length(labelled) == 0L || length(labelled) == length(cols)) {
      next
    }
    labels <- attr(wide[[labelled[[1L]]]], "labels", exact = TRUE)
    for (v in setdiff(cols, labelled)) {
      lab <- attr(wide[[v]], "label")
      wide[[v]] <- haven::labelled(as.vector(wide[[v]]), labels = labels)
      attr(wide[[v]], "label") <- lab
    }
  }
  wide
}

#' Select and rename one wave's columns
bp535_take <- function(wave, map, wide) {
  prefix <- c("1" = "v", "2" = "b", "3" = "c", "4" = "d")[[wave]]
  items <- grep(paste0("^", prefix, "[0-9]{3}"), names(wide), value = TRUE)

  # Wave 2 codes several of its response scales in reverse, so its items keep
  # their source names instead of stacking into the shared `q` columns.
  item_map <- if (wave == "2") {
    stats::setNames(items, items)
  } else {
    stats::setNames(items, paste0("q", substring(items, 2L)))
  }

  # Wave-3 parent items `n159`-`n186` renumber questions that `c159`-`c164`
  # already use, so they keep their source names too.
  extra <- grep("^(n[0-9]{3}|Z[cn][0-9]{3})", names(wide), value = TRUE)
  extra_map <- if (wave == "3") stats::setNames(extra, extra) else character()

  scores <- grep(paste0("^[CPT]", wave, "_"), names(wide), value = TRUE)
  who <- sub("^C[0-9]_", "child_", scores)
  who <- sub("^P[0-9]_", "parent_", who)
  score_map <- stats::setNames(scores, sub("^T[0-9]_", "teacher_", who))

  keep <- c(
    stats::setNames(bp535_constant(), bp535_constant()),
    item_map,
    extra_map,
    score_map,
    map
  )
  out <- dplyr::select(wide, dplyr::all_of(keep))
  dplyr::mutate(out, wave = wave, .after = "participant_id")
}

#' Make item labels wave-neutral and drop the stale screen-time value labels
bp535_relabel_items <- function(df) {
  items <- grep("^q[0-9]{3}", names(df), value = TRUE)
  who <- c(C = "Child", d = "Child", P = "Parent", T = "Teacher")
  df[items] <- lapply(df[items], function(x) {
    lab <- attr(x, "label")
    if (grepl("^[CdPT][0-9][-:]", lab)) {
      attr(x, "label") <- paste0(
        who[[substring(lab, 1L, 1L)]],
        ": ",
        substring(lab, 4L)
      )
    }
    x
  })

  # These carry a band scheme (70 = "6 1/2 hours") the data no longer use: the
  # study's own weekly totals are the daypart sum over ten, so they are tenths.
  hours <- grep("^q(02[3-8]|14[0-5])\\.[123]$", names(df), value = TRUE)
  df[hours] <- lapply(df[hours], function(x) {
    lab <- attr(x, "label")
    x <- haven::zap_labels(x)
    attr(x, "label") <- paste0(lab, " (tenths of an hour)")
    x
  })

  df
}

#' Give bound columns that lost their label the first wave's label
bp535_restore_labels <- function(long, tables) {
  for (col in names(long)) {
    if (is.null(attr(long[[col]], "label"))) {
      labels <- lapply(tables, function(t) attr(t[[col]], "label"))
      labels <- Filter(function(l) length(l) == 1L && nzchar(l), labels)
      if (length(labels) > 0L) {
        attr(long[[col]], "label") <- labels[[1L]]
      }
    }
  }
  long
}

#' Fill a participant's missing values from their nearest wave `max_gap` months away
bp535_carry_gap <- function(x, id, month, max_gap = 12) {
  out <- x
  for (i in which(is.na(x) & !is.na(month))) {
    gap <- abs(month - month[i])
    candidates <- which(id == id[i] & !is.na(x) & gap <= max_gap)
    if (length(candidates) > 0L) {
      out[i] <- x[candidates[which.min(gap[candidates])]]
    }
  }
  out
}

#' Fill wave-2 age from the nearest measured age and the days between the dates
bp535_fill_age <- function(long, date) {
  measured <- which(!is.na(long$Age_yrs) & !is.na(long$ObsDate))
  for (i in which(long$wave == "2" & is.na(long$Age_yrs) & !is.na(date))) {
    own <- measured[long$participant_id[measured] == long$participant_id[i]]
    if (length(own) > 0L) {
      days <- as.numeric(date[i] - long$ObsDate[own])
      k <- which.min(abs(days))
      long$Age_yrs[i] <- long$Age_yrs[own[k]] + days[k] / 365.25
    }
  }
  attr(long$Age_yrs, "label") <- paste(
    "Age in years at measurement (ageW1, Age_yrs_W3, Age_W4); wave 2 is the",
    "nearest measured age plus the days from its ObsDate to the wave-2 Date"
  )
  long
}
