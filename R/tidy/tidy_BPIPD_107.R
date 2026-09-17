#' Tidier for BPIPD-107 (PISA)
#'
#' PISA releases one student questionnaire file and one school questionnaire
#' file per cycle, plus, in some cycles, separate files for additional samples
#' (financial literacy, Moscow City, the 2015 additional countries) and for
#' scores released later (2018 Viet Nam, 2022 creative thinking). The spec
#' declares each cycle as a wave. This tidier assembles each cycle from its
#' files following `bp107_cycle_recipes`, joins the school columns on, keeps the
#' columns listed in `bp107_column_map()`, binds the cycles into one table, and
#' builds a participant id that is unique across cycles. Columns keep their
#' original PISA names, except where the same name carries a different coding
#' in different cycles (the ISCED parental-education indices); recoding is the
#' harmonisation step's job.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble with one row per student per cycle; `pisa_source` names the
#'   spec resource each student row came from
tidy_BPIPD_107 <- function(raw_dataset, spec) {
  waves <- vapply(spec$waves, function(w) as.character(w$wave), character(1))
  parts <- lapply(waves, function(wave) bp107_tidy_wave(raw_dataset, wave))
  names(parts) <- waves

  bp107_check_label_conflicts(parts)
  df <- bp107_bind_waves(parts)

  # `bind_rows()` appends each later cycle's new columns at the end; restore
  # the column map's order.
  ordered <- intersect(bp107_column_map()$column, names(df))
  df <- df[c(ordered, "pisa_source", ".wave", ".wave_label")]

  df <- dplyr::mutate(
    df,
    participant_id = paste0(.wave, "_", sprintf("%.0f", as.numeric(CNTSTUID))),
    .before = 1
  )
  if (anyDuplicated(df$participant_id) > 0) {
    stop(
      "BPIPD-107: `.wave` and `CNTSTUID` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' How each cycle's files assemble into one student table
#'
#' - `spine`: student tables whose rows are participants; a cycle with
#'   additional samples lists several, and they stack.
#' - `replace`: for a spine table, a table that re-issues some of its rows
#'   with more columns filled in (2018 Viet Nam, whose test scores were
#'   released separately); matched rows are overwritten.
#' - `exclude`: countries dropped from a table, keyed by resource name. The
#'   only one is Albania, the 2015 additional file that is
#'   also in the main 2015 file
#' - `join`: groups of tables that add columns to existing participants,
#'   keyed on `CNTSTUID`; the tables in a group stack before joining.
#' - `school`: school questionnaire tables, stacked and joined on
#'   `CNT` + `CNTSCHID`.
bp107_cycle_recipes <- list(
  "2015" = list(
    spine = c("stu_2015", "stu_cm2_2015"),
    exclude = list(
      stu_cm2_2015 = "ALB",
      stu_qq2_cm2_2015 = "ALB",
      sch_cm2_2015 = "ALB"
    ),
    join = list(c("stu_qq2_2015", "stu_qq2_cm2_2015"), "stu_flt_2015"),
    school = c("sch_2015", "sch_cm2_2015")
  ),
  "2018" = list(
    spine = c("stu_2018", "stu_flt_2018", "stu_qmc_2018", "stu_flt_qmc_2018"),
    replace = list(stu_2018 = "stu_vnm_2018"),
    school = c("sch_2018", "sch_qmc_2018")
  ),
  "2022" = list(
    spine = c("stu_2022", "stu_flt_2022"),
    join = list("crt_2022"),
    school = "sch_2022"
  ),
  "2025" = list(
    spine = "stu_2025",
    school = "sch_2025"
  )
)

#' One cycle's students, assembled from its files per `bp107_cycle_recipes`
#'
#' Every table a recipe names must have been read, so a file that went
#' missing from the data folder is an error rather than a silently smaller
#' sample. The assembled table is then cut down to the column map.
bp107_tidy_wave <- function(raw_dataset, wave) {
  recipe <- bp107_cycle_recipes[[wave]]
  if (is.null(recipe)) {
    stop(
      "BPIPD-107: no assembly recipe for cycle ",
      wave,
      "; add one to `bp107_cycle_recipes`.",
      call. = FALSE
    )
  }

  fetch <- function(name) {
    tbl <- raw_dataset$data[[name]]
    if (is.null(tbl)) {
      stop(
        "BPIPD-107: cycle ",
        wave,
        " needs a data resource named `",
        name,
        "` and none was read.",
        call. = FALSE
      )
    }
    tbl <- tibble::as_tibble(tbl)
    out <- recipe$exclude[[name]]
    if (!is.null(out)) {
      cnt <- as.character(haven::zap_labels(tbl$CNT))
      tbl <- tbl[!cnt %in% out, , drop = FALSE]
    }
    tbl
  }

  spine <- lapply(recipe$spine, function(name) {
    tbl <- fetch(name)
    tbl$pisa_source <- name
    replacement <- recipe$replace[[name]]
    if (!is.null(replacement)) {
      tbl <- bp107_replace_rows(tbl, fetch(replacement), what = replacement)
    }
    tbl
  })
  mapped <- stats::na.omit(bp107_column_map()[[wave]])
  stu <- bp107_stack(spine, key = "CNTSTUID", what = "student tables", mapped)

  for (group in recipe$join) {
    extra <- bp107_stack(lapply(group, fetch), "CNTSTUID", group, mapped)
    stu <- bp107_join(stu, extra, by = "CNTSTUID", what = group)
  }

  sch <- bp107_stack(
    lapply(recipe$school, fetch),
    key = c("CNT", "CNTSCHID"),
    what = "school tables",
    mapped
  )
  stu <- bp107_join(stu, sch, by = c("CNT", "CNTSCHID"), what = recipe$school)

  stu <- bp107_select_columns(stu, wave)
  bp107_zap_cycle_labels(bp107_align_id_types(stu))
}

#' Identifier columns that PISA 2025 stores as zero-padded strings where
#' earlier cycles store numbers ("00800001" versus 800001). They are made
#' numeric in every cycle so the cycles bind and the ids compare equal.
bp107_numeric_ids <- c("CNTRYID", "CNTSCHID", "CNTSTUID", "REGION")

bp107_align_id_types <- function(tbl) {
  cols <- intersect(bp107_numeric_ids, names(tbl))
  tbl[cols] <- lapply(tbl[cols], function(x) {
    label <- attr(x, "label")
    out <- as.numeric(haven::zap_labels(x))
    attr(out, "label") <- label
    out
  })
  tbl
}

#' Stack same-cycle tables and insist the key is unique across them
#'
#' Value labels of the mapped columns must agree between the tables (see
#' `bp107_check_label_conflicts()`); label differences in columns the map does
#' not keep are ignored.
bp107_stack <- function(tables, key, what, mapped) {
  if (length(tables) > 1) {
    bp107_check_label_conflicts(tables, columns = mapped)
  }
  out <- bp107_bind_waves(tables)
  if (anyDuplicated(out[key]) > 0) {
    stop(
      "BPIPD-107: ",
      paste(key, collapse = " + "),
      " is not unique across the stacked ",
      paste(what, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  out
}

#' Overwrite rows of `x` with the re-issued rows in `y`, matched on `CNTSTUID`
#'
#' `y` must only contain students already in `x` and only columns `x` has.
bp107_replace_rows <- function(x, y, what) {
  y <- tibble::as_tibble(y)
  extra <- setdiff(names(y), names(x))
  if (length(extra) > 0) {
    stop(
      "BPIPD-107: `",
      what,
      "` has columns the table it replaces rows in does not: ",
      paste(utils::head(extra, 5), collapse = ", "),
      call. = FALSE
    )
  }
  out <- dplyr::rows_update(x, y, by = "CNTSTUID", unmatched = "error")
  if (nrow(out) != nrow(x)) {
    stop(
      "BPIPD-107: replacing rows from `",
      what,
      "` changed the row count.",
      call. = FALSE
    )
  }
  out
}

#' Left-join a cycle table onto the student rows without changing the row count
#'
#' Columns of `y` that already exist in `x` (design and administrative columns
#' such as `STRATUM`, `OECD`, `.wave`) are dropped from `y` first, so the join
#' never produces `.x`/`.y` suffixes. `y` must have one row per key.
bp107_join <- function(x, y, by, what) {
  y <- tibble::as_tibble(y)
  missing_keys <- setdiff(by, names(y))
  if (length(missing_keys) > 0) {
    stop(
      "BPIPD-107: `",
      paste(what, collapse = ", "),
      "` lacks join key(s): ",
      paste(missing_keys, collapse = ", "),
      call. = FALSE
    )
  }
  y <- y[, c(by, setdiff(names(y), names(x))), drop = FALSE]

  out <- dplyr::left_join(x, y, by = by, relationship = "many-to-one")
  if (nrow(out) != nrow(x)) {
    stop(
      "BPIPD-107: joining `",
      paste(what, collapse = ", "),
      "` changed the row count.",
      call. = FALSE
    )
  }
  out
}

# Column map ------------------------------------------------------------------

#' Cycles the column map describes
bp107_cycles <- c("2015", "2018", "2022", "2025")

#' One column-map entry
#'
#' `cycles` lists the cycles whose files carry the item under the tidied name.
#' `source` is only needed when the tidied name differs from the file's column
#' name: a character vector of source columns named by cycle, which then also
#' defines the cycles.
bp107_entry <- function(label, cycles = bp107_cycles, source = NULL) {
  list(label = label, cycles = cycles, source = source)
}

#' The ten plausible values of one domain
bp107_pv_entries <- function(suffix, domain, cycles = bp107_cycles) {
  entries <- lapply(1:10, function(i) {
    bp107_entry(paste0("Plausible value ", i, " in ", domain), cycles)
  })
  names(entries) <- paste0("PV", 1:10, suffix)
  entries
}

#' Columns carried into the tidied table, with a label and the cycles that
#' have them
#'
#' Names are the original PISA variable names. Where a cycle lacks an item the
#' tidied column is `NA` for that cycle. Add here anything the harmonisation
#' step turns out to need; `bp107_select_columns()` errors if a listed column is
#' missing from a cycle's files, so renamed or dropped items surface at once.
bp107_column_map <- function() {
  entries <- c(
    list(
      # --- identifiers and survey design -----------------------------------
      CNT = bp107_entry("Country or economy code (3 characters)"),
      CNTRYID = bp107_entry("Country or economy numeric id"),
      CNTSCHID = bp107_entry("International school id"),
      CNTSTUID = bp107_entry("International student id"),
      OECD = bp107_entry("OECD member country"),
      STRATUM = bp107_entry("Explicit sampling stratum (cycle-specific codes)"),
      SUBNATIO = bp107_entry("Adjudicated sub-national entity"),
      REGION = bp107_entry("Region", c("2022", "2025")),
      W_FSTUWT = bp107_entry("Final trimmed student weight"),
      Option_ICTQ = bp107_entry(
        "Country ran the ICT familiarity questionnaire",
        c("2015", "2022", "2025")
      ),
      Option_WBQ = bp107_entry(
        "Country ran the well-being questionnaire",
        "2022"
      ),
      Option_FL = bp107_entry(
        "Country ran the financial literacy assessment",
        c("2015", "2022")
      ),

      # --- demographics and background ---------------------------------------
      ST001D01T = bp107_entry("International grade"),
      ST003D02T = bp107_entry("Birth month"),
      ST003D03T = bp107_entry("Birth year"),
      ST004D01T = bp107_entry("Gender (1 = female, 2 = male)"),
      AGE = bp107_entry("Age in years at testing (derived)"),
      GRADE = bp107_entry("Grade relative to the country's modal grade"),
      IMMIG = bp107_entry("Immigrant background index"),
      ST019AQ01T = bp107_entry(
        "Born in the country of test",
        c("2015", "2018", "2022")
      ),
      ST022Q01TA = bp107_entry(
        "Language spoken at home is the language of the test",
        c("2015", "2018", "2022", "2025")
      ),
      ESCS = bp107_entry("Index of economic, social and cultural status"),
      HISEI = bp107_entry(
        "Highest parental occupational status (ISEI)",
        c("2018", "2022", "2025")
      ),
      HOMEPOS = bp107_entry("Home possessions index"),
      ICTRES = bp107_entry("ICT resources at home index"),
      PARED = bp107_entry(
        "Highest parental education in years",
        c("2015", "2018")
      ),
      PAREDINT = bp107_entry(
        "Highest parental education in years (international)",
        c("2018", "2022", "2025")
      ),
      REPEAT = bp107_entry("Ever repeated a grade"),
      HISCED_isced97 = bp107_entry(
        "Highest parental education, ISCED-97 levels 0-6",
        source = c("2015" = "HISCED", "2018" = "HISCED")
      ),
      HISCED_isced11 = bp107_entry(
        "Highest parental education, ISCED-2011 levels",
        source = c("2022" = "HISCED", "2025" = "HISCED")
      ),
      MISCED_isced97 = bp107_entry(
        "Mother's education, ISCED-97 levels 0-6",
        source = c("2015" = "MISCED", "2018" = "MISCED")
      ),
      MISCED_isced11 = bp107_entry(
        "Mother's education, ISCED-2011 levels",
        source = c("2022" = "MISCED")
      ),
      FISCED_isced97 = bp107_entry(
        "Father's education, ISCED-97 levels 0-6",
        source = c("2015" = "FISCED", "2018" = "FISCED")
      ),
      FISCED_isced11 = bp107_entry(
        "Father's education, ISCED-2011 levels",
        source = c("2022" = "FISCED")
      ),

      # --- screen use, 2015 and 2018 ICT questionnaire -----------------------
      IC005Q01TA = bp107_entry(
        "Internet use at school on a typical weekday (7 duration bands)",
        c("2015", "2018")
      ),
      IC006Q01TA = bp107_entry(
        "Internet use outside school on a typical weekday (7 duration bands)",
        c("2015", "2018")
      ),
      IC007Q01TA = bp107_entry(
        "Internet use outside school on a typical weekend day (7 duration bands)",
        c("2015", "2018")
      ),
      IC008Q01TA = bp107_entry(
        "Outside school, frequency: playing one-player games",
        c("2015", "2018")
      ),
      IC008Q02TA = bp107_entry(
        "Outside school, frequency: playing collaborative online games",
        c("2015", "2018")
      ),
      IC008Q03TA = bp107_entry(
        "Outside school, frequency: using email",
        c("2015", "2018")
      ),
      IC008Q04TA = bp107_entry(
        "Outside school, frequency: chatting online",
        c("2015", "2018")
      ),
      IC008Q05TA = bp107_entry(
        "Outside school, frequency: participating in social networks",
        c("2015", "2018")
      ),
      IC008Q07NA = bp107_entry(
        "Outside school, frequency: playing online games via social networks",
        c("2015", "2018")
      ),
      IC008Q08TA = bp107_entry(
        "Outside school, frequency: browsing the Internet for fun",
        c("2015", "2018")
      ),
      IC008Q09TA = bp107_entry(
        "Outside school, frequency: reading news on the Internet",
        c("2015", "2018")
      ),
      IC008Q10TA = bp107_entry(
        "Outside school, frequency: obtaining practical information online",
        c("2015", "2018")
      ),
      IC008Q11TA = bp107_entry(
        "Outside school, frequency: downloading music, films, games or software",
        c("2015", "2018")
      ),
      IC008Q12TA = bp107_entry(
        "Outside school, frequency: uploading own content for sharing",
        c("2015", "2018")
      ),
      IC008Q13NA = bp107_entry(
        "Outside school, frequency: downloading new apps on a mobile device",
        c("2015", "2018")
      ),
      ENTUSE = bp107_entry(
        "ICT use outside school for leisure index",
        c("2015", "2018")
      ),
      HOMESCH = bp107_entry(
        "ICT use outside school for schoolwork index",
        c("2015", "2018")
      ),
      USESCH = bp107_entry("ICT use at school index", c("2015", "2018")),
      IC150Q01HA = bp107_entry(
        "Digital device time in lessons per week: test language",
        "2018"
      ),
      IC150Q02HA = bp107_entry(
        "Digital device time in lessons per week: mathematics",
        "2018"
      ),
      IC150Q03HA = bp107_entry(
        "Digital device time in lessons per week: science",
        "2018"
      ),
      IC150Q04HA = bp107_entry(
        "Digital device time in lessons per week: foreign language",
        "2018"
      ),
      IC150Q05HA = bp107_entry(
        "Digital device time in lessons per week: social sciences",
        "2018"
      ),
      IC150Q06HA = bp107_entry(
        "Digital device time in lessons per week: music",
        "2018"
      ),
      IC150Q07HA = bp107_entry(
        "Digital device time in lessons per week: sports",
        "2018"
      ),
      IC150Q08HA = bp107_entry(
        "Digital device time in lessons per week: performing arts",
        "2018"
      ),
      IC150Q09HA = bp107_entry(
        "Digital device time in lessons per week: visual arts",
        "2018"
      ),

      # --- screen use, 2022 and 2025 ------------------------------------------
      ST326Q01JA = bp107_entry(
        "Hours per day on digital resources: learning at school",
        c("2022", "2025")
      ),
      ST326Q02JA = bp107_entry(
        "Hours per day on digital resources: learning before and after school",
        c("2022", "2025")
      ),
      ST326Q03JA = bp107_entry(
        "Hours per day on digital resources: learning on weekends",
        c("2022", "2025")
      ),
      ST326Q04JA = bp107_entry(
        "Hours per day on digital resources: leisure at school",
        c("2022", "2025")
      ),
      ST326Q05JA = bp107_entry(
        "Hours per day on digital resources: leisure before and after school",
        c("2022", "2025")
      ),
      ST326Q06JA = bp107_entry(
        "Hours per day on digital resources: leisure on weekends",
        c("2022", "2025")
      ),
      IC177Q01JA = bp107_entry(
        "Typical weekday, time: playing video games",
        c("2022", "2025")
      ),
      IC177Q02JA = bp107_entry(
        "Typical weekday, time: browsing social networks",
        c("2022", "2025")
      ),
      IC177Q03JA = bp107_entry(
        "Typical weekday, time: browsing the Internet for fun",
        c("2022", "2025")
      ),
      IC177Q04JA = bp107_entry(
        "Typical weekday, time: looking for practical information online",
        c("2022", "2025")
      ),
      IC177Q05JA = bp107_entry(
        "Typical weekday, time: communicating and sharing on social networks",
        c("2022", "2025")
      ),
      IC177Q06JA = bp107_entry(
        "Typical weekday, time: informational material to learn how to do something",
        c("2022", "2025")
      ),
      IC177Q07JA = bp107_entry(
        "Typical weekday, time: creating or editing own digital content",
        c("2022", "2025")
      ),
      IC178Q01JA = bp107_entry(
        "Typical weekend day, time: playing video games",
        c("2022", "2025")
      ),
      IC178Q02JA = bp107_entry(
        "Typical weekend day, time: browsing social networks",
        c("2022", "2025")
      ),
      IC178Q03JA = bp107_entry(
        "Typical weekend day, time: browsing the Internet for fun",
        c("2022", "2025")
      ),
      IC178Q04JA = bp107_entry(
        "Typical weekend day, time: looking for practical information online",
        c("2022", "2025")
      ),
      IC178Q05JA = bp107_entry(
        "Typical weekend day, time: communicating and sharing on social networks",
        c("2022", "2025")
      ),
      IC178Q06JA = bp107_entry(
        "Typical weekend day, time: informational material to learn how to do something",
        c("2022", "2025")
      ),
      IC178Q07JA = bp107_entry(
        "Typical weekend day, time: creating or editing own digital content",
        c("2022", "2025")
      ),
      ICTWKDY = bp107_entry(
        "Time on digital devices on weekdays index",
        c("2022", "2025")
      ),
      ICTWKEND = bp107_entry(
        "Time on digital devices on weekend days index",
        c("2022", "2025")
      ),
      ST322Q01JA = bp107_entry(
        "Turns off notifications during class",
        c("2022", "2025")
      ),
      ST322Q02JA = bp107_entry(
        "Turns off notifications when going to sleep",
        c("2022", "2025")
      ),
      ST322Q03JA = bp107_entry(
        "Keeps digital device near to answer messages when at home",
        "2022"
      ),
      ST322Q04JA = bp107_entry(
        "Has digital device open in class to take notes or search",
        "2022"
      ),
      ST322Q06JA = bp107_entry(
        "Feels pressured to be online and answer messages in class",
        c("2022", "2025")
      ),
      ST322Q07JA = bp107_entry(
        "Feels nervous or anxious without the digital device nearby",
        c("2022", "2025")
      ),
      ST250Q04JA = bp107_entry(
        "Has own cell phone with Internet access",
        c("2022", "2025")
      ),
      ST253Q01JA = bp107_entry(
        "Number of digital devices with screens at home",
        c("2022", "2025")
      ),
      ST254Q01JA = bp107_entry(
        "Number at home: televisions",
        c("2022", "2025")
      ),
      ST254Q02JA = bp107_entry(
        "Number at home: desktop computers",
        c("2022", "2025")
      ),
      ST254Q03JA = bp107_entry(
        "Number at home: laptop computers or notebooks",
        c("2022", "2025")
      ),
      ST254Q04JA = bp107_entry(
        "Number at home: tablets",
        c("2022", "2025")
      ),
      ST254Q05JA = bp107_entry(
        "Number at home: e-book readers",
        c("2022", "2025")
      ),
      ST254Q06JA = bp107_entry(
        "Number at home: cell phones with Internet access",
        "2022"
      )
    ),

    # --- achievement -----------------------------------------------------------
    bp107_pv_entries("MATH", "mathematics"),
    bp107_pv_entries("READ", "reading"),
    bp107_pv_entries("SCIE", "science"),
    bp107_pv_entries("FLIT", "financial literacy", c("2015", "2018", "2022")),
    bp107_pv_entries("CRTH_NC", "creative thinking (number correct)", "2022"),

    list(
      # --- well-being, mental health, bullying, school ------------------------
      ST016Q01NA = bp107_entry("Overall life satisfaction (0-10)"),
      BELONG = bp107_entry("Sense of belonging to school index"),
      ST034Q01TA = bp107_entry(
        "Belonging: I feel like an outsider at school",
        c("2015", "2018", "2022", "2025")
      ),
      ST034Q02TA = bp107_entry(
        "Belonging: I make friends easily at school",
        c("2015", "2018", "2022", "2025")
      ),
      ST034Q03TA = bp107_entry("Belonging: I feel like I belong at school"),
      ST034Q04TA = bp107_entry(
        "Belonging: I feel awkward and out of place at school",
        c("2015", "2018", "2022", "2025")
      ),
      ST034Q05TA = bp107_entry(
        "Belonging: other students seem to like me",
        c("2015", "2018", "2022", "2025")
      ),
      ST034Q06TA = bp107_entry("Belonging: I feel lonely at school"),
      ST038Q01NA = bp107_entry(
        "Bullied in past 12 months: called names by other students",
        "2015"
      ),
      ST038Q02NA = bp107_entry(
        "Bullied in past 12 months: picked on by other students",
        "2015"
      ),
      ST038Q03NA = bp107_entry(
        "Bullied in past 12 months: left out of things on purpose",
        c("2015", "2018", "2022")
      ),
      ST038Q04NA = bp107_entry(
        "Bullied in past 12 months: made fun of by other students"
      ),
      ST038Q05NA = bp107_entry(
        "Bullied in past 12 months: threatened by other students"
      ),
      ST038Q06NA = bp107_entry(
        "Bullied in past 12 months: things taken away or destroyed",
        c("2015", "2018", "2022")
      ),
      ST038Q07NA = bp107_entry(
        "Bullied in past 12 months: hit or pushed around by other students"
      ),
      ST038Q08NA = bp107_entry(
        "Bullied in past 12 months: nasty rumours spread by other students"
      ),
      ST038Q09JA = bp107_entry(
        "Past 12 months: was in a physical fight on school property",
        "2022"
      ),
      ST038Q10JA = bp107_entry(
        "Past 12 months: stayed home from school because felt unsafe",
        "2022"
      ),
      ST038Q11JA = bp107_entry(
        "Past 12 months: gave money to someone at school who threatened me",
        "2022"
      ),
      ST038Q12DA = bp107_entry(
        "Past 12 months: upsetting information about me published online without consent",
        "2025"
      ),
      BEINGBULLIED = bp107_entry("Exposure to bullying index", "2018"),
      BULLIED = bp107_entry("Exposure to bullying index", c("2022", "2025")),
      SWBP = bp107_entry(
        "Subjective well-being: positive affect index",
        "2018"
      ),
      EUDMO = bp107_entry("Eudaemonia: meaning in life index", "2018"),
      RESILIENCE = bp107_entry("Resilience index", "2018"),
      ST185Q01HA = bp107_entry(
        "Meaning in life: my life has clear meaning",
        "2018"
      ),
      ST185Q02HA = bp107_entry(
        "Meaning in life: I have discovered a satisfactory meaning",
        "2018"
      ),
      ST185Q03HA = bp107_entry(
        "Meaning in life: I have a clear sense of what gives meaning",
        "2018"
      ),
      ST186Q01HA = bp107_entry("How often feels: joyful", "2018"),
      ST186Q02HA = bp107_entry("How often feels: afraid", "2018"),
      ST186Q03HA = bp107_entry("How often feels: cheerful", "2018"),
      ST186Q05HA = bp107_entry("How often feels: happy", "2018"),
      ST186Q06HA = bp107_entry("How often feels: scared", "2018"),
      ST186Q07HA = bp107_entry("How often feels: lively", "2018"),
      ST186Q08HA = bp107_entry("How often feels: sad", "2018"),
      ST186Q09HA = bp107_entry("How often feels: proud", "2018"),
      ST186Q10HA = bp107_entry("How often feels: miserable", "2018"),
      ST188Q01HA = bp107_entry(
        "Resilience: I usually manage one way or another",
        "2018"
      ),
      ST188Q02HA = bp107_entry(
        "Resilience: I feel proud that I have accomplished things",
        "2018"
      ),
      ST188Q03HA = bp107_entry(
        "Resilience: I feel that I can handle many things at a time",
        "2018"
      ),
      ST188Q06HA = bp107_entry(
        "Resilience: my belief in myself gets me through hard times",
        "2018"
      ),
      ST188Q07HA = bp107_entry(
        "Resilience: in a difficult situation I can usually find my way out",
        "2018"
      ),
      WB150Q01HA = bp107_entry("Self-rated health", c("2018", "2022")),
      WB153Q01HA = bp107_entry(
        "Body image: I like my look just the way it is",
        c("2018", "2022")
      ),
      WB153Q02HA = bp107_entry(
        "Body image: I consider myself to be attractive",
        c("2018", "2022")
      ),
      WB153Q03HA = bp107_entry(
        "Body image: I am not concerned about my weight",
        c("2018", "2022")
      ),
      WB153Q04HA = bp107_entry(
        "Body image: I like my body",
        c("2018", "2022")
      ),
      WB153Q05HA = bp107_entry(
        "Body image: I like the way my clothes fit me",
        c("2018", "2022")
      ),
      BODYIMA = bp107_entry("Body image index", c("2018", "2022")),
      WB154Q01HA = bp107_entry(
        "Past six months, how often: headache",
        c("2018", "2022")
      ),
      WB154Q02HA = bp107_entry(
        "Past six months, how often: stomach pain",
        c("2018", "2022")
      ),
      WB154Q03HA = bp107_entry(
        "Past six months, how often: back pain",
        c("2018", "2022")
      ),
      WB154Q04HA = bp107_entry(
        "Past six months, how often: feeling depressed",
        c("2018", "2022")
      ),
      WB154Q05HA = bp107_entry(
        "Past six months, how often: irritability or bad temper",
        c("2018", "2022")
      ),
      WB154Q06HA = bp107_entry(
        "Past six months, how often: feeling nervous",
        c("2018", "2022")
      ),
      WB154Q07HA = bp107_entry(
        "Past six months, how often: difficulties getting to sleep",
        c("2018", "2022")
      ),
      WB154Q08HA = bp107_entry(
        "Past six months, how often: feeling dizzy",
        c("2018", "2022")
      ),
      WB154Q09HA = bp107_entry(
        "Past six months, how often: feeling anxious",
        c("2018", "2022")
      ),
      PSYCHSYM = bp107_entry("Psychosomatic symptoms index", "2022"),
      WB155Q01HA = bp107_entry(
        "Satisfied with: your health",
        c("2018", "2022")
      ),
      WB155Q02HA = bp107_entry(
        "Satisfied with: the way you look",
        c("2018", "2022")
      ),
      WB155Q03HA = bp107_entry(
        "Satisfied with: what you learn at school",
        c("2018", "2022")
      ),
      WB155Q04HA = bp107_entry(
        "Satisfied with: your friends",
        c("2018", "2022")
      ),
      WB155Q05HA = bp107_entry(
        "Satisfied with: the neighbourhood you live in",
        c("2018", "2022")
      ),
      WB155Q06HA = bp107_entry(
        "Satisfied with: all the things you have",
        c("2018", "2022")
      ),
      WB155Q07HA = bp107_entry(
        "Satisfied with: how you use your time",
        c("2018", "2022")
      ),
      WB155Q08HA = bp107_entry(
        "Satisfied with: your relationship with your parents",
        c("2018", "2022")
      ),
      WB155Q09HA = bp107_entry(
        "Satisfied with: your relationship with your teachers",
        c("2018", "2022")
      ),
      WB155Q10HA = bp107_entry(
        "Satisfied with: your life at school",
        c("2018", "2022")
      ),
      LIFESAT = bp107_entry("Life satisfaction across domains index", "2022"),
      WB156Q01HA = bp107_entry("Number of close friends", c("2018", "2022")),
      WB158Q01HA = bp107_entry(
        "Days per week spent with friends right after school",
        c("2018", "2022")
      ),
      WB160Q01HA = bp107_entry(
        "How often contacts friends by phone, text or social media",
        c("2018", "2022")
      ),
      WB164Q01HA = bp107_entry(
        "How often worries about the family's money",
        c("2018", "2022")
      ),
      SOCONPA = bp107_entry(
        "Social connection to parents index",
        c("2018", "2022")
      ),
      ANXMAT = bp107_entry("Mathematics anxiety index", "2022"),
      ST062Q01TA = bp107_entry(
        "Last two weeks: skipped a whole school day",
        c("2015", "2018", "2022", "2025")
      ),
      ST062Q02TA = bp107_entry(
        "Last two weeks: skipped some classes",
        c("2015", "2018", "2022", "2025")
      ),
      ST062Q03TA = bp107_entry(
        "Last two weeks: arrived late for school",
        c("2015", "2018", "2022", "2025")
      ),
      ST296Q04JA = bp107_entry(
        "Time spent on homework per day, all subjects",
        c("2022", "2025")
      ),

      # --- school questionnaire ---------------------------------------------
      SC001Q01TA = bp107_entry("School community size (village to megacity)"),
      SC013Q01TA = bp107_entry("Public or private school"),
      SC002Q01TA = bp107_entry(
        "Total enrolment: boys",
        source = c(
          "2015" = "SC002Q01TA",
          "2018" = "SC002Q01TA",
          "2022" = "SC002Q01TA",
          "2025" = "SC002Q01TA_P"
        )
      ),
      SC002Q02TA = bp107_entry(
        "Total enrolment: girls",
        source = c(
          "2015" = "SC002Q02TA",
          "2018" = "SC002Q02TA",
          "2022" = "SC002Q02TA",
          "2025" = "SC002Q02TA_P"
        )
      ),
      SCHSIZE = bp107_entry(
        "School size (total enrolment)",
        source = c(
          "2015" = "SCHSIZE",
          "2018" = "SCHSIZE",
          "2022" = "SCHSIZE",
          "2025" = "SCHSIZE_Q"
        )
      ),
      STRATIO = bp107_entry(
        "Student-teacher ratio",
        source = c(
          "2015" = "STRATIO",
          "2018" = "STRATIO",
          "2022" = "STRATIO",
          "2025" = "STRATIO_Q"
        )
      )
    )
  )

  sources <- lapply(bp107_cycles, function(cycle) {
    vapply(
      names(entries),
      function(column) {
        entry <- entries[[column]]
        if (!is.null(entry$source)) {
          unname(entry$source[cycle])
        } else if (cycle %in% entry$cycles) {
          column
        } else {
          NA_character_
        }
      },
      character(1)
    )
  })
  names(sources) <- bp107_cycles

  tibble::as_tibble(c(
    list(
      column = names(entries),
      label = vapply(entries, `[[`, character(1), "label")
    ),
    sources
  ))
}

#' Cut a cycle's joined table down to the column map
#'
#' Errors if the cycle is unknown to the map or if any column the map expects
#' for that cycle is absent, so a renamed or dropped item is noticed rather
#' than silently becoming `NA`.
bp107_select_columns <- function(tbl, wave) {
  map <- bp107_column_map()
  if (!wave %in% names(map)) {
    stop(
      "BPIPD-107: cycle ",
      wave,
      " is not in `bp107_column_map()`; add it to `bp107_cycles` and review ",
      "every entry.",
      call. = FALSE
    )
  }
  sources <- map[[wave]]
  keep <- !is.na(sources)

  missing <- setdiff(sources[keep], names(tbl))
  if (length(missing) > 0) {
    stop(
      "BPIPD-107: the ",
      wave,
      " files lack columns `bp107_column_map()` expects: ",
      paste(missing, collapse = ", "),
      ". Fix the map entry (the item may be renamed or dropped in this cycle).",
      call. = FALSE
    )
  }

  out <- tbl[sources[keep]]
  names(out) <- map$column[keep]
  out$pisa_source <- tbl$pisa_source
  out$.wave <- tbl$.wave
  out$.wave_label <- tbl$.wave_label
  out
}

# Value-label safety across cycles --------------------------------------------

#' Columns whose codes are cycle-specific lists (sampling strata). The same
#' code names a different thing in each cycle, so their value labels are
#' dropped before binding; the codes and the variable label stay, and the
#' cycle's codebook (or the raw target) gives the meaning within a cycle.
bp107_cycle_specific_codes <- c("STRATUM")

bp107_zap_cycle_labels <- function(tbl) {
  cols <- intersect(bp107_cycle_specific_codes, names(tbl))
  tbl[cols] <- lapply(tbl[cols], haven::zap_labels)
  tbl
}

#' Columns whose value labels are worded differently across cycles but whose
#' codes mean the same thing (for example "Turkey" / "Türkiye", "Native" /
#' "Native student"). Checked by hand against the 2015, 2018 and 2022 files;
#' extend after inspecting the conflicts `bp107_check_label_conflicts()`
#' reports for a new cycle.
bp107_label_conflicts_ok <- c(
  "CNTRYID",
  "CNT",
  "SUBNATIO",
  "ST022Q01TA",
  "ST062Q01TA",
  "ST062Q02TA",
  "ST062Q03TA",
  "IMMIG",
  "REPEAT",
  "SC013Q01TA"
)

#' Stop if a shared column's value labels disagree between cycles
#'
#' `dplyr::bind_rows()` unions the value labels of `haven_labelled` columns
#' and, where one code carries different labels in two cycles, keeps the first
#' cycle's label with only a warning. That would mislabel a recoded item. This
#' check compares labels for the same code across cycles (case, spacing and
#' punctuation ignored), skipping PISA's missing-value codes (5-9, 95-99,
#' 995-999, ...), and errors unless the column is listed in
#' `bp107_label_conflicts_ok`.
bp107_check_label_conflicts <- function(parts, columns = NULL) {
  labels_of <- function(tbl) {
    cols <- names(tbl)[vapply(tbl, haven::is.labelled, logical(1))]
    if (!is.null(columns)) {
      cols <- intersect(cols, columns)
    }
    out <- lapply(cols, function(col) {
      labs <- attr(tbl[[col]], "labels")
      labs <- labs[!bp107_is_missing_code(labs)]
      names(labs) <- tolower(gsub("[^[:alnum:]]+", "", names(labs)))
      labs
    })
    names(out) <- cols
    out
  }
  wave_labels <- lapply(parts, labels_of)

  shared <- Reduce(intersect, lapply(wave_labels, names))
  conflicts <- vapply(
    shared,
    function(col) {
      per_wave <- lapply(wave_labels, function(wl) wl[[col]])
      merged <- unlist(unname(lapply(per_wave, function(labs) {
        stats::setNames(names(labs), as.character(labs))
      })))
      any(tapply(merged, names(merged), function(x) length(unique(x))) > 1)
    },
    logical(1)
  )
  bad <- setdiff(shared[conflicts], bp107_label_conflicts_ok)
  if (length(bad) > 0) {
    stop(
      "BPIPD-107: value labels for the same code differ between cycles in: ",
      paste(bad, collapse = ", "),
      ". Split the column per coding in `bp107_column_map()` (as for HISCED) ",
      "or, if only the wording differs, add it to `bp107_label_conflicts_ok`.",
      call. = FALSE
    )
  }
  invisible(shared[conflicts])
}

#' PISA missing-value codes: 5-9, 95-99, 995-999, 9995-9999, ...
bp107_is_missing_code <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  !is.na(x) & grepl("^9*[5-9]$", format(x, scientific = FALSE, trim = TRUE))
}

#' Bind tables, muffling only the label-wording warnings already reviewed
#' (or, within a cycle, in columns the map does not keep)
bp107_bind_waves <- function(parts) {
  withCallingHandlers(
    dplyr::bind_rows(parts),
    warning = function(w) {
      if (grepl("conflicting value\\s+labels", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
