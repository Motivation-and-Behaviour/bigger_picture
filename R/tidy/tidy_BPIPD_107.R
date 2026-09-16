#' Tidier for BPIPD-107 (PISA)
#'
#' PISA releases one student questionnaire file and one school questionnaire
#' file per cycle, plus, in some cycles, separate files for additional samples
#' (financial literacy, Moscow City, the 2015 additional countries) and for
#' scores released later (2018 Viet Nam, 2022 creative thinking). The spec
#' declares each cycle as a wave. This tidier assembles each cycle from its
#' files following `pisa_cycle_recipes`, joins the school columns on, keeps the
#' columns listed in `pisa_column_map()`, binds the cycles into one table, and
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
  parts <- lapply(waves, function(wave) pisa_tidy_wave(raw_dataset, wave))
  names(parts) <- waves

  pisa_check_label_conflicts(parts)
  df <- pisa_bind_waves(parts)

  # `bind_rows()` appends each later cycle's new columns at the end; restore
  # the column map's order.
  ordered <- intersect(pisa_column_map()$column, names(df))
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
#' - `exclude`: countries dropped from a table, keyed by resource name, with
#'   the reason in the spec notes (Albania's 2015 additional-file rows
#'   duplicate the main file under different ids).
#' - `join`: groups of tables that add columns to existing participants,
#'   keyed on `CNTSTUID`; the tables in a group stack before joining.
#' - `school`: school questionnaire tables, stacked and joined on
#'   `CNT` + `CNTSCHID`.
pisa_cycle_recipes <- list(
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

#' One cycle's students, assembled from its files per `pisa_cycle_recipes`
#'
#' Every table a recipe names must have been read, so a file that went
#' missing from the data folder is an error rather than a silently smaller
#' sample. The assembled table is then cut down to the column map.
pisa_tidy_wave <- function(raw_dataset, wave) {
  recipe <- pisa_cycle_recipes[[wave]]
  if (is.null(recipe)) {
    stop(
      "BPIPD-107: no assembly recipe for cycle ",
      wave,
      "; add one to `pisa_cycle_recipes`.",
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
      tbl <- pisa_replace_rows(tbl, fetch(replacement), what = replacement)
    }
    tbl
  })
  mapped <- stats::na.omit(pisa_column_map()[[wave]])
  stu <- pisa_stack(spine, key = "CNTSTUID", what = "student tables", mapped)

  for (group in recipe$join) {
    extra <- pisa_stack(lapply(group, fetch), "CNTSTUID", group, mapped)
    stu <- pisa_join(stu, extra, by = "CNTSTUID", what = group)
  }

  sch <- pisa_stack(
    lapply(recipe$school, fetch),
    key = c("CNT", "CNTSCHID"),
    what = "school tables",
    mapped
  )
  stu <- pisa_join(stu, sch, by = c("CNT", "CNTSCHID"), what = recipe$school)

  stu <- pisa_select_columns(stu, wave)
  pisa_zap_cycle_specific_labels(pisa_align_id_types(stu))
}

#' Identifier columns that PISA 2025 stores as zero-padded strings where
#' earlier cycles store numbers ("00800001" versus 800001). They are made
#' numeric in every cycle so the cycles bind and the ids compare equal.
pisa_numeric_ids <- c("CNTRYID", "CNTSCHID", "CNTSTUID", "REGION")

pisa_align_id_types <- function(tbl) {
  cols <- intersect(pisa_numeric_ids, names(tbl))
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
#' `pisa_check_label_conflicts()`); label differences in columns the map does
#' not keep are ignored.
pisa_stack <- function(tables, key, what, mapped) {
  if (length(tables) > 1) {
    pisa_check_label_conflicts(tables, columns = mapped)
  }
  out <- pisa_bind_waves(tables)
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
pisa_replace_rows <- function(x, y, what) {
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
pisa_join <- function(x, y, by, what) {
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
pisa_cycles <- c("2015", "2018", "2022", "2025")

#' One column-map entry
#'
#' `cycles` lists the cycles whose files carry the item under the tidied name.
#' `source` is only needed when the tidied name differs from the file's column
#' name: a character vector of source columns named by cycle, which then also
#' defines the cycles.
pisa_entry <- function(label, cycles = pisa_cycles, source = NULL) {
  list(label = label, cycles = cycles, source = source)
}

#' The ten plausible values of one domain
pisa_pv_entries <- function(suffix, domain, cycles = pisa_cycles) {
  entries <- lapply(1:10, function(i) {
    pisa_entry(paste0("Plausible value ", i, " in ", domain), cycles)
  })
  names(entries) <- paste0("PV", 1:10, suffix)
  entries
}

#' Columns carried into the tidied table, with a label and the cycles that
#' have them
#'
#' Names are the original PISA variable names. Where a cycle lacks an item the
#' tidied column is `NA` for that cycle. Add here anything the harmonisation
#' step turns out to need; `pisa_select_columns()` errors if a listed column is
#' missing from a cycle's files, so renamed or dropped items surface at once.
pisa_column_map <- function() {
  entries <- c(
    list(
      # --- identifiers and survey design -----------------------------------
      CNT = pisa_entry("Country or economy code (3 characters)"),
      CNTRYID = pisa_entry("Country or economy numeric id"),
      CNTSCHID = pisa_entry("International school id"),
      CNTSTUID = pisa_entry("International student id"),
      OECD = pisa_entry("OECD member country"),
      STRATUM = pisa_entry("Explicit sampling stratum (cycle-specific codes)"),
      SUBNATIO = pisa_entry("Adjudicated sub-national entity"),
      REGION = pisa_entry("Region", c("2022", "2025")),
      W_FSTUWT = pisa_entry("Final trimmed student weight"),
      Option_ICTQ = pisa_entry(
        "Country ran the ICT familiarity questionnaire",
        c("2015", "2022", "2025")
      ),
      Option_WBQ = pisa_entry(
        "Country ran the well-being questionnaire",
        "2022"
      ),
      Option_FL = pisa_entry(
        "Country ran the financial literacy assessment",
        c("2015", "2022")
      ),

      # --- demographics and background ---------------------------------------
      ST001D01T = pisa_entry("International grade"),
      ST003D02T = pisa_entry("Birth month"),
      ST003D03T = pisa_entry("Birth year"),
      ST004D01T = pisa_entry("Gender (1 = female, 2 = male)"),
      AGE = pisa_entry("Age in years at testing (derived)"),
      GRADE = pisa_entry("Grade relative to the country's modal grade"),
      IMMIG = pisa_entry("Immigrant background index"),
      ST019AQ01T = pisa_entry(
        "Born in the country of test",
        c("2015", "2018", "2022")
      ),
      ST022Q01TA = pisa_entry(
        "Language spoken at home is the language of the test",
        c("2015", "2018", "2022", "2025")
      ),
      ESCS = pisa_entry("Index of economic, social and cultural status"),
      HISEI = pisa_entry(
        "Highest parental occupational status (ISEI)",
        c("2018", "2022", "2025")
      ),
      HOMEPOS = pisa_entry("Home possessions index"),
      ICTRES = pisa_entry("ICT resources at home index"),
      PARED = pisa_entry(
        "Highest parental education in years",
        c("2015", "2018")
      ),
      PAREDINT = pisa_entry(
        "Highest parental education in years (international)",
        c("2018", "2022", "2025")
      ),
      REPEAT = pisa_entry("Ever repeated a grade"),
      HISCED_isced97 = pisa_entry(
        "Highest parental education, ISCED-97 levels 0-6",
        source = c("2015" = "HISCED", "2018" = "HISCED")
      ),
      HISCED_isced11 = pisa_entry(
        "Highest parental education, ISCED-2011 levels",
        source = c("2022" = "HISCED", "2025" = "HISCED")
      ),
      MISCED_isced97 = pisa_entry(
        "Mother's education, ISCED-97 levels 0-6",
        source = c("2015" = "MISCED", "2018" = "MISCED")
      ),
      MISCED_isced11 = pisa_entry(
        "Mother's education, ISCED-2011 levels",
        source = c("2022" = "MISCED")
      ),
      FISCED_isced97 = pisa_entry(
        "Father's education, ISCED-97 levels 0-6",
        source = c("2015" = "FISCED", "2018" = "FISCED")
      ),
      FISCED_isced11 = pisa_entry(
        "Father's education, ISCED-2011 levels",
        source = c("2022" = "FISCED")
      ),

      # --- screen use, 2015 and 2018 ICT questionnaire -----------------------
      IC005Q01TA = pisa_entry(
        "Internet use at school on a typical weekday (7 duration bands)",
        c("2015", "2018")
      ),
      IC006Q01TA = pisa_entry(
        "Internet use outside school on a typical weekday (7 duration bands)",
        c("2015", "2018")
      ),
      IC007Q01TA = pisa_entry(
        "Internet use outside school on a typical weekend day (7 duration bands)",
        c("2015", "2018")
      ),
      IC008Q01TA = pisa_entry(
        "Outside school, frequency: playing one-player games",
        c("2015", "2018")
      ),
      IC008Q02TA = pisa_entry(
        "Outside school, frequency: playing collaborative online games",
        c("2015", "2018")
      ),
      IC008Q03TA = pisa_entry(
        "Outside school, frequency: using email",
        c("2015", "2018")
      ),
      IC008Q04TA = pisa_entry(
        "Outside school, frequency: chatting online",
        c("2015", "2018")
      ),
      IC008Q05TA = pisa_entry(
        "Outside school, frequency: participating in social networks",
        c("2015", "2018")
      ),
      IC008Q07NA = pisa_entry(
        "Outside school, frequency: playing online games via social networks",
        c("2015", "2018")
      ),
      IC008Q08TA = pisa_entry(
        "Outside school, frequency: browsing the Internet for fun",
        c("2015", "2018")
      ),
      IC008Q09TA = pisa_entry(
        "Outside school, frequency: reading news on the Internet",
        c("2015", "2018")
      ),
      IC008Q10TA = pisa_entry(
        "Outside school, frequency: obtaining practical information online",
        c("2015", "2018")
      ),
      IC008Q11TA = pisa_entry(
        "Outside school, frequency: downloading music, films, games or software",
        c("2015", "2018")
      ),
      IC008Q12TA = pisa_entry(
        "Outside school, frequency: uploading own content for sharing",
        c("2015", "2018")
      ),
      IC008Q13NA = pisa_entry(
        "Outside school, frequency: downloading new apps on a mobile device",
        c("2015", "2018")
      ),
      ENTUSE = pisa_entry(
        "ICT use outside school for leisure index",
        c("2015", "2018")
      ),
      HOMESCH = pisa_entry(
        "ICT use outside school for schoolwork index",
        c("2015", "2018")
      ),
      USESCH = pisa_entry("ICT use at school index", c("2015", "2018")),
      IC150Q01HA = pisa_entry(
        "Digital device time in lessons per week: test language",
        "2018"
      ),
      IC150Q02HA = pisa_entry(
        "Digital device time in lessons per week: mathematics",
        "2018"
      ),
      IC150Q03HA = pisa_entry(
        "Digital device time in lessons per week: science",
        "2018"
      ),
      IC150Q04HA = pisa_entry(
        "Digital device time in lessons per week: foreign language",
        "2018"
      ),
      IC150Q05HA = pisa_entry(
        "Digital device time in lessons per week: social sciences",
        "2018"
      ),
      IC150Q06HA = pisa_entry(
        "Digital device time in lessons per week: music",
        "2018"
      ),
      IC150Q07HA = pisa_entry(
        "Digital device time in lessons per week: sports",
        "2018"
      ),
      IC150Q08HA = pisa_entry(
        "Digital device time in lessons per week: performing arts",
        "2018"
      ),
      IC150Q09HA = pisa_entry(
        "Digital device time in lessons per week: visual arts",
        "2018"
      ),

      # --- screen use, 2022 and 2025 ------------------------------------------
      ST326Q01JA = pisa_entry(
        "Hours per day on digital resources: learning at school",
        c("2022", "2025")
      ),
      ST326Q02JA = pisa_entry(
        "Hours per day on digital resources: learning before and after school",
        c("2022", "2025")
      ),
      ST326Q03JA = pisa_entry(
        "Hours per day on digital resources: learning on weekends",
        c("2022", "2025")
      ),
      ST326Q04JA = pisa_entry(
        "Hours per day on digital resources: leisure at school",
        c("2022", "2025")
      ),
      ST326Q05JA = pisa_entry(
        "Hours per day on digital resources: leisure before and after school",
        c("2022", "2025")
      ),
      ST326Q06JA = pisa_entry(
        "Hours per day on digital resources: leisure on weekends",
        c("2022", "2025")
      ),
      IC177Q01JA = pisa_entry(
        "Typical weekday, time: playing video games",
        c("2022", "2025")
      ),
      IC177Q02JA = pisa_entry(
        "Typical weekday, time: browsing social networks",
        c("2022", "2025")
      ),
      IC177Q03JA = pisa_entry(
        "Typical weekday, time: browsing the Internet for fun",
        c("2022", "2025")
      ),
      IC177Q04JA = pisa_entry(
        "Typical weekday, time: looking for practical information online",
        c("2022", "2025")
      ),
      IC177Q05JA = pisa_entry(
        "Typical weekday, time: communicating and sharing on social networks",
        c("2022", "2025")
      ),
      IC177Q06JA = pisa_entry(
        "Typical weekday, time: informational material to learn how to do something",
        c("2022", "2025")
      ),
      IC177Q07JA = pisa_entry(
        "Typical weekday, time: creating or editing own digital content",
        c("2022", "2025")
      ),
      IC178Q01JA = pisa_entry(
        "Typical weekend day, time: playing video games",
        c("2022", "2025")
      ),
      IC178Q02JA = pisa_entry(
        "Typical weekend day, time: browsing social networks",
        c("2022", "2025")
      ),
      IC178Q03JA = pisa_entry(
        "Typical weekend day, time: browsing the Internet for fun",
        c("2022", "2025")
      ),
      IC178Q04JA = pisa_entry(
        "Typical weekend day, time: looking for practical information online",
        c("2022", "2025")
      ),
      IC178Q05JA = pisa_entry(
        "Typical weekend day, time: communicating and sharing on social networks",
        c("2022", "2025")
      ),
      IC178Q06JA = pisa_entry(
        "Typical weekend day, time: informational material to learn how to do something",
        c("2022", "2025")
      ),
      IC178Q07JA = pisa_entry(
        "Typical weekend day, time: creating or editing own digital content",
        c("2022", "2025")
      ),
      ICTWKDY = pisa_entry(
        "Time on digital devices on weekdays index",
        c("2022", "2025")
      ),
      ICTWKEND = pisa_entry(
        "Time on digital devices on weekend days index",
        c("2022", "2025")
      ),
      ST322Q01JA = pisa_entry(
        "Turns off notifications during class",
        c("2022", "2025")
      ),
      ST322Q02JA = pisa_entry(
        "Turns off notifications when going to sleep",
        c("2022", "2025")
      ),
      ST322Q03JA = pisa_entry(
        "Keeps digital device near to answer messages when at home",
        "2022"
      ),
      ST322Q04JA = pisa_entry(
        "Has digital device open in class to take notes or search",
        "2022"
      ),
      ST322Q06JA = pisa_entry(
        "Feels pressured to be online and answer messages in class",
        c("2022", "2025")
      ),
      ST322Q07JA = pisa_entry(
        "Feels nervous or anxious without the digital device nearby",
        c("2022", "2025")
      ),
      ST250Q04JA = pisa_entry(
        "Has own cell phone with Internet access",
        c("2022", "2025")
      ),
      ST253Q01JA = pisa_entry(
        "Number of digital devices with screens at home",
        c("2022", "2025")
      ),
      ST254Q01JA = pisa_entry("Number at home: televisions", c("2022", "2025")),
      ST254Q02JA = pisa_entry(
        "Number at home: desktop computers",
        c("2022", "2025")
      ),
      ST254Q03JA = pisa_entry(
        "Number at home: laptop computers or notebooks",
        c("2022", "2025")
      ),
      ST254Q04JA = pisa_entry(
        "Number at home: tablets",
        c("2022", "2025")
      ),
      ST254Q05JA = pisa_entry(
        "Number at home: e-book readers",
        c("2022", "2025")
      ),
      ST254Q06JA = pisa_entry(
        "Number at home: cell phones with Internet access",
        "2022"
      )
    ),

    # --- achievement -----------------------------------------------------------
    pisa_pv_entries("MATH", "mathematics"),
    pisa_pv_entries("READ", "reading"),
    pisa_pv_entries("SCIE", "science"),
    pisa_pv_entries("FLIT", "financial literacy", c("2015", "2018", "2022")),
    pisa_pv_entries("CRTH_NC", "creative thinking (number correct)", "2022"),

    list(
      # --- well-being, mental health, bullying, school ------------------------
      ST016Q01NA = pisa_entry("Overall life satisfaction (0-10)"),
      BELONG = pisa_entry("Sense of belonging to school index"),
      ST034Q01TA = pisa_entry(
        "Belonging: I feel like an outsider at school",
        c("2015", "2018", "2022", "2025")
      ),
      ST034Q02TA = pisa_entry(
        "Belonging: I make friends easily at school",
        c("2015", "2018", "2022", "2025")
      ),
      ST034Q03TA = pisa_entry("Belonging: I feel like I belong at school"),
      ST034Q04TA = pisa_entry(
        "Belonging: I feel awkward and out of place at school",
        c("2015", "2018", "2022", "2025")
      ),
      ST034Q05TA = pisa_entry(
        "Belonging: other students seem to like me",
        c("2015", "2018", "2022", "2025")
      ),
      ST034Q06TA = pisa_entry("Belonging: I feel lonely at school"),
      ST038Q01NA = pisa_entry(
        "Bullied in past 12 months: called names by other students",
        "2015"
      ),
      ST038Q02NA = pisa_entry(
        "Bullied in past 12 months: picked on by other students",
        "2015"
      ),
      ST038Q03NA = pisa_entry(
        "Bullied in past 12 months: left out of things on purpose",
        c("2015", "2018", "2022")
      ),
      ST038Q04NA = pisa_entry(
        "Bullied in past 12 months: made fun of by other students"
      ),
      ST038Q05NA = pisa_entry(
        "Bullied in past 12 months: threatened by other students"
      ),
      ST038Q06NA = pisa_entry(
        "Bullied in past 12 months: things taken away or destroyed",
        c("2015", "2018", "2022")
      ),
      ST038Q07NA = pisa_entry(
        "Bullied in past 12 months: hit or pushed around by other students"
      ),
      ST038Q08NA = pisa_entry(
        "Bullied in past 12 months: nasty rumours spread by other students"
      ),
      ST038Q09JA = pisa_entry(
        "Past 12 months: was in a physical fight on school property",
        "2022"
      ),
      ST038Q10JA = pisa_entry(
        "Past 12 months: stayed home from school because felt unsafe",
        "2022"
      ),
      ST038Q11JA = pisa_entry(
        "Past 12 months: gave money to someone at school who threatened me",
        "2022"
      ),
      ST038Q12DA = pisa_entry(
        "Past 12 months: upsetting information about me published online without consent",
        "2025"
      ),
      BEINGBULLIED = pisa_entry("Exposure to bullying index", "2018"),
      BULLIED = pisa_entry("Exposure to bullying index", c("2022", "2025")),
      SWBP = pisa_entry("Subjective well-being: positive affect index", "2018"),
      EUDMO = pisa_entry("Eudaemonia: meaning in life index", "2018"),
      RESILIENCE = pisa_entry("Resilience index", "2018"),
      ST185Q01HA = pisa_entry(
        "Meaning in life: my life has clear meaning",
        "2018"
      ),
      ST185Q02HA = pisa_entry(
        "Meaning in life: I have discovered a satisfactory meaning",
        "2018"
      ),
      ST185Q03HA = pisa_entry(
        "Meaning in life: I have a clear sense of what gives meaning",
        "2018"
      ),
      ST186Q01HA = pisa_entry("How often feels: joyful", "2018"),
      ST186Q02HA = pisa_entry("How often feels: afraid", "2018"),
      ST186Q03HA = pisa_entry("How often feels: cheerful", "2018"),
      ST186Q05HA = pisa_entry("How often feels: happy", "2018"),
      ST186Q06HA = pisa_entry("How often feels: scared", "2018"),
      ST186Q07HA = pisa_entry("How often feels: lively", "2018"),
      ST186Q08HA = pisa_entry("How often feels: sad", "2018"),
      ST186Q09HA = pisa_entry("How often feels: proud", "2018"),
      ST186Q10HA = pisa_entry("How often feels: miserable", "2018"),
      ST188Q01HA = pisa_entry(
        "Resilience: I usually manage one way or another",
        "2018"
      ),
      ST188Q02HA = pisa_entry(
        "Resilience: I feel proud that I have accomplished things",
        "2018"
      ),
      ST188Q03HA = pisa_entry(
        "Resilience: I feel that I can handle many things at a time",
        "2018"
      ),
      ST188Q06HA = pisa_entry(
        "Resilience: my belief in myself gets me through hard times",
        "2018"
      ),
      ST188Q07HA = pisa_entry(
        "Resilience: in a difficult situation I can usually find my way out",
        "2018"
      ),
      WB150Q01HA = pisa_entry("Self-rated health", c("2018", "2022")),
      WB153Q01HA = pisa_entry(
        "Body image: I like my look just the way it is",
        c("2018", "2022")
      ),
      WB153Q02HA = pisa_entry(
        "Body image: I consider myself to be attractive",
        c("2018", "2022")
      ),
      WB153Q03HA = pisa_entry(
        "Body image: I am not concerned about my weight",
        c("2018", "2022")
      ),
      WB153Q04HA = pisa_entry(
        "Body image: I like my body",
        c("2018", "2022")
      ),
      WB153Q05HA = pisa_entry(
        "Body image: I like the way my clothes fit me",
        c("2018", "2022")
      ),
      BODYIMA = pisa_entry("Body image index", c("2018", "2022")),
      WB154Q01HA = pisa_entry(
        "Past six months, how often: headache",
        c("2018", "2022")
      ),
      WB154Q02HA = pisa_entry(
        "Past six months, how often: stomach pain",
        c("2018", "2022")
      ),
      WB154Q03HA = pisa_entry(
        "Past six months, how often: back pain",
        c("2018", "2022")
      ),
      WB154Q04HA = pisa_entry(
        "Past six months, how often: feeling depressed",
        c("2018", "2022")
      ),
      WB154Q05HA = pisa_entry(
        "Past six months, how often: irritability or bad temper",
        c("2018", "2022")
      ),
      WB154Q06HA = pisa_entry(
        "Past six months, how often: feeling nervous",
        c("2018", "2022")
      ),
      WB154Q07HA = pisa_entry(
        "Past six months, how often: difficulties getting to sleep",
        c("2018", "2022")
      ),
      WB154Q08HA = pisa_entry(
        "Past six months, how often: feeling dizzy",
        c("2018", "2022")
      ),
      WB154Q09HA = pisa_entry(
        "Past six months, how often: feeling anxious",
        c("2018", "2022")
      ),
      PSYCHSYM = pisa_entry("Psychosomatic symptoms index", "2022"),
      WB155Q01HA = pisa_entry("Satisfied with: your health", c("2018", "2022")),
      WB155Q02HA = pisa_entry(
        "Satisfied with: the way you look",
        c("2018", "2022")
      ),
      WB155Q03HA = pisa_entry(
        "Satisfied with: what you learn at school",
        c("2018", "2022")
      ),
      WB155Q04HA = pisa_entry(
        "Satisfied with: your friends",
        c("2018", "2022")
      ),
      WB155Q05HA = pisa_entry(
        "Satisfied with: the neighbourhood you live in",
        c("2018", "2022")
      ),
      WB155Q06HA = pisa_entry(
        "Satisfied with: all the things you have",
        c("2018", "2022")
      ),
      WB155Q07HA = pisa_entry(
        "Satisfied with: how you use your time",
        c("2018", "2022")
      ),
      WB155Q08HA = pisa_entry(
        "Satisfied with: your relationship with your parents",
        c("2018", "2022")
      ),
      WB155Q09HA = pisa_entry(
        "Satisfied with: your relationship with your teachers",
        c("2018", "2022")
      ),
      WB155Q10HA = pisa_entry(
        "Satisfied with: your life at school",
        c("2018", "2022")
      ),
      LIFESAT = pisa_entry("Life satisfaction across domains index", "2022"),
      WB156Q01HA = pisa_entry("Number of close friends", c("2018", "2022")),
      WB158Q01HA = pisa_entry(
        "Days per week spent with friends right after school",
        c("2018", "2022")
      ),
      WB160Q01HA = pisa_entry(
        "How often contacts friends by phone, text or social media",
        c("2018", "2022")
      ),
      WB164Q01HA = pisa_entry(
        "How often worries about the family's money",
        c("2018", "2022")
      ),
      SOCONPA = pisa_entry(
        "Social connection to parents index",
        c("2018", "2022")
      ),
      ANXMAT = pisa_entry("Mathematics anxiety index", "2022"),
      ST062Q01TA = pisa_entry(
        "Last two weeks: skipped a whole school day",
        c("2015", "2018", "2022", "2025")
      ),
      ST062Q02TA = pisa_entry(
        "Last two weeks: skipped some classes",
        c("2015", "2018", "2022", "2025")
      ),
      ST062Q03TA = pisa_entry(
        "Last two weeks: arrived late for school",
        c("2015", "2018", "2022", "2025")
      ),
      ST296Q04JA = pisa_entry(
        "Time spent on homework per day, all subjects",
        c("2022", "2025")
      ),

      # --- school questionnaire ---------------------------------------------
      SC001Q01TA = pisa_entry("School community size (village to megacity)"),
      SC013Q01TA = pisa_entry("Public or private school"),
      SC002Q01TA = pisa_entry(
        "Total enrolment: boys",
        source = c(
          "2015" = "SC002Q01TA",
          "2018" = "SC002Q01TA",
          "2022" = "SC002Q01TA",
          "2025" = "SC002Q01TA_P"
        )
      ),
      SC002Q02TA = pisa_entry(
        "Total enrolment: girls",
        source = c(
          "2015" = "SC002Q02TA",
          "2018" = "SC002Q02TA",
          "2022" = "SC002Q02TA",
          "2025" = "SC002Q02TA_P"
        )
      ),
      SCHSIZE = pisa_entry(
        "School size (total enrolment)",
        source = c(
          "2015" = "SCHSIZE",
          "2018" = "SCHSIZE",
          "2022" = "SCHSIZE",
          "2025" = "SCHSIZE_Q"
        )
      ),
      STRATIO = pisa_entry(
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

  sources <- lapply(pisa_cycles, function(cycle) {
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
  names(sources) <- pisa_cycles

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
pisa_select_columns <- function(tbl, wave) {
  map <- pisa_column_map()
  if (!wave %in% names(map)) {
    stop(
      "BPIPD-107: cycle ",
      wave,
      " is not in `pisa_column_map()`; add it to `pisa_cycles` and review ",
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
      " files lack columns `pisa_column_map()` expects: ",
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
pisa_cycle_specific_codes <- c("STRATUM")

pisa_zap_cycle_specific_labels <- function(tbl) {
  cols <- intersect(pisa_cycle_specific_codes, names(tbl))
  tbl[cols] <- lapply(tbl[cols], haven::zap_labels)
  tbl
}

#' Columns whose value labels are worded differently across cycles but whose
#' codes mean the same thing (for example "Turkey" / "Türkiye", "Native" /
#' "Native student"). Checked by hand against the 2015, 2018 and 2022 files;
#' extend after inspecting the conflicts `pisa_check_label_conflicts()`
#' reports for a new cycle.
pisa_label_conflicts_ok <- c(
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
#' `pisa_label_conflicts_ok`.
pisa_check_label_conflicts <- function(parts, columns = NULL) {
  labels_of <- function(tbl) {
    cols <- names(tbl)[vapply(tbl, haven::is.labelled, logical(1))]
    if (!is.null(columns)) {
      cols <- intersect(cols, columns)
    }
    out <- lapply(cols, function(col) {
      labs <- attr(tbl[[col]], "labels")
      labs <- labs[!pisa_is_missing_code(labs)]
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
  bad <- setdiff(shared[conflicts], pisa_label_conflicts_ok)
  if (length(bad) > 0) {
    stop(
      "BPIPD-107: value labels for the same code differ between cycles in: ",
      paste(bad, collapse = ", "),
      ". Split the column per coding in `pisa_column_map()` (as for HISCED) ",
      "or, if only the wording differs, add it to `pisa_label_conflicts_ok`.",
      call. = FALSE
    )
  }
  invisible(shared[conflicts])
}

#' PISA missing-value codes: 5-9, 95-99, 995-999, 9995-9999, ...
pisa_is_missing_code <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  !is.na(x) & grepl("^9*[5-9]$", format(x, scientific = FALSE, trim = TRUE))
}

#' Bind tables, muffling only the label-wording warnings already reviewed
#' (or, within a cycle, in columns the map does not keep)
pisa_bind_waves <- function(parts) {
  withCallingHandlers(
    dplyr::bind_rows(parts),
    warning = function(w) {
      if (grepl("conflicting value labels", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
