#' Tidier for BPIPD-170 (Monitoring the Future)
#'
#' Two annual cross-sections: an 8th/10th grade release (eight grade-by-form
#' files that stack up to 2011, one file from 2012) and a 12th grade release
#' (a DS0001 core of every senior plus six form files joined on the serial).
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per respondent per year, all years stacked
tidy_BPIPD_170 <- function(raw_dataset, spec) {
  item_vars <- bp170_item_vars()
  serial_pattern <- "ARCHIVE ID|R'?S +ID ?- ?SERIAL"

  var_labels <- function(df) {
    frame_labels <- attr(df, "variable.labels")
    if (!is.null(frame_labels)) {
      return(stats::setNames(as.character(frame_labels), names(frame_labels)))
    }
    vapply(df, \(col) attr(col, "label") %||% NA_character_, character(1))
  }

  find_vars <- function(df, pattern) {
    labels <- var_labels(df)
    names(labels)[!is.na(labels) & grepl(pattern, labels, ignore.case = TRUE)]
  }

  drop_labels <- function(x) {
    if (inherits(x, "haven_labelled")) {
      x <- unclass(x)
      attributes(x) <- NULL
    }
    x
  }

  find_var <- function(df, pattern, file) {
    hits <- find_vars(df, pattern)
    if (length(hits) > 1) {
      stop(
        "BPIPD-170: `",
        pattern,
        "` matches more than one variable in ",
        file,
        ": ",
        paste(hits, collapse = ", "),
        call. = FALSE
      )
    }
    if (length(hits) == 0) NULL else hits
  }

  as_code <- function(x, name, file) {
    x <- drop_labels(x)
    if (!is.factor(x) && !is.character(x)) {
      return(as.numeric(x))
    }
    values <- as.character(x)
    codes <- sub("^\\((-?[0-9]+)\\).*$", "\\1", values)
    bad <- !is.na(values) & codes == values
    if (any(bad)) {
      stop(
        "BPIPD-170: `",
        name,
        "` in ",
        file,
        " has values that are not ICPSR `(code) LABEL` factors: ",
        paste(utils::head(unique(values[bad]), 3), collapse = ", "),
        call. = FALSE
      )
    }
    as.numeric(codes)
  }

  prep_file <- function(name, file, stream, year, ds) {
    df <- raw_dataset$data[[name]]
    file_labels <- var_labels(df)

    if (
      !is.null(df$.wave) &&
        !identical(unique(as.character(df$.wave)), as.character(year))
    ) {
      stop(
        "BPIPD-170: `.wave` disagrees with the year in the path for ",
        file,
        call. = FALSE
      )
    }

    # From 2018 the 8th/10th file carries one serial per grade (`AI_08`,
    # `AI_10`); a respondent has a value in exactly one of them.
    serial_cols <- find_vars(df, serial_pattern)
    if (length(serial_cols) == 0) {
      stop("BPIPD-170: no archive serial found in ", file, call. = FALSE)
    }
    serial <- as.character(drop_labels(df[[serial_cols[1]]]))
    for (col in serial_cols[-1]) {
      serial <- ifelse(
        is.na(serial),
        as.character(drop_labels(df[[col]])),
        serial
      )
    }

    # Up to 2011 the 8th/10th release splits the two grades across files:
    # DS0001-DS0004 are the four 8th-grade forms and DS0005-DS0008 the four
    # 10th-grade forms.
    grade <- if (identical(stream, "12")) {
      rep(12L, nrow(df))
    } else if (year <= 2011L) {
      rep(if (ds <= 4L) 8L else 10L, nrow(df))
    } else {
      as.integer(as_code(df$V501, "V501", file))
    }

    weight_col <- intersect(c("V5", "ARCHIVE_WT"), names(df))

    out <- tibble::tibble(
      wave = as.character(year),
      .wave_label = if (is.null(df$.wave_label)) {
        NA_character_
      } else {
        as.character(df$.wave_label)
      },
      # Kept deliberately: `participant_id` is built from it.
      data_year = year,
      grade = grade,
      form = as.integer(as_code(df$V3, "V3", file)),
      weight = if (length(weight_col)) {
        as.numeric(drop_labels(df[[weight_col[1]]]))
      } else {
        NA_real_
      },
      serial = serial
    )

    # MTF renumbers its variables.
    for (nm in names(item_vars)) {
      col <- find_var(df, item_vars[[nm]], file)
      if (!is.null(col)) {
        out[[nm]] <- as_code(df[[col]], col, file)
        attr(out[[nm]], "label") <- paste0(col, ": ", file_labels[[col]])
      }
    }

    # Resolved here rather than in `variables.csv` because the codes change
    # in 2005: 0/1 is White/Black up to 2004, then 1/2/3 is Black/White/Hispanic
    if ("race" %in% names(out)) {
      labels <- if (year <= 2004L) {
        c("0" = "White", "1" = "Black")
      } else {
        c("1" = "Black", "2" = "White", "3" = "Hispanic")
      }
      race_label <- attr(out$race, "label", exact = TRUE)
      out$race <- unname(labels[as.character(out$race)])
      attr(out$race, "label") <- race_label
    }

    out
  }

  # ---- index the released files ------------------------------------------
  matches <- raw_dataset$meta$matches
  matches <- matches[matches$role == "data", , drop = FALSE]

  index <- tibble::tibble(
    name = as.character(matches$resource_name),
    file = as.character(matches$file),
    stream = ifelse(grepl("8th-10th", matches$file), "8-10", "12"),
    year = as.integer(sub(".*/(\\d{4})/DS[0-9]+/[^/]+$", "\\1", matches$file)),
    ds = as.integer(sub(".*-([0-9]{4})-Data\\.[a-z]+$", "\\1", matches$file))
  )

  missing <- setdiff(index$name, names(raw_dataset$data))
  if (length(missing) > 0) {
    stop(
      "BPIPD-170: spec resources with no ingested table: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (anyNA(index$year) || anyNA(index$ds)) {
    stop(
      "BPIPD-170: could not read the year and dataset number from: ",
      paste(
        utils::head(index$file[is.na(index$year) | is.na(index$ds)], 3),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  # Everything outside these ranges is a supplementary release
  is_primary <- (index$stream == "12" & index$ds <= 7L) |
    (index$stream == "8-10" & index$year <= 2011L & index$ds <= 8L) |
    (index$stream == "8-10" & index$year >= 2012L & index$ds == 1L)
  index <- index[is_primary, ]

  prepped <- Map(
    prep_file,
    index$name,
    index$file,
    index$stream,
    index$year,
    index$ds
  )

  # ---- assemble each stream-year -----------------------------------------
  assemble <- function(rows) {
    if (!identical(index$stream[rows[1]], "12")) {
      # 8th/10th grade: form files are disjoint samples, so they simply stack.
      return(dplyr::bind_rows(prepped[rows]))
    }

    core_rows <- rows[index$ds[rows] == 1L]
    if (length(core_rows) != 1) {
      stop(
        "BPIPD-170: expected exactly one 12th grade core file (DS0001) for ",
        index$year[rows[1]],
        call. = FALSE
      )
    }
    core <- prepped[[core_rows]]
    form_rows <- rows[index$ds[rows] > 1L]
    if (length(form_rows) == 0) {
      return(core)
    }

    forms <- dplyr::bind_rows(prepped[form_rows])
    form_only <- setdiff(names(forms), names(core))
    if (length(form_only) == 0) {
      return(core)
    }

    dplyr::left_join(
      core,
      forms[c("serial", form_only)],
      by = "serial",
      relationship = "one-to-one"
    )
  }

  df <- dplyr::bind_rows(
    unname(lapply(
      split(seq_len(nrow(index)), paste(index$stream, index$year)),
      assemble
    ))
  )

  # ---- participant key ----------------------------------------------------
  # The serial repeats between the 8th- and 10th-grade samples of a year and
  # again in every later year, so all three parts are needed.
  df <- dplyr::mutate(
    df,
    participant_id = paste(data_year, grade, serial, sep = "-"),
    .before = 1
  )

  # `bind_rows()` drops the label attributes the item loop attached, so put
  # back the label of the first file that matched each column.
  item_labels <- list()
  for (part in prepped) {
    for (nm in names(item_vars)) {
      label <- attr(part[[nm]], "label", exact = TRUE)
      if (!is.null(label) && is.null(item_labels[[nm]])) {
        item_labels[[nm]] <- label
      }
    }
  }
  labels <- c(bp170_design_labels(), unlist(item_labels))
  for (nm in names(labels)) {
    attr(df[[nm]], "label") <- unname(labels[[nm]])
  }

  unlabelled <- names(df)[
    vapply(df, \(col) is.null(attr(col, "label", exact = TRUE)), logical(1))
  ]
  if (length(unlabelled) > 0) {
    stop(
      "BPIPD-170: columns with no label: ",
      paste(unlabelled, collapse = ", "),
      call. = FALSE
    )
  }

  duplicated_ids <- unique(df$participant_id[duplicated(df$participant_id)])
  if (length(duplicated_ids) > 0) {
    stop(
      "BPIPD-170: `participant_id` is not unique; ",
      length(duplicated_ids),
      " repeated, e.g. ",
      paste(utils::head(duplicated_ids, 3), collapse = ", "),
      call. = FALSE
    )
  }

  df
}

#' Item variables, by the label text that identifies them
#'
#' MTF renumbers its variables across forms, grades and releases (e.g., TV hours
#' is V1120 in 2000, V1121 from 2004, V2120/V3120/V4120 on the other forms), so
#' each item is located by its label rather than by name.
bp170_item_vars <- function() {
  design_vars <- c(
    region = "SCHOOL REGION|SCHL RGN|SCH REG",
    smsa = "NON[- ]?S?MSA",
    large_msa = "LARGE MSA",
    pop_density = "POPULATION DENSITY"
  )
  demographic_vars <- c(
    sex = "R'?S SEX",
    race = "R'?S RACE|RACE--B/W/H",
    age_ge18 = "AGE <>18 DICHOTOMY",
    age_ge16 = "AGE <>16 DICHOTOMY",
    grade_level = "WHAT GRADE LEVL",
    mother_education = "MOTHR EDUC LEVE",
    father_education = "FATHR EDUC LEVE",
    hshld_father = "HSHLD FATHE",
    hshld_mother = "HSHLD MOTHE"
  )
  # Time-use items are in hours per WEEK up to 2017 and hours per DAY from 2018
  screen_week_vars <- c(
    computer_hrs_week_school = "HR/W CO?MPUTR SC",
    computer_hrs_week_job = "HR/W CO?MPUTR JO",
    computer_hrs_week_other = "HR/W CO?MPUTR OT",
    internet_hrs_week_leisure = "HR/W INTERNET S",
    gaming_hrs_week = "HR/W GAMING",
    social_media_hrs_week = "HR/W SOCIAL NET WEB",
    texting_hrs_week = "HR/W TEXT CELL PHO",
    phone_talk_hrs_week = "HR/W TALK CELL PHO",
    video_chat_hrs_week = "HR/W VIDEO CHAT",
    homework_hrs_week = "HRS/WK SPND HMWK"
  )
  screen_day_vars <- c(
    tv_hrs_weekday = "HRS TV/DAY",
    tv_hrs_weekend = "HRS TV/WKEND",
    watch_video_hrs_weekday = "WKDAY HRS WATCH/VIDEO",
    watch_video_hrs_weekend = "WKEND HRS WATCH/VIDEO",
    gaming_hrs_day = "HRS/DAY GAMING",
    social_media_hrs_day = "HRS/DAY SOCIAL/NETW",
    texting_hrs_day = "HRS/DAY TEXTING",
    phone_talk_hrs_day = "HRS/DAY TALK PHONE",
    video_chat_hrs_day = "HRS/DAY VIDEO CHAT",
    email_hrs_day = "HRS/DAY EMAILING",
    shop_online_hrs_day = "HRS/DAY SHOP ONLINE"
  )
  outcome_vars <- c(
    grade_average = "R HS GRADE/D ?= ?1",
    fight_gang = "FRQ GANG FIGHT",
    steal_under50 = "FRQ STEAL <\\$50",
    steal_over50 = "FRQ STEAL >\\$50",
    damage_school_property = "FRQ DMG SCH PPTY",
    skip_days_4wk = "#DA/4W SC MS CUT",
    skip_class_4wk = "#DA/4W SKP CLASS",
    bullied_school = "BULLIED@SCHL",
    bullied_online = "BULLIED ONLINE",
    esteem_pos_attitude = "ATT TWD SELF",
    esteem_person_worth = "PRSN OF WORTH",
    esteem_satisfied = "SATISFD W MYSELF",
    esteem_proud = "MUCH TO B PROUD",
    dep_enjoy_life = "I ENJOY LIFE",
    dep_meaningless = "LIFE MEANINGLESS",
    dep_good_alive = "GOOD TO BE ALIVE",
    dep_hopeless = "FUTURE HOPELESS",
    dep_cant_do_right = "I DO WRONG THING",
    dep_life_not_useful = "MY LIFE NT USEFL",
    dep_lonely = "OFTN FEEL LONELY",
    lonely_left_out = "OFTN FL LEFT OUT",
    lonely_wish_friends = "OFT WSH MOR FRND",
    lonely_turn_to = "ALWYS SM1 HELP R",
    lonely_talk_to = "USLY SM1 TALK TO",
    lonely_friends_around = "USLY FRDS BE WTH",
    anxious = "OFTEN FEEL ANXIOUS",
    life_satisfaction = "LIFE AS WHL",
    sat_education = "SAT EDUC EXPRNC",
    sat_friends = "SAT OWN FRIENDS",
    sat_parents = "SAT GT ALNG PRN",
    enjoy_school = "LSTYR/ENJOY SCHL",
    happy_school = "LSTYR/HAPPY IN SCH"
  )
  c(
    design_vars,
    demographic_vars,
    screen_week_vars,
    screen_day_vars,
    outcome_vars
  )
}

#' Labels for the columns the tidier builds
bp170_design_labels <- function() {
  c(
    participant_id = "Participant key: survey year, grade and archive serial",
    wave = "Survey year (spec wave)",
    .wave_label = "Wave label stamped by the reader (the survey year)",
    data_year = "Year of the annual survey administration",
    grade = "Grade sampled (8, 10 or 12)",
    form = "V3: FORM ID (questionnaire form)",
    weight = "V5/ARCHIVE_WT: SAMPLING WEIGHT / ARCHIVE WEIGHT",
    serial = "ARCHIVE ID / R'S ID-SERIAL # (archive serial number)"
  )
}
