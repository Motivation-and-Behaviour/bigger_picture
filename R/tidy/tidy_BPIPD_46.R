#' Tidier for BPIPD-46 (PSID-CDS)
#'
#' Each wave's demographic file lists its participating children; the other
#' child files join one to one, the PCG-household file many to one, and the
#' diary episode files are summed per child and diary day.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per CDS wave the child took part in
tidy_BPIPD_46 <- function(raw_dataset, spec) {
  labels <- bp46_layout_labels(spec)
  waves <- vapply(spec$waves, function(w) as.character(w$wave), character(1))
  parts <- lapply(waves, function(wave) {
    bp46_wave(raw_dataset$data, wave, labels)
  })
  df <- dplyr::bind_rows(parts)
  # bind_rows() drops the label of a column that several waves share.
  shared <- unlist(lapply(parts, function(part) lapply(part, attr, "label")))
  shared <- shared[!duplicated(names(shared))]
  unlabelled <- vapply(df, function(x) is.null(attr(x, "label")), logical(1))
  for (nm in intersect(names(df)[unlabelled], names(shared))) {
    attr(df[[nm]], "label") <- shared[[nm]]
  }

  # The PSID person identifier is the 1968 interview number x 1000 + person
  # number (CDS-2019 User Guide, Table 4.2).
  df$participant_id <- sprintf("%.0f", df$CDSCUMID68 * 1000 + df$CDSCUMPN)
  attr(df$participant_id, "label") <-
    "PSID person identifier: CDSCUMID68 x 1000 + CDSCUMPN"
  # CDS-II and CDS-III re-interviewed only CDS-I children, and CDS-2019 to
  # CDS-2021 only CDS-2014 children and children newly aged 0-4 in 2019.
  df$cohort <- ifelse(
    df$.wave %in% c("1997", "2002", "2007"),
    "Original CDS (CDS-I 1997)",
    "CDS-2014 generation"
  )
  attr(df$cohort, "label") <- "CDS sample the child was selected into"
  df <- dplyr::relocate(df, "participant_id", "cohort", ".wave", ".wave_label")

  df <- bp46_carry_to_2020(df, bp46_household_columns(df))
  df <- bp46_repeat_grade(df, waves)
  df <- bp46_family_unit(df)

  # CDS-2021 asked only frequency screen items (no diary or duration item), so
  # its rows go once they have served as donors to the CDS-2020 carries.
  df <- dplyr::filter(df, .wave != "2021")
  # Its own columns are then empty, except those holding carried values.
  df <- df[vapply(df, function(x) !all(is.na(x)), logical(1))]

  if (anyDuplicated(df[c("participant_id", ".wave")]) > 0) {
    stop(
      "BPIPD-46: `participant_id` and `.wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' Child-level files joined to each wave's spine, by resource stem
bp46_child_files <- function() {
  list(
    "1997" = c(
      "pcg_child",
      "child",
      "td_agg",
      "td_followup",
      "teacher_ems",
      "teacher_pd"
    ),
    "2002" = c(
      "pcg_child",
      "child",
      "assess",
      "td_agg",
      "td_followup",
      "teacher_ems"
    ),
    "2007" = c("pcg_child", "child", "assess", "td_agg", "td_followup"),
    "2014" = c("pcg_child", "child", "assess", "td_agg", "td_followup"),
    "2019" = c("pcg_child", "child", "assess", "td_agg", "td_followup"),
    "2020" = c("td_agg", "td_followup"),
    "2021" = c("pcg_child", "child")
  )
}

#' Variable labels for every fixed-width field, from the spec's layout files
bp46_layout_labels <- function(spec) {
  resources <- c(
    spec$resources,
    unlist(lapply(spec$waves, `[[`, "resources"), recursive = FALSE)
  )
  layouts <- unique(unlist(lapply(resources, `[[`, "col_positions")))
  dir <- bp_harmonisation_dataset_dir(spec$dataset_id)
  fields <- do.call(
    rbind,
    lapply(layouts, function(path) {
      utils::read.csv(fs::path(dir, path), colClasses = "character")
    })
  )
  stats::setNames(fields$label, fields$name)
}

#' Attach the layout's variable label to each column of a raw table
bp46_label <- function(tbl, labels) {
  tbl[] <- Map(
    function(x, nm) {
      if (nm %in% names(labels)) {
        attr(x, "label") <- labels[[nm]]
      }
      x
    },
    tbl,
    names(tbl)
  )
  tbl
}

#' Assemble one wave: the spine, its child files, PCG household and diary sums
bp46_wave <- function(data, wave, labels) {
  yy <- substr(wave, 3, 4)
  take <- function(stem, from = wave) {
    tbl <- data[[paste0(stem, "_", from)]]
    bp46_label(tbl[setdiff(names(tbl), c(".wave", ".wave_label"))], labels)
  }
  join <- function(base, tbl, by, relationship = "one-to-one") {
    dplyr::left_join(
      base,
      tbl,
      by = by,
      keep = TRUE,
      relationship = relationship
    )
  }

  # The demographic file holds every child with a PCG-child or child
  # interview; CDS-2020 has none, and its PCG-child file holds every child.
  base <- bp46_label(
    data[[paste0(if (wave == "2020") "pcg_child" else "demog", "_", wave)]],
    labels
  )
  # Every child file starts with a release number, then the wave's PSID
  # family interview and sequence numbers.
  key <- names(base)[2:3]
  for (stem in bp46_child_files()[[wave]]) {
    tbl <- take(stem)
    base <- join(base, tbl, stats::setNames(names(tbl)[2:3], key))
  }

  # DEMOG2019 carries the CDS-2020 eligibility, result and weights.
  cds2020 <- c("X19C20ELIG", "X19C20RES", "X19CVWGT", "X19LG20WGT")
  if (wave == "2019") {
    base <- base[setdiff(names(base), cds2020)]
  }
  if (wave == "2020") {
    base <- dplyr::left_join(
      base,
      take("demog", "2019")[c("X19YRID", "X19CYPSN", cds2020)],
      by = c(P20YRID = "X19YRID", P20CYPSN = "X19CYPSN"),
      relationship = "one-to-one"
    )
  }

  # CDS-2020 children carry their CDS-2019 identifiers (CDS_2020_readme.txt).
  link <- if (wave == "2020") "19" else yy
  ind <- bp46_label(data$cdsind_2021, labels)
  ids <- c(
    "CDSCUMREL",
    "CDSCUMID68",
    "CDSCUMPN",
    paste0(c("CRFID", "CRSN"), link),
    grep(paste0(yy, "$"), names(ind), value = TRUE)
  )
  if (wave == "2020") {
    ids <- c(ids, "CDS_HID19", "PCGHHNO19")
  }
  base <- dplyr::left_join(
    base,
    ind[unique(ids)],
    by = stats::setNames(paste0(c("CRFID", "CRSN"), link), key),
    relationship = "one-to-one"
  )

  # PCG-household files key on CDS household and PCG number, which the wave's
  # ID map gives to 2007 and CDSIND2021 from 2014 (core IDs miss some PCGs).
  hh <- take("pcg_hh")
  if (wave %in% c("1997", "2002", "2007")) {
    im <- take("id_map")
    base <- join(base, im, stats::setNames(names(im)[1:2], key))
    prefix <- c("1997" = "Q2", "2002" = "Q22", "2007" = "Q32")[[wave]]
    hh_by <- stats::setNames(
      paste0(prefix, c("CDSHID", "CDSHPIN")),
      paste0(c("CDSHID", "PCGINST"), yy)
    )
  } else {
    if (wave == "2014") {
      im <- take("id_map")
      base <- join(base, im, stats::setNames(names(im)[1:2], key))
    }
    hh_by <- stats::setNames(
      paste0("H", yy, c("CDSHID", "PCGHH")),
      paste0(c("CDS_HID", "PCGHHNO"), link)
    )
  }
  base <- join(base, hh, hh_by, "many-to-one")
  if (wave == "2020") {
    base <- base[setdiff(names(base), c("CDS_HID19", "PCGHHNO19"))]
  }

  # Sex is recorded only on the household roster (from 2014).
  if (wave %in% c("2014", "2019", "2021")) {
    base <- join(
      base,
      take("hh_roster"),
      stats::setNames(
        paste0("R", yy, c("ID68", "PN")),
        c("CDSCUMID68", "CDSCUMPN")
      )
    )
  }

  if (wave != "2021") {
    base <- dplyr::left_join(
      base,
      bp46_diary_sums(data, wave, key),
      by = key,
      relationship = "one-to-one"
    )
  }
  if (wave == "1997") {
    base <- dplyr::left_join(
      base,
      bp46_school_diary_sums(data, key),
      by = key,
      relationship = "one-to-one"
    )
  }

  base
}

#' Primary-activity codes of screen use: TV, electronic games, devices
bp46_screen_codes <- function(wave) {
  # The study's home computer and electronic-device block without 509x
  # (school) and 5390/5960 (travel), plus online classes (2014 coding manual).
  if (wave == "1997") {
    c(501:508, 510:513, 879, 919)
  } else {
    c(5010:5089, 5100:5139, 5200, 8790, 9190)
  }
}

#' Activity codes in the episode file that the aggregate file has no column for
bp46_unaggregated_codes <- function() {
  # Descriptions from the 2014 Time Diary Coding Manual.
  codes <- c(
    "5052" = "texting using a cell phone, smartphone, tablet or other device",
    "5053" = "creating and uploading internet content",
    "5200" = "online classes, all devices",
    "5960" = "travel for non-academic computer or electronic-device use",
    "7741" = "criminal or illegal activity, interaction with the police",
    "8785" = "indoor play area",
    "9391" = "listening to books on CD or another recording on a device",
    "9663" = "time out as a punishment"
  )
  list(
    "2014" = codes,
    "2019" = codes[names(codes) != "7741"],
    "2020" = codes[!names(codes) %in% c("7741", "8785")]
  )
}

#' Device codes of the 2014 episode file summed per device, all and gaming
bp46_devices_2014 <- function() {
  # Device used with the primary activity (COLF_A1_14, COLF_A2_14; TD_ACT14).
  list(
    any = c("6" = "computer", "9" = "smartphone", "10" = "tablet"),
    game = c(
      "5" = "console",
      "6" = "computer",
      "9" = "smartphone",
      "10" = "tablet"
    )
  )
}

#' Episode, source and companion columns of a wave's diary activity file
bp46_activity_columns <- function(wave) {
  yy <- substr(wave, 3, 4)
  # No one else, mother, father, stepmother, stepfather
  who <- c("A", "B", "C", "E", "F")
  switch(
    wave,
    "1997" = list(
      activity = "COLA",
      day = "WDAYWEND",
      duration = "DURATION",
      with = paste0("COLG_", who),
      around = paste0("COLH_", who),
      companions = "^COL[GH]_[A-M]$"
    ),
    "2002" = list(
      activity = "COLA_02",
      day = "DIARY_02",
      duration = "DUR_02",
      with = paste0("COLG", who, "_02"),
      around = paste0("COLH", who, "_02"),
      companions = "^COL[GH][A-M]_02$"
    ),
    list(
      activity = paste0("COLA_", yy),
      day = paste0("DIARY_", yy),
      duration = paste0("DUR_", yy),
      with = paste0("COLH", who, "_", yy),
      around = paste0("COLI", who, "_", yy),
      companions = paste0("^COL[HI][A-M]_", yy, "$")
    )
  )
}

#' Seconds per diary day from the media and activity episode files
bp46_diary_sums <- function(data, wave, key) {
  yy <- substr(wave, 3, 4)
  agg <- data[[paste0("td_agg_", wave)]]
  child <- paste(agg[[2]], agg[[3]])
  out <- stats::setNames(agg[2:3], key)

  media <- data[[paste0("td_media_", wave)]]
  m <- function(n) media[[sprintf("TD%sM%02d", yy, n)]]
  m_child <- paste(m(2), m(3))
  m_day <- ifelse(m(4) == 1, "wd", "we")

  act <- data[[paste0("td_activity_", wave)]]
  cols <- bp46_activity_columns(wave)
  a_child <- paste(act[[2]], act[[3]])
  a_day <- ifelse(act[[cols$day]] == 1, "wd", "we")
  code <- act[[cols$activity]]
  screen <- code %in% bp46_screen_codes(wave)
  selected <- function(columns) Reduce(`|`, lapply(act[columns], `==`, 1))
  parent_with <- selected(cols$with[-1])
  parent_around <- !parent_with & selected(cols$around[-1])
  # No one marked taking part or around means no companion was recorded.
  with_na <- !selected(grep(cols$companions, names(act), value = TRUE))

  violence <- c(
    "1" = "non-violent",
    "2" = "slapstick or comedic violence",
    "3" = "sports violence",
    "4" = "victimization",
    "5" = "gratuitous violence",
    "6" = "ultra violence",
    "8" = "uncodeable",
    "9" = "not ascertained"
  )
  curriculum <- c(
    "1" = "no curriculum",
    "2" = "pro-social",
    "3" = "school readiness",
    "4" = "extended academic learning",
    "5" = "informal learning",
    "6" = "culture, science or history",
    "8" = "uncodeable",
    "9" = "not ascertained"
  )
  extra <- bp46_unaggregated_codes()[[wave]]
  # 2014 device mentions with the primary activity (used only in 2014)
  devices <- bp46_devices_2014()
  on <- function(k) act$COLF_A1_14 == k | act$COLF_A2_14 == k

  for (day in c("wd", "we")) {
    day_name <- if (day == "wd") "weekday" else "weekend"
    # 99999 in the aggregate file marks a diary day that was not returned
    tv <- agg[[paste0(toupper(day), yy, "_919")]]
    returned <- tv != 99999
    # A returned day with no matching episode spent zero seconds on it
    day_total <- function(secs, keep, ids, label) {
      x <- as.numeric(tapply(secs[keep], ids[keep], sum)[child])
      x[is.na(x)] <- 0
      x[!returned] <- NA
      attr(x, "label") <- paste0("Time diary, ", day_name, ": seconds ", label)
      x
    }
    media_total <- function(keep, label) {
      day_total(m(8), keep & m_day == day, m_child, label)
    }
    act_total <- function(keep, label) {
      day_total(act[[cols$duration]], keep & a_day == day, a_child, label)
    }

    for (k in names(violence)) {
      out[[paste0("tdmedia_violence_", k, "_secs_", day)]] <- media_total(
        m(16) == as.numeric(k),
        paste0("of TV the study coded violence ", k, " (", violence[[k]], ")")
      )
    }
    for (k in names(curriculum)) {
      out[[paste0("tdmedia_curriculum_", k, "_secs_", day)]] <- media_total(
        m(15) == as.numeric(k),
        paste0(
          "of TV the study coded curriculum ",
          k,
          " (",
          curriculum[[k]],
          ")"
        )
      )
    }
    out[[paste0("tdmedia_uncoded_secs_", day)]] <- structure(
      tv - media_total(TRUE, ""),
      label = paste0(
        "Time diary, ",
        day_name,
        ": seconds of TV (activity 919) with no episode in the media file"
      )
    )

    out[[paste0("tdact_screen_secs_", day)]] <- act_total(
      screen,
      "of screen activities (TV, electronic games, computer or device)"
    )
    out[[paste0("tdact_screen_parent_with_secs_", day)]] <- act_total(
      screen & parent_with,
      "of screen activities with a mother, father or step-parent taking part"
    )
    out[[paste0("tdact_screen_parent_around_secs_", day)]] <- act_total(
      screen & parent_around,
      "of screen activities with a parent figure around but none taking part"
    )
    out[[paste0("tdact_screen_with_na_secs_", day)]] <- act_total(
      screen & with_na,
      "of screen activities with no companion recorded"
    )
    for (k in names(extra)) {
      out[[paste0("tdact_", k, "_secs_", day)]] <- act_total(
        code == as.numeric(k),
        paste0("of activity ", k, " (", extra[[k]], ")")
      )
    }

    if (wave == "2014") {
      mention <- " as 1st or 2nd device used with the primary activity"
      for (k in names(devices$any)) {
        out[[paste0("tdact_device_", devices$any[[k]], "_secs_", day)]] <-
          act_total(
            on(as.numeric(k)),
            paste0(
              "of primary activities with ",
              devices$any[[k]],
              " (device code ",
              k,
              ")",
              mention
            )
          )
      }
      for (k in names(devices$game)) {
        out[[paste0("tdact_game_", devices$game[[k]], "_secs_", day)]] <-
          act_total(
            code == 5020 & on(as.numeric(k)),
            paste0(
              "of electronic games (activity 5020) with ",
              devices$game[[k]],
              " (device code ",
              k,
              ")",
              mention
            )
          )
      }
    }

    # Homework (5490) on a computer, smartphone or tablet: 2014 device codes,
    # 2019-2020 the yes/no box "Did you use a smartphone, tablet or computer?".
    homework_device <- switch(
      wave,
      "2014" = Reduce(`|`, lapply(as.numeric(names(devices$any)), on)),
      "2019" = ,
      "2020" = act[[paste0("COLF_", yy)]] == 1
    )
    if (!is.null(homework_device)) {
      out[[paste0("tdact_homework_device_secs_", day)]] <- act_total(
        code == 5490 & homework_device,
        paste0(
          "of homework (activity 5490) with a computer, smartphone or tablet ",
          "(2014: device code 6, 9 or 10 as 1st or 2nd device used with the ",
          "primary activity; 2019 and 2020: device box COLF_19 or COLF_20 = 1, ",
          "used a smartphone, tablet or computer)"
        )
      )
    }
  }
  out
}

#' Seconds of the 1997 teacher-kept school-day diaries, all and with computers
bp46_school_diary_sums <- function(data, key) {
  files <- list(
    es = c(table = "school_diary_es_1997", prefix = "Q9", computer = "Q9CJ07"),
    ms = c(table = "school_diary_ms_1997", prefix = "Q3", computer = "Q3CH06"),
    pd = c(table = "school_diary_pd_1997", prefix = "Q10", computer = "Q10CJ07")
  )
  school <- c(
    es = "elementary school",
    ms = "middle school",
    pd = "preschool or child care"
  )
  parts <- Map(
    function(f, nm) {
      sd <- data[[f[["table"]]]]
      ids <- paste(sd[[2]], sd[[3]])
      first <- !duplicated(ids)
      secs <- sd[[paste0(f[["prefix"]], "CC")]] -
        sd[[paste0(f[["prefix"]], "CB")]]
      computer <- sd[[f[["computer"]]]] == 1
      title <- paste0("School-day diary (", school[[nm]], " teacher, 1997): ")
      out <- stats::setNames(sd[first, 2:3], key)
      out[[paste0("sdiary_", nm, "_secs")]] <- structure(
        as.numeric(tapply(secs, ids, sum)[ids[first]]),
        label = paste0(title, "seconds of activities recorded")
      )
      out[[paste0("sdiary_", nm, "_computer_secs")]] <- structure(
        as.numeric(tapply(secs * computer, ids, sum)[ids[first]]),
        label = paste0(title, "seconds of activities that used computers")
      )
      out
    },
    files,
    names(files)
  )
  # 20 children have both an elementary and a middle-school diary.
  Reduce(
    function(a, b) {
      dplyr::full_join(a, b, by = key, relationship = "one-to-one")
    },
    parts
  )
}

#' The household-composition and PCG-employment columns carried into CDS-2020
bp46_household_columns <- function(df) {
  roster <- function(yy) {
    grep(
      paste0("^X", yy, "R[0-9]{2}(SEX|RTC|GDAD|GMOM|AUN1|AUN2|UNC1|UNC2)$"),
      names(df),
      value = TRUE
    )
  }
  # The PCG's relationship to the child travels with the work-for-pay answer.
  list(
    work = list(
      cols = list(
        "2019" = c("H19S32", "X19PCGRLSP"),
        "2021" = c("H21S32", "X21PCGRLSP")
      ),
      date = "H"
    ),
    household = list(
      cols = list("2019" = roster("19"), "2021" = roster("21")),
      date = "X"
    )
  )
}

#' Copy values onto CDS-2020 rows from the child's interview 12 months away
bp46_carry_to_2020 <- function(df, blocks) {
  # CDS-2019 ended in May 2020 and CDS-2021 began in November 2021, so only
  # CDS-2020 rows can be 12 months or less from another wave's interview.
  ym <- function(prefix) {
    12 * df[[paste0(prefix, "IWYR")]] + df[[paste0(prefix, "IWMO")]]
  }
  target <- which(df$.wave == "2020")
  target_month <- ym("H20")[target]

  for (block in blocks) {
    month <- rep(NA_real_, nrow(df))
    donor <- rep(FALSE, nrow(df))
    for (w in names(block$cols)) {
      rows <- df$.wave == w
      month[rows] <- ym(paste0(block$date, substr(w, 3, 4)))[rows]
      first <- block$cols[[w]][[1]]
      donor[rows] <- !is.na(df[[first]][rows])
    }
    pairs <- merge(
      data.frame(
        target = target,
        month = target_month,
        id = df$participant_id[target]
      ),
      data.frame(donor = which(donor), id = df$participant_id[donor])
    )
    pairs$gap <- abs(month[pairs$donor] - pairs$month)
    pairs <- pairs[pairs$gap <= 12, ]
    # The nearest interview wins; a tie goes to the earlier one.
    pairs <- pairs[order(pairs$target, pairs$gap, month[pairs$donor]), ]
    pairs <- pairs[!duplicated(pairs$target), ]

    for (w in names(block$cols)) {
      from <- pairs[df$.wave[pairs$donor] == w, ]
      for (col in block$cols[[w]]) {
        x <- df[[col]]
        x[from$target] <- x[from$donor]
        attr(x, "label") <- paste0(
          attr(x, "label"),
          " [CDS-2020 rows: carried from this interview when 12 months or",
          " less before or after the CDS-2020 PCG interview]"
        )
        df[[col]] <- x
      }
    }
  }
  df
}

#' Ever repeated a grade, with a yes carried to every later wave attended
bp46_repeat_grade <- function(df, waves) {
  items <- c(
    "1997" = "Q1G20",
    "2002" = "Q21B27",
    "2007" = "Q31B27",
    "2014" = "P14F24",
    "2019" = "P19F24",
    "2021" = "P21F24"
  )
  x <- rep(NA_real_, nrow(df))
  for (w in names(items)) {
    rows <- df$.wave == w
    x[rows] <- df[[items[[w]]]][rows]
  }
  order <- match(df$.wave, waves)
  yes <- !is.na(x) & x == 1
  first_yes <- tapply(order[yes], df$participant_id[yes], min)
  later <- order > first_yes[df$participant_id]
  x[!is.na(later) & later] <- 1
  attr(x, "label") <- paste0(
    "Since starting kindergarten, ever repeated a grade or held back (PCG;",
    " 1 yes, 5 no, 8 DK, 9 NA, 0 inap.): ",
    paste(items, collapse = ", "),
    " in their own waves; a yes is carried to every later wave attended"
  )
  df$repeat_grade_ever <- x
  df
}

#' One family per CDS-2014-generation child, linking 2013 and 2019 families
bp46_family_unit <- function(df) {
  rows <- which(df$.wave %in% c("2014", "2019"))
  family <- ifelse(
    df$.wave[rows] == "2014",
    paste0("2013_", sprintf("%.0f", df$X14YRID[rows])),
    paste0("2019_", sprintf("%.0f", df$X19YRID[rows]))
  )
  child <- df$participant_id[rows]
  # Families that share a child form one group, named by its first id in sort
  # order, a 2013 id wherever the group has one.
  unit <- family
  repeat {
    linked <- stats::ave(stats::ave(unit, child, FUN = min), family, FUN = min)
    if (identical(linked, unit)) {
      break
    }
    unit <- linked
  }
  df$family_unit <- unit[match(df$participant_id, child)]
  attr(df$family_unit, "label") <- paste(
    "PSID family of a CDS-2014-generation child: 2013 (X14YRID) and 2019",
    "(X19YRID) family interview numbers, prefixed by year, with families that",
    "share a child linked into one group named by its first id in sort order"
  )
  df
}
