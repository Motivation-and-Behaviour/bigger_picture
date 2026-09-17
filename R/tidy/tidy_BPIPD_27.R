#' Tidier for BPIPD-27 (Millennium Cohort Study)
#'
#' One SPSS file per module per sweep:
#'
#' - cohort-member modules (`mcs<n>_cm_*`) are keyed on family plus
#'   cohort-member number and form the spine;
#' - `hhgrid` holds one row per household member, and is read at both of the
#'   grains it contains: the cohort member's own row joins onto the spine, and
#'   the rest of the roster supplies each responding parent's sex;
#' - family modules are keyed on family alone and are broadcast to every
#'   cohort member in the family (MCS families can hold twins or triplets);
#' - parent modules hold one row per responding parent, so they are reduced to
#'   one row per family before joining. At the earlier sweeps the main
#'   respondent is also the person who reports the child's screen use.
#'
#' Variables keep their original MCS spelling. Every column has a one-letter
#' sweep prefix (`B` = sweep 2 ... `G` = sweep 7)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per cohort member per sweep
tidy_BPIPD_27 <- function(raw_dataset, spec) {
  sweeps <- lapply(spec$waves, function(wave) bp27_sweep(raw_dataset, wave))
  df <- dplyr::bind_rows(bp27_prefix_shared_columns(sweeps))

  attr(df$MCSID, "label") <-
    "MCS Research ID - Anonymised Family/Household Identifier"
  attr(df$mcs_cnum, "label") <- "Cohort Member number within an MCS family"
  attr(df$participant_id, "label") <-
    "MCSID and cohort member number, unique within a sweep"

  if (anyDuplicated(df[c("participant_id", ".wave")]) > 0) {
    stop(
      "BPIPD-27: `participant_id` and `.wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' Assemble one sweep into a cohort-member table
bp27_sweep <- function(raw_dataset, wave) {
  modules <- function(...) bp27_modules(raw_dataset, wave$wave, ...)

  cm <- bp27_cognitive_scores(
    modules(c(
      "cm_derived",
      "cm_interview",
      "cm_cognitive_assessment",
      "cm_teacher_survey",
      "cm_aspirations",
      "cm_oral_fluid"
    )),
    wave$wave
  )
  if (length(cm) == 0) {
    stop(
      "BPIPD-27: no cohort-member module for wave ",
      wave$wave,
      call. = FALSE
    )
  }

  base <- Reduce(
    function(base, tbl) {
      bp27_add(base, tbl, bp27_cm_key(tbl), dplyr::full_join, "one-to-one")
    },
    cm
  )
  key <- bp27_cm_key(base)

  grid <- modules("hhgrid")

  for (tbl in grid) {
    base <- bp27_add(
      base,
      bp27_hhgrid_cohort_members(tbl),
      key,
      dplyr::left_join,
      "one-to-one"
    )
  }

  # Family rows are shared by every cohort member in the family
  for (tbl in modules(
    c(
      "family_derived",
      "family_interview",
      "geographically_linked_data",
      "neighbourhood_observations"
    ),
    extra = "mcs_sweep[0-9]+_imd_[a-z]+_[0-9]+[.]sav$"
  )) {
    base <- bp27_add(base, tbl, "MCSID")
  }

  parent_cm <- modules("parent_cm_interview")
  answering <- bp27_answering_parent(parent_cm)

  # `parent_derived` carries the parental education variables
  roster_sex <- bp27_person_sex(grid)
  for (tbl in modules("parent_derived")) {
    if (!is.null(roster_sex)) {
      tbl <- bp27_add(tbl, roster_sex, c("MCSID", bp27_person_key(tbl)))
    }
    base <- bp27_add(
      base,
      bp27_respondent(tbl, 1L, "MCSID", answering = answering),
      "MCSID"
    )
    base <- bp27_add(
      base,
      bp27_respondent(
        tbl,
        2L,
        "MCSID",
        suffix = "_PARTNER",
        answering = answering
      ),
      "MCSID"
    )
  }

  for (tbl in parent_cm) {
    base <- bp27_add(
      base,
      bp27_respondent(tbl, 1L, key, answering = answering),
      key,
      dplyr::left_join,
      "one-to-one"
    )
  }

  # Sweep 6's time-use diary is one row per 10-minute slot per diary day
  for (tbl in modules("cm_tud_harmonised")) {
    base <- bp27_add(
      base,
      bp27_tud_minutes(tbl),
      key,
      dplyr::left_join,
      "one-to-one"
    )
  }

  base$mcs_cnum <- as.integer(base[[key[2]]])
  base$participant_id <- paste0(base$MCSID, "-", base$mcs_cnum)
  base$.wave <- as.character(wave$wave)
  base$.wave_label <- as.character(wave$label %||% wave$wave)

  base
}

#' Screen-use minutes per diary day type from the sweep-6 time-use diary
bp27_tud_minutes <- function(tbl) {
  activities <- c(
    tv = 38, # Watch TV, DVDs, downloaded videos
    game = 37, # Playing electronic games and Apps
    socmedia = 34, # Browsing and updating social networking sites
    internet = 35, # General internet browsing, programming
    messaging = 33, # Answering emails, instant messaging, texting
    calls = 31 # Speaking on the phone (including Skype, video calls)
  )
  key <- bp27_cm_key(tbl)
  needed <- c(key, "FCTUDAD", "FCTUDWEEKDAY", "FCTUDACT")
  if (!all(needed %in% names(tbl))) {
    stop(
      "BPIPD-27: the time-use diary lacks ",
      paste(setdiff(needed, names(tbl)), collapse = ", "),
      call. = FALSE
    )
  }

  slots <- tibble::tibble(
    MCSID = tbl$MCSID,
    cnum = bp27_plain(tbl[[key[2]]]),
    day = bp27_plain(tbl$FCTUDAD),
    # FCTUDWEEKDAY: 1 = Sunday ... 7 = Saturday
    weekend = bp27_plain(tbl$FCTUDWEEKDAY) %in% c(1, 7),
    activity = bp27_plain(tbl$FCTUDACT)
  )

  per_day <- dplyr::summarise(
    slots,
    weekend = weekend[[1]],
    n_slots = dplyr::n(),
    missing_slots = sum(is.na(activity)),
    .by = c("MCSID", "cnum", "day")
  )
  if (any(per_day$n_slots != 144L)) {
    stop("BPIPD-27: a diary day does not have 144 slots.", call. = FALSE)
  }
  slot_day <- paste(slots$MCSID, slots$cnum, slots$day)
  child_day <- paste(per_day$MCSID, per_day$cnum, per_day$day)
  for (nm in names(activities)) {
    minutes <- 10 *
      tapply(slots$activity == activities[[nm]], slot_day, sum, na.rm = TRUE)
    per_day[[nm]] <- as.numeric(minutes[child_day])
  }

  # A child with two days of one type (not the case in this release) gets
  # the mean of the two.
  per_type <- dplyr::summarise(
    per_day,
    dplyr::across(
      dplyr::all_of(c("missing_slots", names(activities))),
      ~ mean(.x)
    ),
    .by = c("MCSID", "cnum", "weekend")
  )
  measures <- c(names(activities), "missing_slots")
  suffix <- function(df, tag) {
    df <- df[df$weekend == identical(tag, "we"), c("MCSID", "cnum", measures)]
    names(df)[match(measures, names(df))] <- paste0(
      "tud_",
      measures,
      "_",
      ifelse(measures == "missing_slots", "", "mins_"),
      tag
    )
    df
  }
  out <- dplyr::full_join(
    suffix(per_type, "wd"),
    suffix(per_type, "we"),
    by = c("MCSID", "cnum"),
    relationship = "one-to-one"
  )
  names(out)[names(out) == "cnum"] <- key[2]

  labels <- c(
    tv = "watching TV, DVDs or downloaded videos",
    game = "playing electronic games and apps",
    socmedia = "browsing and updating social networking sites",
    internet = "general internet browsing (not social networking)",
    messaging = "answering emails, instant messaging, texting",
    calls = "speaking on the phone, including Skype and video calls",
    missing_slots = "10-minute slots with no activity recorded"
  )
  for (nm in names(out)) {
    m <- regmatches(nm, regexec("^tud_([a-z_]+?)(_mins)?_(wd|we)$", nm))[[1]]
    if (length(m) == 0) {
      next
    }
    day_type <- if (m[[4]] == "we") "weekend" else "weekday"
    attr(out[[nm]], "label") <- paste0(
      "Time-use diary (sweep 6): ",
      if (m[[2]] == "missing_slots") "" else "minutes ",
      labels[[m[[2]]]],
      " on the ",
      day_type,
      " diary day"
    )
  }
  out
}

#' Give sweep-specific names to columns MCS left unprefixed
bp27_prefix_shared_columns <- function(sweeps) {
  # Columns the tidier itself puts in every sweep are shared on purpose.
  constructed <- c(
    "MCSID",
    "participant_id",
    "mcs_cnum",
    "respondent_sex",
    "respondent_sex_PARTNER",
    ".wave",
    ".wave_label"
  )
  counts <- table(unlist(lapply(sweeps, names)))
  shared <- setdiff(names(counts)[counts > 1], constructed)

  lapply(sweeps, function(tbl) {
    rename <- intersect(names(tbl), shared)
    if (length(rename) == 0) {
      return(tbl)
    }

    prefixed <- paste0(substr(bp27_cm_key(tbl)[2], 1, 1), rename)
    if (any(prefixed %in% names(tbl))) {
      stop(
        "BPIPD-27: prefixing shared columns would overwrite ",
        paste(intersect(prefixed, names(tbl)), collapse = ", "),
        call. = FALSE
      )
    }

    names(tbl)[match(rename, names(tbl))] <- prefixed
    tbl
  })
}

#' A wave's tables, selected by module name
bp27_modules <- function(raw_dataset, wave, modules = NULL, extra = NULL) {
  tables <- raw_dataset$data
  in_wave <- vapply(
    tables,
    function(tbl) identical(as.character(tbl$.wave[1]), wave),
    logical(1)
  )
  tables <- tables[in_wave]

  patterns <- c(
    paste0("mcs[0-9]+_", modules, "[.]sav$"),
    extra
  )
  selected <- unlist(
    lapply(patterns, function(pattern) tables[grepl(pattern, names(tables))]),
    recursive = FALSE
  )

  lapply(selected, function(tbl) {
    tbl <- tibble::as_tibble(tbl)
    names(tbl)[toupper(names(tbl)) == "MCSID"] <- "MCSID"

    ids <- grep("^MCSID$|^.CNUM00$|^.PNUM00$", names(tbl))
    tbl[ids] <- lapply(tbl[ids], bp27_plain)

    tbl[, setdiff(names(tbl), c(".wave", ".wave_label"))]
  })
}

#' Keep the cohort members' own rows from a household grid
bp27_hhgrid_cohort_members <- function(tbl) {
  cnum <- tbl[[bp27_cm_key(tbl)[2]]]
  roles <- grepl("^.(PNUM00|ELIG00|RESP00)$", names(tbl))
  tbl[!is.na(cnum) & cnum >= 1, !roles]
}

#' Each household member's sex, from the rest of the household grid
bp27_person_sex <- function(grids) {
  if (length(grids) == 0) {
    return(NULL)
  }

  tbl <- grids[[1]]
  cnum <- tbl[[bp27_cm_key(tbl)[2]]]
  sex <- grep("PSEX", names(tbl), value = TRUE)
  if (length(sex) != 1) {
    stop(
      "BPIPD-27: expected one household-grid sex column, found ",
      length(sex),
      ".",
      call. = FALSE
    )
  }

  roster <- tbl[is.na(cnum) | cnum < 1, c("MCSID", bp27_person_key(tbl), sex)]
  roster[[sex]] <- haven::labelled(
    bp27_plain(roster[[sex]]),
    c(Male = 1L, Female = 2L),
    label = "Sex of the responding parent, from the household grid"
  )
  names(roster)[3] <- "respondent_sex"
  roster
}

#' The `*PNUM00` person-number column for a table, e.g. `"BPNUM00"`
bp27_person_key <- function(tbl) {
  pnum <- grep("^.PNUM00$", names(tbl), value = TRUE)
  if (length(pnum) == 0) {
    stop("BPIPD-27: no `*PNUM00` column to key on.", call. = FALSE)
  }

  pnum[1]
}

#' Strip an identifier or flag column back to a plain vector
bp27_plain <- function(x) {
  x <- unclass(x)
  attributes(x) <- NULL
  if (is.character(x)) x else as.integer(x)
}

#' Drop item-level responses from the cognitive assessment
bp27_cognitive_scores <- function(tables, wave) {
  keep <- list(
    # Bracken School Readiness and BAS Naming Vocabulary derived scores.
    sweep_2 = "^.D",
    # BAS ability scores, T-scores and test raw totals.
    sweep_3 = "ABIL$|TSCORE$|SCO00$",
    # BAS ability and standard scores, and section/test raw totals.
    sweep_4 = "AB00$|SD00$|SC00$|SCO00$"
  )[[wave]]

  if (is.null(keep)) {
    return(tables)
  }

  cognitive <- grepl("cm_cognitive_assessment", names(tables))
  tables[cognitive] <- lapply(tables[cognitive], function(tbl) {
    tbl[, grepl(keep, names(tbl)) | names(tbl) %in% bp27_cm_key(tbl)]
  })

  tables
}

#' The family plus cohort-member key for a table, e.g. `c("MCSID", "FCNUM00")`
bp27_cm_key <- function(tbl) {
  cnum <- grep("^.CNUM00$", names(tbl), value = TRUE)
  if (length(cnum) == 0) {
    stop("BPIPD-27: no `*CNUM00` column to key on.", call. = FALSE)
  }

  c("MCSID", cnum[1])
}

#' The parent who answered the questions about the child, as `MCSID` plus
#' person-number keys
bp27_answering_parent <- function(tables) {
  flagged <- Filter(
    function(tbl) any(grepl("_OUT_PARQUEST$", names(tbl))),
    tables
  )
  if (length(flagged) != 1) {
    return(character(0))
  }

  tbl <- flagged[[1]]
  outcome <- grep("_OUT_PARQUEST$", names(tbl), value = TRUE)[1]
  flag <- bp27_plain(tbl[[outcome]])
  pnum <- bp27_plain(tbl[[bp27_person_key(tbl)]])
  unique(paste(tbl$MCSID, pnum)[!is.na(flag) & flag %in% c(1L, 3L)])
}

#' Keep one respondent's rows from a parent-level table
bp27_respondent <- function(
  tbl,
  code,
  by,
  suffix = NULL,
  answering = character(0)
) {
  resp <- grep("RESP00$", names(tbl), value = TRUE)
  pnum <- grep("^.PNUM00$", names(tbl), value = TRUE)

  tbl <- if (length(resp) > 0) {
    flag <- bp27_plain(tbl[[resp[1]]])
    tbl[!is.na(flag) & flag == code, ]
  } else if (length(pnum) > 0) {
    person <- bp27_plain(tbl[[pnum[1]]])
    answered <- paste(tbl$MCSID, person) %in% answering
    ordered <- tbl[order(!answered, person), ]
    dplyr::slice(ordered, code, .by = dplyr::all_of(by))
  } else {
    stop(
      "BPIPD-27: no `*RESP00` or `*PNUM00` column to select a parent.",
      call. = FALSE
    )
  }

  if (!is.null(suffix)) {
    renamed <- setdiff(names(tbl), by)
    names(tbl)[match(renamed, names(tbl))] <- paste0(renamed, suffix)
  }

  tbl
}

#' Add a module's new columns to the spine
bp27_add <- function(
  base,
  tbl,
  by,
  join = dplyr::left_join,
  relationship = "many-to-one"
) {
  tbl <- tbl[, c(by, setdiff(names(tbl), names(base)))]
  join(base, tbl, by = by, relationship = relationship)
}
