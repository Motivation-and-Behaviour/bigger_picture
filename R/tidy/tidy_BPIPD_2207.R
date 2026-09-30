#' Tidier for BPIPD-2207 (Korean Media Panel Survey)
#'
#' Each survey year's person file is joined to its household file and to its
#' three-day media diary, with the year stripped from column names, then stacked.
#' Labels come from KISDI's codebooks (Korean), which the CSVs do not carry.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per respondent per survey year (`pid`, `.wave`), the
#'   diary's day-level columns averaged over its weekday (`_wd`) and weekend
#'   (`_we`) days
tidy_BPIPD_2207 <- function(raw_dataset, spec) {
  waves <- vapply(spec$waves, function(w) as.character(w$wave), character(1))
  df <- dplyr::bind_rows(lapply(waves, function(w) {
    bp2207_tidy_wave(raw_dataset, w)
  }))
  df <- bp2207_parents(df)

  # The survey is annual (user guide p.6); the diary's first day dates the interview.
  diary_month <- ifelse(df$d_mm1st %in% 1:12, df$d_mm1st, NA)
  interview_month <- 12 * (2000 + as.integer(df$.wave)) + diary_month
  carried <- c("h_h_income1", "h_h_income2", "h_fly_typ", "parent_figures")
  for (col in carried) {
    df[[col]] <- bp2207_carry_gap(df[[col]], df$pid, interview_month)
  }

  # `bind_rows()` drops the labels, so they go on after it.
  df <- bp2207_label(df, raw_dataset$codebook)
  for (col in carried) {
    attr(df[[col]], "label") <- paste0(
      attr(df[[col]], "label"),
      "; missing years carried within `pid` from the nearest year interviewed",
      " 12 months or less away"
    )
  }

  if (anyDuplicated(df[c("pid", ".wave")]) > 0) {
    stop(
      "BPIPD-2207: `pid` and `.wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' Columns that already carry the same name in every year's file
bp2207_link_pattern <- "^(p[0-9]{2}pid|h[0-9]{2}hid|[phd][0-9]{2}ans|KMPS[0-9]{2})$"

#' Person columns that are free text in some years and numeric in others
bp2207_text_columns <- c(
  "p_school9",
  "p_school13",
  "p_school14",
  "p_school17",
  "p_a01030",
  "p_a01031"
)

#' Diary place and media slot columns, dropped for KISDI's per-day summaries
bp2207_slot_pattern <- "^d_(p|[MAC][A-I])[0-9]+$"

#' Diary columns that differ between a respondent's three diary days
bp2207_diary_day_pattern <- paste0(
  "^d_(m1|mm1st|dd1st|day1|weekend|spday1|diary[0-9]+|s[0-9]+|edu_time|sleep)$",
  "|^d_[MAC]_(fre|time|user|edutime|gametime)[0-9]+$"
)

#' Rename a file's own columns, `p24gender` to `p_gender`
bp2207_strip_year <- function(tbl, prefix, wave) {
  own <- startsWith(names(tbl), paste0(prefix, wave)) &
    !grepl(bp2207_link_pattern, names(tbl))
  names(tbl)[own] <- sub(
    paste0("^", prefix, wave),
    paste0(prefix, "_"),
    names(tbl)[own]
  )
  tbl
}

#' Add a table's new columns to the person spine
bp2207_add <- function(base, tbl, by, relationship) {
  tbl <- tbl[c(by, setdiff(names(tbl), names(base)))]
  dplyr::left_join(base, tbl, by = by, relationship = relationship)
}

#' Fill missing values within participant from the nearest year 12 months or less away
bp2207_carry_gap <- function(x, id, month, max_gap = 12) {
  observed <- x
  person <- match(id, unique(id))
  donors <- split(seq_along(x), person)
  for (i in which(is.na(x) & !is.na(month))) {
    donor <- donors[[person[i]]]
    donor <- donor[!is.na(observed[donor]) & !is.na(month[donor])]
    gap <- abs(month[donor] - month[i])
    if (any(gap <= max_gap)) {
      x[i] <- observed[donor][which.min(gap)]
    }
  }
  x
}

#' Minutes per code of one diary family within the slots `keep` selects
#'
#' KISDI's `*_time` summaries count 15 minutes per slot and stream carrying the
#' code; this counts the same where `keep(stream letter)` is TRUE.
bp2207_code_minutes <- function(tbl, family, keep, measure) {
  summary <- grep(
    paste0("^d_", family, "_time[0-9]+$"),
    names(tbl),
    value = TRUE
  )
  codes <- as.integer(sub(paste0("^d_", family, "_time"), "", summary))
  streams <- grep(paste0("^d_", family, "[A-I]1$"), names(tbl), value = TRUE)
  counts <- integer(nrow(tbl) * length(codes))
  for (stream in sub("1$", "", streams)) {
    slots <- as.matrix(tbl[paste0(stream, 1:96)])
    kept <- keep(substring(stream, nchar(stream)))
    code <- match(slots[kept], codes)
    row <- row(slots)[kept]
    hit <- !is.na(code)
    counts <- counts +
      tabulate(row[hit] + (code[hit] - 1L) * nrow(tbl), length(counts))
  }
  counts <- matrix(15 * counts, nrow(tbl))
  stats::setNames(
    lapply(seq_along(codes), function(j) counts[, j]),
    paste0("d_", family, "_", measure, codes)
  )
}

#' Per-day minutes at an education facility, and media minutes spent gaming
#'
#' The at-school and device-gaming rows cross the place, media and activity
#' slots, which are dropped; each stream records one medium with its activity.
bp2207_slot_minutes <- function(tbl) {
  at_edu <- as.matrix(tbl[paste0("d_p", 1:96)]) == 4
  edu <- function(stream) at_edu
  gaming <- function(stream) as.matrix(tbl[paste0("d_A", stream, 1:96)]) == 23
  out <- c(
    list(d_edu_time = 15 * rowSums(at_edu)),
    bp2207_code_minutes(tbl, "M", edu, "edutime"),
    bp2207_code_minutes(tbl, "A", edu, "edutime"),
    bp2207_code_minutes(tbl, "C", edu, "edutime"),
    bp2207_code_minutes(tbl, "M", gaming, "gametime")
  )
  # The 2010 diary has no sleep slots.
  if ("d_s1" %in% names(tbl)) {
    out$d_sleep <- 15 * rowSums(as.matrix(tbl[paste0("d_s", 1:96)]) == 1)
  }
  tibble::as_tibble(out)
}

#' Mean of `cols` over each respondent's diary days where `keep` is TRUE
bp2207_day_mean <- function(tbl, cols, keep, suffix) {
  keep <- keep %in% TRUE
  ids <- unique(tbl$pid[keep])
  group <- match(tbl$pid[keep], ids)
  sums <- rowsum(as.matrix(tbl[keep, cols]), group, reorder = TRUE)
  out <- tibble::as_tibble(sums / tabulate(group, length(ids)))
  names(out) <- paste0(cols, suffix)
  tibble::add_column(out, pid = ids, .before = 1)
}

#' One row per respondent from a year's diary file, its days averaged by type
#'
#' Weekday (`_wd`) and weekend (`_we`) means follow each day's weekend flag
#' (1 weekday, 2 weekend), so unwritten days (888) drop out; the at-school
#' columns average only the weekday days with time at an education facility.
bp2207_diary <- function(tbl, wave) {
  tbl <- bp2207_strip_year(tibble::as_tibble(tbl), "d", wave)
  tbl <- dplyr::bind_cols(tbl, bp2207_slot_minutes(tbl))
  tbl <- tbl[!grepl(bp2207_slot_pattern, names(tbl))]

  weekday <- tbl$d_weekend %in% 1
  weekend <- tbl$d_weekend %in% 2
  school_day <- weekday & tbl$d_edu_time > 0
  tbl$d_days_wd <- stats::ave(as.integer(weekday), tbl$pid, FUN = sum)
  tbl$d_days_we <- stats::ave(as.integer(weekend), tbl$pid, FUN = sum)
  tbl$d_edu_days_wd <- stats::ave(as.integer(school_day), tbl$pid, FUN = sum)

  daily <- grep(bp2207_diary_day_pattern, names(tbl), value = TRUE)
  out <- dplyr::distinct(tbl[setdiff(names(tbl), daily)])
  # The first diary day dates the interview.
  first <- tbl[tbl$d_m1 == 1, c("pid", "d_mm1st", "d_dd1st")]

  summaries <- grep(
    "^d_([MAC]_(fre|time|user|gametime)[0-9]+|sleep)$",
    names(tbl),
    value = TRUE
  )
  school <- grep("^d_([MAC]_edutime[0-9]+|edu_time)$", names(tbl), value = TRUE)
  for (part in list(
    first,
    bp2207_day_mean(tbl, summaries, weekday, "_wd"),
    bp2207_day_mean(tbl, summaries, weekend, "_we"),
    bp2207_day_mean(tbl, school, school_day, "_wd")
  )) {
    out <- dplyr::left_join(out, part, by = "pid", relationship = "one-to-one")
  }
  out
}

#' Variable descriptions from every codebook sheet but the guide and frequencies
#'
#' Section sheets give the name in column 1 and the description in column 2;
#' the diary's computed-variable sheet names three variables before its
#' description, so each name takes the first following cell that is not a name.
bp2207_codebook_labels <- function(codebook) {
  files <- unlist(codebook[c(
    "kmp_person_codebook",
    "kmp_household_codebook",
    "kmp_diary_codebook"
  )])
  pairs <- lapply(files, function(file) {
    sheets <- readxl::excel_sheets(file)[-1]
    sheets <- sheets[!grepl("FREQ", sheets)]
    lapply(sheets, function(sheet) {
      x <- readxl::read_excel(
        file,
        sheet = sheet,
        col_names = FALSE,
        col_types = "text",
        .name_repair = "minimal"
      )
      x <- as.matrix(x[seq_len(min(ncol(x), 4))])
      is_name <- !is.na(x) & grepl("^[phd]__", x)
      rows <- lapply(seq_len(ncol(x) - 1), function(j) {
        desc <- rep(NA_character_, nrow(x))
        for (k in rev(seq(j + 1, ncol(x)))) {
          desc <- ifelse(is_name[, k], desc, x[, k])
        }
        keep <- is_name[, j] & !is.na(desc)
        data.frame(name = x[keep, j], desc = desc[keep])
      })
      do.call(rbind, rows)
    })
  })
  pairs <- do.call(rbind, unlist(pairs, recursive = FALSE))
  pairs <- pairs[!duplicated(pairs$name), ]
  stats::setNames(pairs$desc, sub("^([phd])__", "\\1_", pairs$name))
}

#' Label every column the codebooks describe, and the columns built here
bp2207_label <- function(tbl, codebook) {
  desc <- bp2207_codebook_labels(codebook)
  nm <- names(tbl)
  diary <- grepl("^d_.*_(wd|we)$", nm)
  base <- ifelse(diary, sub("_(wd|we)$", "", nm), nm)
  day <- ifelse(
    diary,
    ifelse(
      endsWith(nm, "_wd"),
      "Mean over weekday diary days: ",
      "Mean over weekend diary days: "
    ),
    ""
  )
  day[grepl("^d_([MAC]_edutime[0-9]+|edu_time)_wd$", nm)] <-
    "Mean over weekday diary days with time at an education facility: "
  label <- ifelse(base %in% names(desc), paste0(day, desc[base]), NA)

  # Per-day summaries: the codebook's text names the code, not the measure.
  parts <- regmatches(
    base,
    regexec("^d_([MAC])_(fre|time|user|edutime|gametime)([0-9]+)$", base)
  )
  hit <- lengths(parts) == 4L
  family <- c(M = "media", A = "activity", C = "connection")
  measure <- c(
    fre = "episodes",
    time = "minutes",
    user = "share of days used",
    edutime = "minutes at an education facility (place code 4)",
    gametime = "minutes gaming (activity code 23)"
  )
  label[hit] <- vapply(
    which(hit),
    function(i) {
      p <- parts[[i]]
      text <- desc[paste0("d_", p[[2]], "_time", p[[4]])]
      text <- sub("\\s*\\(빈도[^()]*\\)$", "", text)
      paste0(
        day[[i]],
        measure[[p[[3]]]],
        ", ",
        family[[p[[2]]]],
        " code ",
        p[[4]],
        if (is.na(text)) "" else paste0(" (", text, ")")
      )
    },
    character(1)
  )

  built <- c(
    d_sleep = "minutes asleep, 15 per slot with the sleep flag (1 asleep), midnight to midnight",
    d_edu_time = "minutes at an education facility (place code 4)"
  )
  hit <- base %in% names(built)
  label[hit] <- paste0(day[hit], built[base[hit]])
  counts <- c(
    d_days_wd = "Number of diary days flagged as weekdays (weekend flag 1)",
    d_days_we = "Number of diary days flagged as weekend days (weekend flag 2)",
    d_edu_days_wd = "Number of weekday diary days with any time at an education facility (place code 4)",
    d_mm1st = paste(desc[["d_mm1st"]], "(first diary day)"),
    d_dd1st = paste(desc[["d_dd1st"]], "(first diary day)")
  )
  label[nm %in% names(counts)] <- counts[nm[nm %in% names(counts)]]

  hit <- which(!is.na(label))
  tbl[hit] <- Map(
    function(x, l) {
      attr(x, "label") <- l
      x
    },
    tbl[hit],
    label[hit]
  )
  tbl
}

#' Parents' own education and the number of parent figures in the household
#'
#' The roster, indexed by `housenum`, gives each member's relation to the head.
#' A child of the head (code 3) has the head and spouse (1, 2) as parent figures
#' and a grandchild (5) the head's child and child-in-law (3, 4); a mother or
#' father is taken only when one member of that sex qualifies.
bp2207_parents <- function(df) {
  n <- nrow(df)
  rel <- as.matrix(df[paste0("h_m", 1:10, "_rel")])
  sex <- as.matrix(df[paste0("h_m", 1:10, "_gen")])
  figure <- (df$p_rel %in% 3 & matrix(rel %in% 1:2, n)) |
    (df$p_rel %in% 5 & matrix(rel %in% 3:4, n))
  mothers <- figure & matrix(sex %in% 2, n)
  fathers <- figure & matrix(sex %in% 1, n)

  known <- df$p_rel %in%
    c(3, 5) &
    rowSums(!is.na(rel)) > 0 &
    rowSums(mothers) <= 1 &
    rowSums(fathers) <= 1
  df$parent_figures <- ifelse(known, rowSums(figure), NA)
  attr(df$parent_figures, "label") <- paste(
    "Resident parent figures in the household roster (0-2): head and spouse",
    "for a child of the head, head's child and child-in-law for a grandchild"
  )

  person <- paste(df$hid, df$.wave, df$housenum)
  for (parent in c("mother", "father")) {
    m <- if (parent == "mother") mothers else fathers
    member <- ifelse(rowSums(m) == 1, max.col(m, ties.method = "first"), NA)
    from <- match(paste(df$hid, df$.wave, member), person)
    from[is.na(member) | is.na(df$p_school[from])] <- NA
    # Parent education is always carried: the level and completion travel together.
    from <- carry_within(from, df$pid, as.integer(df$.wave))
    rule <- "from the parent figure's own person row; years without one carry the earliest year with a value"
    df[[paste0(parent, "_school")]] <- df$p_school[from]
    attr(df[[paste0(parent, "_school")]], "label") <- paste(
      parent,
      "figure's highest education level (`p_school`),",
      rule
    )
    df[[paste0(parent, "_school2")]] <- df$p_school2[from]
    attr(df[[paste0(parent, "_school2")]], "label") <- paste(
      parent,
      "figure's completion status at that level (`p_school2`),",
      rule
    )
  }
  df
}

#' One survey year: the person file with the household and diary columns added
bp2207_tidy_wave <- function(raw_dataset, wave) {
  fetch <- function(kind) {
    raw_dataset$data[[paste0("kmp_wave", wave, "_", kind, "_data")]]
  }

  person <- bp2207_strip_year(tibble::as_tibble(fetch("person")), "p", wave)
  text <- intersect(bp2207_text_columns, names(person))
  person[text] <- lapply(person[text], as.character)

  household <- bp2207_strip_year(
    tibble::as_tibble(fetch("household")),
    "h",
    wave
  )

  # Each year a few respondents carry an `hid` the household file has no row for.
  person <- bp2207_add(
    person,
    household,
    by = "hid",
    relationship = "many-to-one"
  )

  bp2207_add(
    person,
    bp2207_diary(fetch("device"), wave),
    by = "pid",
    relationship = "one-to-one"
  )
}
