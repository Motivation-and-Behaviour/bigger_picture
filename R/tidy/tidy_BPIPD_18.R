#' Tidier for BPIPD-18 (Longitudinal Study of Australian Children)
#'
#' Stacks the B and K cohort main wave files and the waves 2.5 and 3.5
#' mail-outs after removing each file's age indicator from its variable names
#' (Data User Guide 5.1 and 5.6), then adds the time-use diary summaries,
#' CogState tasks and NAPLAN results by `hicid`.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per study child per wave (`hicid`, `.wave`)
tidy_BPIPD_18 <- function(raw_dataset, spec) {
  data <- raw_dataset$data
  files <- bp18_files()
  naplan <- data$naplan[setdiff(names(data$naplan), "cohort")]

  # Each file is assembled on its own, so no join copies the stacked table.
  parts <- lapply(seq_len(nrow(files)), function(i) {
    tbl <- data[[files$resource[i]]]
    if (!is.na(files$cogstate[i])) {
      tbl <- bp18_add(tbl, data[[files$cogstate[i]]])
    }
    tbl <- bp18_strip(tbl, files$prefix[i], files$resource[i])

    # Five mail-out rows (lsacgrk9) hold nothing but `hicid`.
    items <- setdiff(names(tbl), c("hicid", ".wave", ".wave_label"))
    filled <- vapply(
      tbl[items],
      function(x) !is.na(x) & (!is.character(x) | nzchar(x)),
      logical(nrow(tbl))
    )
    tbl <- tbl[rowSums(filled) > 0L, , drop = FALSE]
    # 9C1 rows in which the study child took no part hold only parents' items.
    if (tbl$.wave[[1]] == "9C1") {
      tbl <- tbl[as.vector(unclass(tbl$childp)) != 0L, , drop = FALSE]
    }

    if (!is.na(files$diary[i])) {
      tbl <- bp18_add_diary(tbl, bp18_diary(data, files$diary[i]))
    }
    # NAPLAN holds every test year of the child, repeated on each wave.
    bp18_add(tbl, naplan)
  })
  df <- bp18_bind(parts)
  rm(parts)

  wave_order <- match(df$.wave, vapply(spec$waves, `[[`, "", "wave"))
  for (col in bp18_parent_education) {
    df[[col]] <- bp18_carry_education(df[[col]], df$hicid, wave_order)
  }
  # Stratum and selection postcode are fixed at selection (UG 5.6); only the
  # diary-only rows lack them.
  for (col in c("stratum", "pcodes")) {
    label <- attr(df[[col]], "label", exact = TRUE)
    df[[col]] <- carry_within(df[[col]], df$hicid, wave_order)
    attr(df[[col]], "label") <- paste0(
      label,
      " (carried within child to diary-only rows)"
    )
  }

  # The mail-outs record no age or date; they were sent in 2007 (wave 2.5) and
  # 2009 (wave 3.5) (UG 1.3), so age is taken at 1 July of that year.
  mailout <- df$.wave %in% c("2.5", "3.5")
  dob <- carry_within(df$zf04m1, df$hicid, wave_order)
  df$zf04m1[mailout] <- dob[mailout]
  attr(df$zf04m1, "label") <- paste0(
    attr(df$zf04m1, "label", exact = TRUE),
    " (carried within child to the mail-out rows)"
  )
  sent <- as.Date(ifelse(df$.wave == "2.5", "2007-07-01", "2009-07-01"))
  df$age_mailout <- ifelse(
    mailout,
    as.numeric(sent - df$zf04m1) / 365.25,
    NA_real_
  )
  attr(df$age_mailout, "label") <- paste(
    "Age in years at 1 July of the mail-out year (2007 at wave 2.5, 2009 at",
    "wave 3.5) from the carried DOB (derived in the tidier)"
  )
  df$graderepeated_carried <- bp18_graderepeated(df, wave_order)

  if (anyDuplicated(df[c("hicid", ".wave")]) > 0) {
    stop(
      "BPIPD-18: `hicid` and `.wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }
  df
}

#' Main and mail-out files in spec order, with each file's age indicator and
#' the CogState and diary files of the same children and wave
bp18_files <- function() {
  # K wave 2.5 (mailout_k7) and wave 9C2 (survey_b17, survey_k21) are left
  # out: their only screen items are frequency ratings, with no duration item.
  tibble::tribble(
    ~resource    , ~prefix , ~cogstate      , ~diary             ,
    "survey_b0"  , "a"     , NA             , "tud_diary_b0"     ,
    "survey_k4"  , "c"     , NA             , "tud_diary_k4"     ,
    "survey_b2"  , "b"     , NA             , "tud_diary_b2"     ,
    "survey_k6"  , "d"     , NA             , "tud_diary_k6"     ,
    "mailout_b3" , "b"     , NA             , NA                 ,
    "survey_b4"  , "c"     , NA             , "tud_diary_b4"     ,
    "survey_k8"  , "e"     , NA             , "tud_diary_k8"     ,
    "mailout_b5" , "c"     , NA             , NA                 ,
    "mailout_k9" , "e"     , NA             , NA                 ,
    "survey_b6"  , "d"     , NA             , NA                 ,
    "survey_k10" , "f"     , NA             , "tud_episodes_k10" ,
    "survey_b8"  , "e"     , NA             , NA                 ,
    "survey_k12" , "g"     , NA             , "tud_episodes_k12" ,
    "survey_b10" , "f"     , NA             , "tud_episodes_b10" ,
    "survey_k14" , "h"     , "cogstate_k14" , "tud_episodes_k14" ,
    "survey_b12" , "g"     , NA             , "tud_episodes_b12" ,
    "survey_k16" , "i"     , NA             , NA                 ,
    "survey_b14" , "h"     , "cogstate_b14" , "tud_episodes_b14" ,
    "survey_k18" , "j"     , NA             , NA                 ,
    "survey_b16" , "i1"    , NA             , NA                 ,
    "survey_k20" , "k1"    , NA             , NA                 ,
    "survey_b19" , "j2"    , NA             , NA                 ,
    "survey_k23" , "l"     , NA             , NA
  )
}

#' Stack the per-file tables one column at a time, which needs about half the
#' memory `dplyr::bind_rows()` needs at this width; labels are kept as there
bp18_bind <- function(parts) {
  cols <- unique(unlist(lapply(parts, names), use.names = FALSE))
  sizes <- vapply(parts, nrow, integer(1))
  out <- lapply(cols, function(col) {
    pieces <- lapply(parts, `[[`, col)
    present <- !vapply(pieces, is.null, logical(1))
    pieces[present] <- bp18_align_labels(pieces[present])
    ptype <- vctrs::vec_ptype_common(!!!pieces[present])
    pieces[!present] <- lapply(sizes[!present], vctrs::vec_init, x = ptype)
    x <- vctrs::vec_c(!!!pieces, .ptype = ptype)
    attr(x, "label") <- attr(pieces[present][[1]], "label", exact = TRUE)
    x
  })
  names(out) <- cols
  tibble::new_tibble(out, nrow = sum(sizes))
}

#' Give a code the first file's wording where later files only reword it, so
#' that a code whose meaning changes still warns when the files are stacked
bp18_align_labels <- function(pieces) {
  kept <- character()
  for (i in seq_along(pieces)) {
    labels <- attr(pieces[[i]], "labels", exact = TRUE)
    if (is.null(labels)) {
      next
    }
    codes <- as.character(as.vector(labels))
    known <- match(codes, names(kept))
    reworded <- !is.na(known) & names(labels) != kept[known]
    if (any(reworded)) {
      same <- reworded &
        bp18_label_key(names(labels)) == bp18_label_key(kept[known])
      names(labels)[same] <- kept[known[same]]
      attr(pieces[[i]], "labels") <- labels
    }
    new <- is.na(known) & !is.na(codes)
    kept[codes[new]] <- names(labels)[new]
  }
  pieces
}

#' A value label without its case, punctuation or the files' two rewordings
bp18_label_key <- function(x) {
  x <- gsub("per week", "a week", tolower(x), fixed = TRUE)
  x <- gsub("not yet started", "not started yet", x, fixed = TRUE)
  gsub("[^a-z0-9]", "", x)
}

#' Columns without an age indicator in every main file (UG 5.6)
bp18_unprefixed <- c(
  "hicid",
  "cohort",
  "wave",
  "stratum",
  "pcodes",
  ".wave",
  ".wave_label"
)

#' Items whose codes, type or question change between waves, kept apart under
#' a file suffix (`pc06a2_b10` from lsacgrb10)
bp18_split_codings <- c(
  "fd19b",
  "he06c2a",
  "he17c2a",
  "ho01c2",
  # The same name asks a different question in some files (variable labels).
  c("fd19a", "fd19c", "fd19d", "fd19e", "pe02b", "hs17c", "hs27i", "re22za"),
  c("he06c2b", "he17c2b", "he17c3a", "he17c3b"),
  paste0("fn11", c("a", "b", "f", "m"), "2l"),
  c("pc14t1f", "pc14t2a", "pc14t2b", "pc30", "pc32a1", "tp09a"),
  "pa14b9j",
  "pc06a2",
  "pc29c2",
  "pc81c3b",
  paste0("pw13", c("a", "b", "f", "m"), "2"),
  paste0("pw14", c("a", "b", "f", "m"), "2"),
  paste0("re22y4", letters[1:6])
)

#' Add a per-child table's columns that the base table does not already hold
bp18_add <- function(base, tbl) {
  tbl <- tbl[c("hicid", setdiff(names(tbl), names(base)))]
  dplyr::left_join(base, tbl, by = "hicid", relationship = "one-to-one")
}

#' Add a diary summary, keeping children with a diary but no wave record
bp18_add_diary <- function(tbl, diary) {
  out <- dplyr::full_join(tbl, diary, by = "hicid", relationship = "one-to-one")
  for (col in c("cohort", "wave", ".wave", ".wave_label")) {
    out[[col]][is.na(out[[col]])] <- tbl[[col]][[1]]
  }
  out
}

#' The diary summary of one diary file
bp18_diary <- function(data, resource) {
  # Two wave 3 diaries (diaryb4) carry no `hicid` and cannot be linked.
  tbl <- data[[resource]][!is.na(data[[resource]]$hicid), , drop = FALSE]
  diary <- if (resource %in% names(bp18_paper_diaries())) {
    bp18_paper_diary(tbl, bp18_paper_diaries()[[resource]])
  } else {
    bp18_episode_diary(
      tbl,
      bp18_episode_codes()[[bp18_episode_schemes()[[resource]]]]
    )
  }
  bp18_label_tud(diary)
}

#' Drop a file's age indicator from its item names and store whole numbers as
#' integers
bp18_strip <- function(tbl, prefix, resource) {
  nm <- names(tbl)
  stem <- substring(nm, nchar(prefix) + 1L)
  strip <- startsWith(nm, prefix) & !nm %in% bp18_unprefixed
  split <- strip & stem %in% bp18_split_codings
  nm[strip] <- stem[strip]
  nm[split] <- paste0(stem[split], "_", sub("^[a-z]+_", "", resource))
  names(tbl) <- nm
  tbl[] <- lapply(tbl, bp18_compact)
  for (col in intersect(names(tbl), bp18_character_columns)) {
    tbl[[col]] <- bp18_as_character(tbl[[col]])
  }
  tbl
}

#' A whole-number double as an integer, keeping its labels
bp18_compact <- function(x) {
  if (!is.double(x) || inherits(x, c("Date", "POSIXt", "difftime"))) {
    return(x)
  }
  values <- as.vector(unclass(x))
  labels <- attr(x, "labels", exact = TRUE)
  whole <- function(v) all(is.na(v) | (v == trunc(v) & abs(v) < 2e9))
  if (!whole(values) || !whole(as.numeric(labels))) {
    return(x)
  }
  out <- as.integer(values)
  if (!is.null(labels)) {
    out <- haven::labelled(
      out,
      labels = stats::setNames(as.integer(labels), names(labels))
    )
  }
  attr(out, "label") <- attr(x, "label", exact = TRUE)
  out
}

#' Postcode indicators, text in some files and numbers in others
bp18_character_columns <- c("pcodes", "pcoden", "pcodei")

#' A postcode indicator as text without padding
bp18_as_character <- function(x) {
  out <- trimws(as.character(as.vector(unclass(x))))
  attr(out, "label") <- attr(x, "label", exact = TRUE)
  out
}

#' Parents' education items carried within child (mother and father figures)
bp18_parent_education <- c(
  "fd08m1",
  "fd08m2a",
  "fd08m3a",
  "fd08m3b",
  "fd08f1",
  "fd08f2a",
  "fd08f3a",
  "fd08f3b"
)

#' Fill waves that did not ask an item from the child's earliest substantive
#' answer
bp18_carry_education <- function(x, id, order) {
  donor <- x
  donor[!is.na(donor) & donor < 0] <- NA
  filled <- carry_within(donor, id, order)
  missing <- is.na(x)
  x[missing] <- filled[missing]
  attr(x, "label") <- paste0(
    attr(x, "label", exact = TRUE),
    " (carried within child from the earliest wave with an answer where not asked)"
  )
  x
}

#' Ever repeated a grade at the wave, an observed TRUE carried to every later
#' wave of the child
bp18_graderepeated <- function(df, order) {
  code <- function(x) as.vector(unclass(x))
  # pc47a1 is the ever item (Parent 1, the young person at K wave 8 and B wave
  # 10); pc47a6 and pc47c6 ask about repeating since the last wave.
  observed <- dplyr::case_when(
    code(df$pc47a1) %in%
      1L |
      code(df$pc47a6) %in% 1L |
      code(df$pc47c6) %in% 1L ~ TRUE,
    code(df$pc47a1) %in% 2L ~ FALSE,
    .default = NA
  )
  first <- stats::ave(
    ifelse(observed %in% TRUE, order, Inf),
    df$hicid,
    FUN = min
  )
  out <- observed | order > first
  attr(out, "label") <- paste(
    "Ever repeated a grade: TRUE if pc47a1, pc47a6 or pc47c6 is yes, FALSE if",
    "pc47a1 is no, an observed TRUE carried within child to every later wave",
    "(built in the tidier)"
  )
  out
}

#' Waves 1-3 paper diaries: the study's minutes-per-day totals and the diary
#' day (1 = Monday ... 7 = Sunday)
bp18_paper_diaries <- function() {
  list(
    tud_diary_b0 = c(tv = "b1da11t", day = "b1d2"),
    tud_diary_k4 = c(tv = "k1da11t", comp = "k1da13t", day = "k1d2"),
    tud_diary_b2 = c(tv = "b2da12t", comp = "b2da14t", day = "dotw"),
    tud_diary_k6 = c(tv = "k2da11t", comp = "k2da13t", day = "dotw"),
    tud_diary_b4 = c(tv = "b3da12t", comp = "b3da14t", day = "dotw"),
    tud_diary_k8 = c(tv = "k3da11t", comp = "k3da13t", day = "dotw")
  )
}

#' Mean minutes per weekday and weekend diary day from a waves 1-3 diary file
bp18_paper_diary <- function(tbl, cols) {
  activities <- setdiff(names(cols), "day")
  day <- as.vector(unclass(tbl[[cols[["day"]]]]))
  days <- tibble::tibble(
    hicid = tbl$hicid,
    weekend = ifelse(is.na(day), NA, day %in% 6:7)
  )
  for (a in activities) {
    days[[paste0(a, "_any")]] <- as.numeric(tbl[[cols[[a]]]])
  }
  bp18_day_means(days, "Parent (paper 15-minute diary)")
}

#' Episode diary files and their activity coding scheme
bp18_episode_schemes <- function() {
  c(
    tud_episodes_k10 = "wave4",
    tud_episodes_k12 = "wave5",
    tud_episodes_b10 = "wave6",
    tud_episodes_k14 = "wave6",
    tud_episodes_b12 = "wave6",
    tud_episodes_b14 = "wave6"
  )
}

#' Screen activity codes of each episode coding scheme, grouped as the study
#' codes them
bp18_episode_codes <- function() {
  list(
    wave4 = list(
      tv = 67,
      games = c(63, 64, 65),
      homework_device = c(61, 62),
      browsing = 66,
      electronic_media = 60,
      texting_socnet = 84,
      videochat = 85,
      phone_mobile = 83,
      phone_landline = 82
    ),
    # Code 60 sits in the electronic-device block and matches 601 at wave 6.
    wave5 = list(
      tv = 62,
      games = 61,
      homework_device = 60,
      socnet = 63,
      browsing = 66,
      device_other = c(64, 65, 67, 68, 69),
      texting = 84,
      online_chat = 85,
      videochat = 83,
      phone_mobile = 82,
      phone_landline = 81
    ),
    wave6 = list(
      tv = 621,
      games = c(611, 612),
      homework_device = 601,
      socnet = 631,
      browsing = 661,
      device_other = c(641, 651, 671, 681, 691),
      texting = 841,
      online_chat = 851,
      videochat = 831,
      phone_mobile = 821,
      phone_landline = 811
    )
  )
}

#' Minutes per screen activity on the diary day of a waves 4-8 episode diary
bp18_episode_diary <- function(tbl, codes) {
  # Clock times as minutes since waking, so episodes after midnight follow on.
  since_wake <- function(x) {
    (as.numeric(x) %% 86400 / 60 - as.numeric(tbl$awake) %% 86400 / 60) %% 1440
  }
  ep <- tibble::tibble(
    hicid = tbl$hicid,
    loop = tbl$loop,
    start = since_wake(tbl$stime),
    bed = since_wake(tbl$bed),
    sleep = if ("sleep" %in% names(tbl)) since_wake(tbl$sleep) else NA_real_,
    day = as.vector(unclass(if ("day" %in% names(tbl)) tbl$day else tbl$dow)),
    main = haven::zap_labels(tbl$mainact)
  )
  concurrent <- as.matrix(haven::zap_labels(tbl[grep("^secact", names(tbl))]))
  ep$weekend <- ifelse(is.na(ep$day), NA, ep$day %in% 6:7)
  ep <- ep[order(ep$hicid, ep$loop), , drop = FALSE]
  concurrent <- concurrent[order(tbl$hicid, tbl$loop), , drop = FALSE]

  # An episode ends when the next begins; the last ends at bed time, or at
  # sleep time when it began after going to bed.
  last <- !duplicated(ep$hicid, fromLast = TRUE)
  end <- c(ep$start[-1], NA)
  day_end <- ifelse(ep$start < ep$bed, ep$bed, ep$sleep)
  end[last] <- day_end[last]
  minutes <- end - ep$start
  minutes[!is.na(minutes) & minutes <= 0] <- NA

  days <- dplyr::summarise(
    ep,
    weekend = weekend[[1]],
    .by = c("hicid")
  )
  for (a in names(codes)) {
    is_main <- ep$main %in% codes[[a]]
    is_any <- is_main |
      rowSums(matrix(concurrent %in% codes[[a]], nrow = nrow(concurrent))) > 0
    days[[paste0(a, "_main")]] <- as.numeric(tapply(
      minutes * is_main,
      ep$hicid,
      sum,
      na.rm = TRUE
    )[as.character(days$hicid)])
    days[[paste0(a, "_any")]] <- as.numeric(tapply(
      minutes * is_any,
      ep$hicid,
      sum,
      na.rm = TRUE
    )[as.character(days$hicid)])
  }
  bp18_day_means(days, "Study child (activity-episode diary)")
}

#' Person means over weekday and over weekend diary days, one row per child
bp18_day_means <- function(days, diarist) {
  measures <- setdiff(names(days), c("hicid", "weekend"))
  # Diaries whose day of the week is not recorded are left out.
  out <- dplyr::summarise(
    days[!is.na(days$weekend), , drop = FALSE],
    dplyr::across(dplyr::all_of(measures), ~ mean(.x)),
    n = dplyr::n(),
    .by = c("hicid", "weekend")
  )
  wide <- function(we) {
    part <- out[out$weekend == we, c("hicid", measures, "n")]
    tag <- if (we) "we" else "wd"
    names(part) <- c(
      "hicid",
      paste0("tud_", measures, "_mins_", tag),
      paste0("tud_days_", tag)
    )
    part
  }
  res <- dplyr::full_join(
    wide(FALSE),
    wide(TRUE),
    by = "hicid",
    relationship = "one-to-one"
  )
  res$tud_days_wd[is.na(res$tud_days_wd)] <- 0L
  res$tud_days_we[is.na(res$tud_days_we)] <- 0L
  res$tud_diarist <- diarist
  res
}

#' Variable labels for the diary summary columns
bp18_label_tud <- function(tud) {
  activity <- c(
    tv = "watching TV, programs, DVDs, movies or videos",
    comp = "using a computer or computer games",
    games = "playing electronic or computer games",
    homework_device = "homework on a computer or electronic device",
    socnet = "on social networking sites",
    browsing = "internet browsing or internet use not elsewhere coded",
    device_other = "other electronic device use (downloading or posting media, internet shopping, websites, applications, nec)",
    electronic_media = "electronic media, games or computer use (wave 4 umbrella code 60)",
    texting_socnet = "texting, email or social networking (wave 4 combined code 84)",
    texting = "texting or emailing",
    online_chat = "online chatting or instant messaging",
    videochat = "video chatting",
    phone_mobile = "talking on a mobile phone",
    phone_landline = "talking on a landline phone"
  )
  for (nm in names(tud)) {
    m <- regmatches(nm, regexec("^tud_(.+)_(main|any)_mins_(wd|we)$", nm))[[1]]
    if (length(m) > 0) {
      attr(tud[[nm]], "label") <- paste0(
        "Time-use diary: mean minutes ",
        activity[[m[[2]]]],
        if (m[[3]] == "main") {
          " as the main activity"
        } else {
          " as main or concurrent activity"
        },
        " per ",
        if (m[[4]] == "wd") "weekday" else "weekend",
        " diary day"
      )
    }
  }
  attr(
    tud$tud_days_wd,
    "label"
  ) <- "Time-use diary: number of weekday diary days"
  attr(
    tud$tud_days_we,
    "label"
  ) <- "Time-use diary: number of weekend diary days"
  attr(tud$tud_diarist, "label") <- "Time-use diary: who completed the diary"
  tud
}
