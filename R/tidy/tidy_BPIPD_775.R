#' Tidier for BPIPD-775 (ECLS-K, in a folder named ECLS-B)
#'
#' One fixed-width NCES record per child holds nine rounds as name prefixes
#' (`P4TVHR` is round 4); they are pivoted to rounds and labelled from the
#' Stata dictionary and do-file, which the data file does not carry.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per round attended (rounds 2-9)
tidy_BPIPD_775 <- function(raw_dataset, spec) {
  raw <- raw_dataset$data$eclsk2011_k5_child
  values <- bp775_value_labels(grep(
    "ECLSK2011_K5PUF\\.do$",
    raw_dataset$docs,
    value = TRUE
  ))
  map <- bp775_column_map(names(raw), values)
  child_cols <- map$name[is.na(map$round)]
  coded <- unique(map$stem[
    map$name %in% names(raw)[vapply(raw, is.character, logical(1))]
  ])

  # Round 1 (fall 2010) asked no screen item; it is read only for the fills.
  long <- dplyr::bind_rows(lapply(c(2:9, 1L), function(round) {
    cols <- map[map$round %in% round, ]
    from <- c(child_cols, cols$name)
    if (round == 1L) {
      cols <- cols[
        cols$stem %in%
          c(names(bp775_carried()), bp775_date_stems(), "X_KAGE_R"),
      ]
      from <- c("CHILDID", cols$name)
    }
    bp775_round_frame(raw, round, from, map$stem[match(from, map$name)], coded)
  }))

  # Children with no round-2 row keep their X12 composites, dated at round 1.
  carried <- bp775_carried()
  x12 <- map[startsWith(map$name, "X12") & map$stem %in% names(carried), ]
  r1 <- which(
    long$round == 1L & !long$CHILDID %in% long$CHILDID[long$round == 2L]
  )
  for (i in seq_len(nrow(x12))) {
    long[[x12$stem[[i]]]][r1] <- raw[[x12$name[[i]]]][
      match(long$CHILDID[r1], raw$CHILDID)
    ]
  }

  month <- bp775_occasion_month(long)
  pairs <- c(X_PAR1ED_I = "X_HPAR1", X_PAR2ED_I = "X_HPAR2")
  for (stem in names(carried)) {
    # A not-ascertained code (-9) records nothing: it is filled, never a source.
    source <- replace(long[[stem]], long[[stem]] %in% -9, NA)
    if (carried[[stem]] == "parent education, always") {
      from <- carry_within(
        ifelse(is.na(source), NA_integer_, seq_along(source)),
        long$CHILDID,
        long$round
      )
      # Each education value keeps the parent's relationship code of its round;
      # X12 spans the kindergarten year, so round 2 falls back to round 1's code.
      rel <- replace(long[[pairs[[stem]]]], long[[pairs[[stem]]]] %in% -9, NA)
      k1 <- long$round == 1L
      k2 <- which(long$round == 2L & is.na(rel))
      rel[k2] <- rel[k1][match(long$CHILDID[k2], long$CHILDID[k1])]
      long[[paste0(pairs[[stem]], "_ED")]] <- dplyr::coalesce(
        rel,
        long[[pairs[[stem]]]]
      )[dplyr::coalesce(from, seq_along(source))]
      filled <- source[from]
    } else {
      filled <- bp775_carry_gap(source, long$CHILDID, long$round, month)
    }
    gap <- (is.na(long[[stem]]) | long[[stem]] %in% -9) & !is.na(filled)
    long[[stem]][gap] <- filled[gap]
  }
  long$child_age_months <- bp775_age_filled(long)
  long$grade_repeated <- bp775_grade_repeated(long)
  long <- long[long$round != 1L, , drop = FALSE]

  if (anyDuplicated(long[c("CHILDID", "round")]) > 0L) {
    stop(
      "BPIPD-775: `CHILDID` and `round` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  long$cohort <- "ECLS-K"
  stems <- unique(map$stem[!is.na(map$round)])
  long <- long[c(
    "CHILDID",
    "cohort",
    "round",
    setdiff(child_cols, "CHILDID"),
    intersect(stems, names(long)),
    names(bp775_constructed())
  )]
  bp775_label(
    long,
    map,
    bp775_variable_labels(raw_dataset$codebook$eclsk2011_k5_dictionary),
    values
  )
}

#' Round and cross-round stem of every column in the child file
bp775_column_map <- function(columns, values) {
  # X12, X34 and X56 composites span two rounds and sit on the later one.
  span <- grepl("^(IFX|X)(12|34|56)", columns)
  parts <- regmatches(columns, regexec("^(IF[A-Z]|[A-Z])([1-9])(.+)$", columns))
  parts[span] <- regmatches(
    columns[span],
    regexec("^(IFX|X)[135]([246])(.+)$", columns[span])
  )
  part <- function(k) {
    vapply(
      parts,
      function(p) if (length(p) == 4L) p[[k]] else NA_character_,
      character(1)
    )
  }
  prefix <- part(2L)
  round <- as.integer(part(3L))
  rest <- part(4L)
  by_round <- !is.na(round) &
    !grepl("^W", columns) &
    !columns %in% bp775_child_history()
  stem <- paste0(prefix, ifelse(startsWith(rest, "_"), "", "_"), rest)

  # Fall rounds 3 and 5 asked these about the summer, not the school year.
  summer <- by_round &
    prefix == "P" &
    round %in% c(3L, 5L) &
    rest %in% bp775_summer_items()
  stem[summer] <- paste0(stem[summer], "_SUMMER")
  # The base-year composites lack the `_I` of the same composite in later rounds.
  aliased <- columns %in% names(bp775_aliases())
  stem[aliased] <- bp775_aliases()[columns[aliased]]
  # Codes that change meaning between rounds get one stem per coding, `_R<rounds>`.
  for (recoded in intersect(bp775_recoded(), stem[by_round])) {
    at <- which(by_round & stem == recoded)
    coding <- vapply(values[columns[at]], bp775_coding, character(1))
    for (code in unique(coding)) {
      group <- coding == code
      later <- group & round[at] != 1L
      rounds <- sort(round[at][if (any(later)) later else group])
      stem[at[group]] <- paste0(recoded, "_R", paste(rounds, collapse = ""))
    }
  }
  # `X_DISTPOV` is also a round-less source column.
  stem[stem %in% columns] <- paste0(stem[stem %in% columns], "_ROUND")

  tibble::tibble(
    name = unname(columns),
    round = ifelse(by_round, round, NA_integer_),
    stem = ifelse(by_round, stem, unname(columns))
  )
}

#' One round's attending children, with that round's columns under their stems
bp775_round_frame <- function(raw, round, from, stems, coded) {
  out <- raw[bp775_attended(raw, round), from]
  names(out) <- stems
  # Ids and codes stored as text in some rounds and as numbers in others.
  for (stem in intersect(coded, stems)) {
    if (!is.character(out[[stem]])) {
      out[[stem]] <- ifelse(
        is.na(out[[stem]]),
        NA_character_,
        sprintf("%.0f", out[[stem]])
      )
    }
  }
  tibble::add_column(out, round = round, .after = "CHILDID")
}

#' Which children took part in a round, by NCES's definition of a round respondent
bp775_attended <- function(raw, round) {
  # Rounds 3 and 5 went to a fall subsample with its own participation flag.
  if (round %in% c(3L, 5L)) {
    return(raw[[paste0("X", round, "FALLSMP")]] %in% 1)
  }
  # Child assessment or parent interview data (K-5 manual, sections 4.1, 7.4).
  flags <- paste0(
    "X",
    round,
    c(
      "RDGFLG",
      "MTHFLG",
      "SCIFLG",
      "NRFLG",
      "DCCSFLG",
      "FLNKFLG",
      "HGTFLG",
      "WGTFLG",
      "ASMTST",
      "PARDAT"
    )
  )
  Reduce(`|`, lapply(raw[intersect(flags, names(raw))], function(x) x %in% 1))
}

#' Columns filled across rounds, with the rule each follows
bp775_carried <- function() {
  c(
    X_PAR1ED_I = "parent education, always",
    X_PAR2ED_I = "parent education, always",
    X_SESL_I = "household SES, gap of 12 months or less",
    X_INCCAT_I = "household SES, gap of 12 months or less",
    X_POVTY_I = "household SES, gap of 12 months or less",
    X_HPARNT = "family structure, gap of 12 months or less",
    X_HPAR1 = "family structure, gap of 12 months or less",
    X_HPAR2 = "family structure, gap of 12 months or less",
    X_NUMSIB = "siblings, gap of 12 months or less",
    X_PAR1EMP_I = "parent employment, gap of 12 months or less",
    X_PAR2EMP_I = "parent employment, gap of 12 months or less"
  )
}

#' Base-year composites renamed to the stem their later-round versions use
bp775_aliases <- function() {
  c(
    X12SESL = "X_SESL_I",
    X2POVTY = "X_POVTY_I",
    X1PAR1EMP = "X_PAR1EMP_I",
    X1PAR2EMP = "X_PAR2EMP_I"
  )
}

#' Stems whose value codes mean different things in different rounds (do-file)
bp775_recoded <- function() {
  c(
    "X_GRDLVL",
    "X_HIGGRD",
    "X_SCHBMM",
    "T_GRADE",
    "T_ACCOM",
    "T_BEZDAC",
    "T_BEZDSL",
    "T_BFLWIN",
    "A_TLKPAR",
    "P_SPORT",
    "P_DOCTOR",
    "P_DENTIS",
    "P_STRESS",
    "P_GTTSCH",
    "P_BMCNTC",
    "P_LANGUA",
    "P_PARCT1",
    "P_DOW_1",
    "P_DOW_2",
    "P_RELNUM",
    "P_HIG_1_I",
    "P_HIG_2_I"
  )
}

#' One string per coding: the non-negative codes with their normalised labels
bp775_coding <- function(labels) {
  labels <- labels[labels >= 0]
  text <- gsub("[^A-Z0-9]", "", toupper(gsub("\\([^)]*\\)", "", names(labels))))
  paste(labels, text, collapse = ";")
}

#' Stems that date each round's parent interview and child assessment
bp775_date_stems <- function() {
  c("X_PARDAT", "F_INTVYY", "F_INTVMM", "X_ASMTYY", "X_ASMTMM")
}

#' Month of each row's occasion: the parent interview, else the child assessment
bp775_occasion_month <- function(long) {
  parent <- long$X_PARDAT %in% 1
  year <- ifelse(parent, long$F_INTVYY, long$X_ASMTYY)
  month <- ifelse(parent, long$F_INTVMM, long$X_ASMTMM)
  ifelse(year > 0 & month > 0, 12 * year + month, NA_real_)
}

#' Fill each missing value from the same child's nearest round within 12 months
bp775_carry_gap <- function(x, id, round, month, max_gap = 12) {
  o <- order(id, round)
  xs <- x[o]
  ids <- id[o]
  ms <- month[o]
  n <- length(xs)
  at <- seq_len(n)
  prev <- cummax(ifelse(is.na(xs), 0L, at))
  nxt <- rev(cummin(rev(ifelse(is.na(xs), n + 1L, at))))
  prev[prev == 0L | ids[pmax(prev, 1L)] != ids] <- NA
  nxt[nxt > n | ids[pmin(nxt, n)] != ids] <- NA
  gap_prev <- abs(ms - ms[prev])
  gap_next <- abs(ms[nxt] - ms)
  use_prev <- !is.na(gap_prev) & gap_prev <= max_gap
  use_next <- !is.na(gap_next) & gap_next <= max_gap
  # A tie goes to the earlier round.
  take <- ifelse(
    use_prev & (!use_next | gap_prev <= gap_next),
    prev,
    ifelse(use_next, nxt, NA)
  )
  fill <- is.na(xs) & !is.na(take)
  xs[fill] <- xs[take[fill]]
  x[o] <- xs
  x
}

#' Assessment age in months, filled on unassessed rows from the nearest one
bp775_age_filled <- function(long) {
  age <- dplyr::coalesce(long$X_KAGE_R, long$X_AGE)
  age[age < 0] <- NA
  at <- function(yy, mm) ifelse(yy > 0 & mm > 0, 12 * yy + mm, NA_real_)
  assessed <- at(long$X_ASMTYY, long$X_ASMTMM)
  interview <- at(long$F_INTVYY, long$F_INTVMM)
  source <- which(!is.na(age) & !is.na(assessed))
  target <- which(is.na(age) & !is.na(interview))
  pairs <- merge(
    data.frame(target = target, id = long$CHILDID[target]),
    data.frame(source = source, id = long$CHILDID[source]),
    by = "id"
  )
  pairs$gap <- interview[pairs$target] - assessed[pairs$source]
  # The nearest assessed round wins; a tie goes to the earlier round.
  pairs <- pairs[
    order(pairs$target, abs(pairs$gap), long$round[pairs$source]),
  ]
  pairs <- pairs[!duplicated(pairs$target), ]
  age[pairs$target] <- age[pairs$source] + pairs$gap
  age
}

#' Repeated a grade: kindergarten repeated or the modal grade fallen behind
bp775_grade_repeated <- function(long) {
  # The teacher's first-time-kindergartner flag stands in for a -9 composite.
  first_k <- ifelse(long$X1FIRKDG %in% -9, long$T1FIRKDG, long$X1FIRKDG)
  first_k <- carry_within(
    replace(first_k, !first_k %in% 1:2, NA),
    long$CHILDID,
    long$round
  )
  later <- dplyr::coalesce(
    long$X_GRDLVL_R56,
    long$X_GRDLVL_R7,
    long$X_GRDLVL_R8,
    long$X_GRDLVL_R9
  )
  # Rounds 1-2 are the sampled kindergarten year; from round 5, 1 is modal.
  level <- dplyr::case_when(
    long$round <= 2L ~ "modal",
    long$X_GRDLVL_R3 %in% 1 | long$X_GRDLVL_R4 %in% 1 ~ "kindergarten",
    long$X_GRDLVL_R3 %in% 2 | long$X_GRDLVL_R4 %in% 2 ~ "modal",
    later %in% 1 ~ "modal",
    later %in% 2 ~ "other",
    .default = NA_character_
  )
  o <- order(long$CHILDID, long$round)
  id <- long$CHILDID[o]
  lv <- level[o]
  after_modal <- c(FALSE, lv[-length(lv)] %in% "modal") & duplicated(id)
  repeated_k <- first_k[o] %in% 2
  event <- repeated_k | lv %in% "kindergarten" | (lv %in% "other" & after_modal)
  ever <- stats::ave(as.integer(event), id, FUN = cumsum) > 0L
  out <- rep(NA, length(o))
  out[o] <- ifelse(
    ever,
    TRUE,
    ifelse(first_k[o] %in% 1 & lv %in% "modal", FALSE, NA)
  )
  out
}

#' Columns the tidier builds, with their labels
bp775_constructed <- function() {
  c(
    X_HPAR1_ED = paste(
      "RELATIONSHIP OF PRNT 1 TO FOCAL CHILD in the round X_PAR1ED_I came from",
      "[filled: with parent education, always]"
    ),
    X_HPAR2_ED = paste(
      "RELATIONSHIP OF PRNT 2 TO FOCAL CHILD in the round X_PAR2ED_I came from",
      "[filled: with parent education, always]"
    ),
    child_age_months = paste(
      "CHILD ASSESSMENT AGE (MONTHS): X_KAGE_R round 2, X_AGE rounds 3-9",
      "[filled: on unassessed rows, the nearest assessed round's age plus the",
      "months from that assessment to this row's parent interview]"
    ),
    grade_repeated = paste(
      "REPEATED A GRADE: TRUE where kindergarten was repeated (X1FIRKDG = 2,",
      "T1FIRKDG = 2 where X1FIRKDG is -9), the child was in kindergarten in",
      "round 3 or 4, or in a grade other than the modal grade in rounds 5-9 right",
      "after a round at the modal grade, carried to later rounds; FALSE for a",
      "first-time kindergartner at the modal grade with no earlier TRUE"
    )
  )
}

#' Fall-round parent items whose stem the spring rounds use for the school year
bp775_summer_items <- function() {
  c(
    "TVHR",
    "TVMIN",
    "VIDHR",
    "VIDMIN",
    "TUTREA",
    "TUTMTH",
    "TUTSCI",
    "TUTENGL",
    "TUTOTH"
  )
}

#' Birth and kindergarten-entry items, kept on every row under their source names
bp775_child_history <- function() {
  # Round 1 asked them and the round-2 SPQ repeated them for parents first
  # interviewed in spring; they describe the child's birth, not the round.
  c(
    "P1BRTWGT",
    "P1WEIGHP",
    "P1WEIGHO",
    "P1WEIGHG",
    "P1WEIGH5",
    "P1WEIGH3",
    "P1MORE10",
    "P1PREMAT",
    "P1ERLYUN",
    "P1EARLY",
    "P1BRSFED",
    "P1BRFDUN",
    "P1BRFDNM",
    "P1BRFDMO",
    "P1MULTIP",
    "P2WEIGHU",
    "P2WEIGHP",
    "P2WEIGHO",
    "P2WEIGHG",
    "P2WEIGH5",
    "P2WEIGH3",
    "P2MORE10",
    "P2PREMAT",
    "P2EARLY",
    "P2ERLYUN",
    "P2MULTIP",
    "X1FIRKDG",
    "T1FIRKDG"
  )
}

#' Variable labels from the Stata dictionary
bp775_variable_labels <- function(path) {
  lines <- iconv(readLines(path, warn = FALSE), "WINDOWS-1252", "UTF-8")
  parts <- regmatches(
    lines,
    regexec(
      '^\\s*_column\\([0-9]+\\)\\s+\\S+\\s+(\\S+)\\s+%\\S+\\s+"(.*)"\\s*$',
      lines
    )
  )
  parts <- Filter(function(part) length(part) == 3L, parts)
  stats::setNames(
    trimws(vapply(parts, `[`, character(1), 3L)),
    vapply(parts, `[`, character(1), 2L)
  )
}

#' Value labels from the do-file's `label define` and `label values` statements
bp775_value_labels <- function(path) {
  lines <- iconv(readLines(path, warn = FALSE), "WINDOWS-1252", "UTF-8")
  starts <- grep("^\\s*label define ", lines)
  ends <- grep("^\\s*;\\s*$", lines)
  sets <- lapply(starts, function(start) {
    body <- lines[seq(start + 1L, ends[findInterval(start, ends) + 1L] - 1L)]
    parts <- regmatches(body, regexec('^\\s*(-?[0-9.]+)\\s+"(.*)"\\s*$', body))
    parts <- Filter(function(part) length(part) == 3L, parts)
    stats::setNames(
      as.numeric(vapply(parts, `[`, character(1), 2L)),
      vapply(parts, `[`, character(1), 3L)
    )
  })
  names(sets) <- trimws(sub("^\\s*label define\\s+", "", lines[starts]))
  uses <- regmatches(
    lines,
    regexec("^\\s*label values\\s+(\\S+)\\s+([^; ]+)\\s*;", lines)
  )
  uses <- Filter(function(use) length(use) == 3L, uses)
  stats::setNames(
    sets[vapply(uses, `[`, character(1), 3L)],
    vapply(uses, `[`, character(1), 2L)
  )
}

#' Label the constructed columns, the round-less columns and every stem
bp775_label <- function(long, map, labels, values) {
  attr(long$cohort, "label") <- "ECLS kindergarten cohort of the record"
  long$round <- haven::labelled(
    long$round,
    labels = bp775_rounds(),
    label = "ECLS-K data-collection round"
  )
  carried <- bp775_carried()
  constructed <- bp775_constructed()
  for (stem in setdiff(names(long), c("cohort", "round", names(constructed)))) {
    sources <- map[map$stem == stem, ]
    if (any(!sources$round %in% 1L)) {
      sources <- sources[!sources$round %in% 1L, ]
    }
    # A stem's label drops the round prefix and lists wording that changed.
    text <- trimws(sub("^[A-Z]{1,2}[0-9]{1,2}K? ", "", labels[sources$name]))
    label <- if (length(unique(text)) == 1L) {
      text[[1]]
    } else {
      paste(paste0("R", sources$round, ": ", text), collapse = " | ")
    }
    if (endsWith(stem, "_SUMMER")) {
      label <- paste(
        label,
        "[a typical day of the summer before the fall round]"
      )
    }
    if (stem %in% names(carried)) {
      label <- paste0(label, " [filled: ", carried[[stem]], "]")
    }
    codes <- unlist(unname(values[sources$name]))
    codes <- codes[!duplicated(codes)]
    long[[stem]] <- if (length(codes) == 0L || is.character(long[[stem]])) {
      structure(long[[stem]], label = label)
    } else {
      haven::labelled(long[[stem]], labels = codes, label = label)
    }
  }
  for (stem in names(constructed)) {
    parent <- sub("_ED$", "", stem)
    long[[stem]] <- if (parent == stem) {
      structure(long[[stem]], label = constructed[[stem]])
    } else {
      haven::labelled(
        as.numeric(long[[stem]]),
        labels = attr(long[[parent]], "labels"),
        label = constructed[[stem]]
      )
    }
  }
  long
}

#' The nine rounds, from the school-identifier labels of the dictionary
bp775_rounds <- function() {
  c(
    "Fall 2010, kindergarten" = 1L,
    "Spring 2011, kindergarten" = 2L,
    "Fall 2011, first grade (fall subsample)" = 3L,
    "Spring 2012, first grade" = 4L,
    "Fall 2012, second grade (fall subsample)" = 5L,
    "Spring 2013, second grade" = 6L,
    "Spring 2014, third grade" = 7L,
    "Spring 2015, fourth grade" = 8L,
    "Spring 2016, fifth grade" = 9L
  )
}
