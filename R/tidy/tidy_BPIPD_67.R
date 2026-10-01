#' Tidier for BPIPD-67 (PATH)
#'
#' Stacks the Youth / Parent files of ICPSR 36498 (waves 1-8) and 37786 (the
#' half-waves 4.5, 5.5 and 7.5) after stripping each column's wave prefix, with
#' each wave's cross-sectional weight joined on.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per youth per wave interviewed, keyed on `PERSONID` and `wave`
tidy_BPIPD_67 <- function(raw_dataset, spec) {
  tables <- lapply(raw_dataset$data, bp67_prepare)
  is_weights <- startsWith(names(tables), "weights_")
  weights <- tables[is_weights]
  names(weights) <- vapply(weights, function(tbl) tbl$wave[[1]], character(1))

  parts <- lapply(tables[!is_weights], function(tbl) {
    wave_weights <- weights[[tbl$wave[[1]]]]
    # Waves 1 and 2 release their weights in the youth file itself.
    if (is.null(wave_weights)) {
      return(tbl)
    }
    dplyr::left_join(
      tbl,
      bp67_weights(wave_weights),
      by = c("PERSONID", "wave"),
      relationship = "one-to-one"
    )
  })

  df <- dplyr::bind_rows(bp67_align_types(parts))
  df <- bp67_relabel(df, raw_dataset$data)

  wave_order <- as.numeric(df$wave)
  df$parent_relationship <- bp67_parent_relationship(df, wave_order)
  df$other_parent_relationship <- bp67_other_parent_relationship(df)
  attr(df$parent_relationship, "label") <- paste(
    "DERIVED - Parent respondent's relationship to the youth (R_Y_PT0001,",
    "R_Y_PT0001_V2, R_Y_PT0001_V3); where PATH skipped the question because",
    "the same parent answered at the PATH wave just before, that wave's value.",
    "Other is a foster parent, other relative or non-relative at waves 1-4,",
    "and from wave 5 also an adoptive or step parent"
  )
  attr(df$other_parent_relationship, "label") <- paste(
    "DERIVED - Relationship to the youth of the other parental figure living",
    "in the household: the resident spouse or partner (R_Y_PT0002 and its",
    "_V2/_V3 recodes) or other parental figure or guardian (R_Y_PM0059,",
    "R_Y_PM0062 and recodes), the one closest to a biological parent where",
    "there are several. Other as in parent_relationship"
  )

  df$mother_educ <- bp67_bio_parent_educ(df, "Biological mother")
  df$father_educ <- bp67_bio_parent_educ(df, "Biological father")
  attr(df$mother_educ, "label") <- paste(
    "DERIVED - Biological mother's education on one five-level scale: the",
    "parent respondent's (R_Y_PM0001) where they are the biological mother",
    "(parent_relationship), else the spouse or guardian's (R_Y_PM0118, waves",
    "2-4) where that is the biological mother (R_Y_PT0002, R_Y_PM0059)"
  )
  attr(df$father_educ, "label") <- gsub(
    "mother",
    "father",
    attr(df$mother_educ, "label"),
    fixed = TRUE
  )

  # Parent education always, and the stratum and PSU, which never change within
  # a youth, carry to every wave.
  for (column in bp67_carried) {
    label <- attr(df[[column]], "label")
    df[[column]] <- carry_within(df[[column]], df$PERSONID, wave_order)
    attr(df[[column]], "label") <- paste0(
      label,
      "; missing waves take the participant's earliest observed value"
    )
  }

  attr(df$Y_PWGT, "label") <- paste(
    "Youth weight for cross-sectional estimates of the wave (User Guide Table",
    "6.1): the cross-sectional weights R01_Y_PWGT, R04_Y_C04WGT and",
    "R07_Y_C07WGT at the waves that formed a cohort, and elsewhere the",
    "single-wave weights PATH uses as pseudo cross-sectional weights,",
    "R02_Y_PWGT, R03_Y_SWGT, R05_Y_S04WGT, R06_Y_S04WGT and R08_Y_S07WGT"
  )
  for (k in seq_len(100)) {
    attr(df[[paste0("Y_PWGT", k)]], "label") <- paste(
      "Replicate",
      k,
      "of Y_PWGT"
    )
  }

  df$parent_educ_highest <- bp67_parent_educ_highest(df)
  attr(df$parent_educ_highest, "label") <- paste(
    "DERIVED - Highest education of the parent respondent and resident spouse",
    "or guardian on one five-level scale: R_P_PARSP_EDUC (waves 4.5-6) and",
    "R_P_PARSP_EDUC_V2 (waves 7-8), the higher of R_Y_PM0001 and R_Y_PM0118",
    "(waves 2-4); at wave 1, which asked about one parent, R_Y_PM0001 where no",
    "spouse or partner lives in the household (PT0045), otherwise the highest",
    "including values carried from later waves, else NA"
  )

  df$adhd_told_before <- bp67_adhd_told_before(df, wave_order)
  attr(df$adhd_told_before, "label") <- paste(
    "DERIVED - Parent reported at an earlier wave that a health professional",
    "had told them the youth has ADHD or ADD (from R_Y_PY0052, PT0052_NB and",
    "PT0052_12M); FALSE if the youth's first interview recorded never told and",
    "no later wave reported it; NA at the first interview"
  )

  if (anyDuplicated(df[c("PERSONID", "wave")]) > 0) {
    stop(
      "BPIPD-67: `PERSONID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' Parent education and survey design columns carried within participant
bp67_carried <- c(
  "mother_educ",
  "father_educ",
  "R_Y_PM0001",
  "R_Y_PM0118",
  "R_P_PARSP_EDUC",
  "R_P_PARSP_EDUC_V2",
  "VARSTRAT",
  "VARPSU"
)

#' The five education levels the four parent-education codings share
bp67_parent_educ_levels <- c(
  "Less than high school" = "Less than high school",
  "Less than High School" = "Less than high school",
  "High school graduate or equivalent" = "High school graduate or GED",
  "GED" = "High school graduate or GED",
  "High school graduate" = "High school graduate or GED",
  "High school graduate or GED" = "High school graduate or GED",
  "Some college (no degree) or associates degree" = "Some college (no degree) or associates degree",
  "Bachelor's degree" = "Bachelor's degree",
  "Advanced degree" = "Advanced degree"
)

#' Rank of a parent-education factor on the five shared levels
bp67_educ_rank <- function(x) {
  match(bp67_parent_educ_levels[bp67_label(x)], unique(bp67_parent_educ_levels))
}

#' Highest parent education, preferring each wave's own highest-parent item
bp67_parent_educ_highest <- function(df) {
  parent <- bp67_educ_rank(df$R_Y_PM0001)
  spouse <- bp67_educ_rank(df$R_Y_PM0118)
  highest <- bp67_educ_rank(df$R_P_PARSP_EDUC)
  highest_v2 <- bp67_educ_rank(df$R_P_PARSP_EDUC_V2)
  no_partner <- readr::parse_number(as.character(df$PT0045)) %in% 2

  best <- dplyr::case_when(
    df$wave %in% c("4.5", "5", "5.5", "6") & !is.na(highest) ~ highest,
    df$wave %in% c("7", "7.5", "8") & !is.na(highest_v2) ~ highest_v2,
    df$wave %in% c("2", "3", "4") ~ pmax(parent, spouse, na.rm = TRUE),
    df$wave == "1" & no_partner ~ parent,
    df$wave == "1" & is.na(spouse) & is.na(highest) & is.na(highest_v2) ~ NA,
    .default = pmax(parent, spouse, highest, highest_v2, na.rm = TRUE)
  )
  unique(bp67_parent_educ_levels)[best]
}

#' Education of the youth's biological mother or father, before any carry
bp67_bio_parent_educ <- function(df, who) {
  respondent <- ifelse(
    df$parent_relationship %in% who,
    bp67_educ_rank(df$R_Y_PM0001),
    NA
  )
  # PM0118 asks about the resident spouse, else a biological parent figure.
  other <- dplyr::coalesce(bp67_label(df$R_Y_PT0002), bp67_label(df$R_Y_PM0059))
  guardian <- ifelse(other %in% who, bp67_educ_rank(df$R_Y_PM0118), NA)
  unique(bp67_parent_educ_levels)[pmax(respondent, guardian, na.rm = TRUE)]
}

#' The parent respondent's relationship to the youth, filled where PATH skipped it
bp67_parent_relationship <- function(df, wave_order) {
  asked <- dplyr::coalesce(
    bp67_label(df$R_Y_PT0001),
    bp67_label(df$R_Y_PT0001_V2),
    bp67_label(df$R_Y_PT0001_V3)
  )
  # Skipped only when the same parent answered at the PATH wave just before,
  # half-waves included (questionnaire box PXR03), so runs break at any gap.
  schedule <- c(1, 2, 3, 4, 4.5, 5, 5.5, 6, 7, 7.5, 8)
  interviewed <- !is.na(asked) | !is.na(df$R_P_OTHPAR_INHH)
  o <- order(df$PERSONID, wave_order)
  id <- df$PERSONID[o]
  ok <- interviewed[o]
  follows <- c(FALSE, id[-1] == id[-length(id)] & ok[-1] & ok[-length(ok)]) &
    c(FALSE, diff(match(wave_order[o], schedule)) == 1)
  run <- cumsum(!follows)
  last <- cummax(ifelse(is.na(asked[o]), 0L, seq_along(o)))
  filled <- ifelse(last >= match(run, run), asked[o][pmax(last, 1L)], NA)
  filled[order(o)]
}

#' The other resident parental figure's relationship, nearest to a parent first
bp67_other_parent_relationship <- function(df) {
  rank <- c(
    "Biological mother" = 1,
    "Biological father" = 1,
    "Biological parent" = 1,
    "Adopted or step parent" = 2,
    "Other" = 3
  )
  best <- rep(NA_character_, nrow(df))
  for (stem in c("R_Y_PT0002", "R_Y_PM0059", "R_Y_PM0062")) {
    for (column in paste0(stem, c("", "_V2", "_V3"))) {
      x <- bp67_label(df[[column]])
      better <- !is.na(x) & (is.na(best) | rank[x] < rank[best])
      best[better] <- x[better]
    }
  }
  best
}

#' A PATH factor's level label without its `(1) 1 = ` code prefix
bp67_label <- function(x) {
  trimws(sub("^\\([0-9]+\\) [0-9]+ = ", "", as.character(x)))
}

#' Whether an ADHD diagnosis was reported at any of the youth's earlier waves
bp67_adhd_told_before <- function(df, wave_order) {
  code <- function(x) readr::parse_number(as.character(x))
  # Waves 2-8 ask every parent about the past 12 months and only new
  # respondents about ever, so history is needed to tell previous from never.
  told <- (code(df$R_Y_PY0052) %in% 1) |
    (code(df$PT0052_NB) %in% 1) |
    (code(df$PT0052_12M) %in% 1)
  never <- (code(df$R_Y_PY0052) %in% 2) | (code(df$PT0052_NB) %in% 2)

  o <- order(df$PERSONID, wave_order)
  id <- df$PERSONID[o]
  first <- !duplicated(id)
  told_so_far <- stats::ave(as.integer(told[o]), id, FUN = cumsum)
  told_earlier <- (told_so_far - told[o]) > 0
  never_at_first <- never[o][first][match(id, id[first])]

  out <- rep(NA, length(id))
  out[!first & never_at_first] <- FALSE
  out[told_earlier] <- TRUE
  out[order(o)]
}

#' One wave's file, with the wave prefix stripped from its column names
bp67_prepare <- function(tbl) {
  tbl <- tibble::as_tibble(tbl)
  wave <- bp67_wave(names(tbl))
  names(tbl) <- bp67_stem(names(tbl))
  tbl[] <- lapply(tbl, bp67_utf8_levels)

  # `PERSONID` is padded to 20 characters up to wave 4 and to 10 from wave 5.
  tbl$PERSONID <- trimws(as.character(tbl$PERSONID))

  tbl$wave <- wave
  dplyr::relocate(tbl, "wave")
}

#' The wave a file holds, from its variable-name prefix
bp67_wave <- function(x) {
  # `R01` is wave 1 and `X04` the half-wave 4.5 (User Guide section 7.4).
  prefix <- unique(regmatches(x, regexpr("^[RX][0-9]{2}", x)))
  half <- ifelse(startsWith(prefix, "X"), 0.5, 0)
  as.character(as.integer(substr(prefix, 2, 3)) + half)
}

#' A weight file's full-sample and replicate weights under the `Y_PWGT` stem
bp67_weights <- function(tbl) {
  names(tbl) <- sub("^Y_[A-Z]+[0-9]*WGT", "Y_PWGT", names(tbl))
  # `CASEID` numbers the rows of each file and is not a person id.
  tbl[setdiff(names(tbl), "CASEID")]
}

#' Strip the `R01`/`X04` wave prefix from a variable name or its label
bp67_stem <- function(x) {
  sub("^[RX][0-9]{2}_?", "", x)
}

#' Re-encode a factor's levels, which the ICPSR files store as latin1
bp67_utf8_levels <- function(x) {
  if (!is.factor(x)) {
    return(x)
  }
  levels(x) <- iconv(levels(x), from = "latin1", to = "UTF-8", sub = "?")
  x
}

#' Coerce to character the columns whose class differs between waves
bp67_align_types <- function(parts) {
  classes <- lapply(parts, function(tbl) {
    vapply(tbl, function(x) class(x)[[1]], character(1))
  })
  by_column <- split(
    unlist(classes, use.names = FALSE),
    unlist(lapply(classes, names), use.names = FALSE)
  )
  # ICPSR writes an item with no valid case in a wave as numeric `NA`.
  mixed <- names(by_column)[lengths(lapply(by_column, unique)) > 1L]

  lapply(parts, function(tbl) {
    columns <- intersect(mixed, names(tbl))
    tbl[columns] <- lapply(tbl[columns], as.character)
    tbl
  })
}

#' Restore variable labels from the earliest wave that has each column
bp67_relabel <- function(df, tables) {
  labels <- list(wave = "PATH wave")
  for (tbl in tables) {
    # ICPSR keeps labels in a data-frame attribute that `bind_rows()` drops.
    stems <- bp67_stem(names(tbl))
    text <- attr(tbl, "variable.labels", exact = TRUE)
    text <- bp67_stem(iconv(text, from = "latin1", to = "UTF-8", sub = "?"))
    fresh <- setdiff(stems, names(labels))
    labels[fresh] <- text[match(fresh, stems)]
  }

  for (column in intersect(names(df), names(labels))) {
    attr(df[[column]], "label") <- labels[[column]]
  }
  df
}
