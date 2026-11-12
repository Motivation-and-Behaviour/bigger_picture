#' Tidier for BPIPD-631 (UK Understanding Society)
#'
#' Stacks the youth self-completion files of BHPS waves 4-18 and UKHLS waves
#' 1-15, joining each youth (within wave) to their household, resident
#' parents' adult interviews and, for BHPS, age and sample origin from the
#' household roster, and resident siblings counted from the BHPS household
#' relationship files; the cross-wave person file adds ethnicity, birthplace
#' and birth weight and timing, the child files a parent's report of the
#' school type, and the parenting-style files one parent's parenting-style
#' answers about a youth aged 10.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per youth per wave (`pidp` x `wave`)
tidy_BPIPD_631 <- function(raw_dataset, spec) {
  waves <- setdiff(names(raw_dataset$data), "xwavedat")
  tables <- lapply(raw_dataset$data[waves], function(tbl) {
    bp631_zap_missing(bp631_unprefix(tbl))
  })
  indresp <- bp631_stack(tables, "indresp")
  xwave <- bp631_zap_missing(raw_dataset$data$xwavedat)
  # The cross-wave file's design codes are joined under their own names and
  # preferred below: the BHPS wave files give two sampling units an earlier
  # stratum code than the cross-wave file does.
  xwave <- dplyr::rename(xwave, psu_xwave = "psu", strata_xwave = "strata")
  parent_cols <- c("pidp", "ethn_dv", "bornuk_dv")

  # `mnspno`/`fnspno`: natural/step/adoptive mother's and father's person
  # numbers; 0 (none) matches no `pno`.
  df <- bp631_stack(tables, "youth") |>
    bp631_add(
      bp631_stack(tables, "indall"),
      c("survey", "wave", "pidp"),
      "one-to-one"
    ) |>
    bp631_add(
      bp631_stack(tables, "hhresp"),
      c("survey", "wave", "hidp"),
      "many-to-one"
    ) |>
    bp631_add(
      bp631_stack(tables, "hhsamp"),
      c("survey", "wave", "hidp"),
      "many-to-one"
    ) |>
    bp631_add(
      bp631_siblings(bp631_stack(tables, "egoalt")),
      c("survey", "wave", "hidp", "pno"),
      "one-to-one"
    ) |>
    dplyr::left_join(
      bp631_parent(indresp, "mother", c("survey", "wave", "hidp", "pno")),
      by = dplyr::join_by("survey", "wave", "hidp", "mnspno" == "pno"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      bp631_parent(indresp, "father", c("survey", "wave", "hidp", "pno")),
      by = dplyr::join_by("survey", "wave", "hidp", "fnspno" == "pno"),
      relationship = "many-to-one"
    ) |>
    bp631_add(xwave, "pidp", "many-to-one") |>
    # UKHLS youth files also name parents who gave no interview this wave
    dplyr::mutate(
      mother_pidp = dplyr::coalesce(mother_pidp, mnspid),
      father_pidp = dplyr::coalesce(father_pidp, fnspid)
    ) |>
    # Parents take ethnicity and birthplace only; birth items are the youth's
    dplyr::left_join(
      bp631_parent(xwave[parent_cols], "mother", "pidp"),
      by = c("mother_pidp" = "pidp"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      bp631_parent(xwave[parent_cols], "father", "pidp"),
      by = c("father_pidp" = "pidp"),
      relationship = "many-to-one"
    ) |>
    dplyr::relocate("pidp", "survey", "wave")

  df <- dplyr::left_join(
    df,
    bp631_child(bp631_stack(tables, "child"), df),
    by = c("survey", "wave", "hidp", "pno"),
    relationship = "one-to-one"
  )

  df <- dplyr::left_join(
    df,
    bp631_parstyle(bp631_stack(tables, "parstyle"), df),
    by = c("survey", "wave", "hidp", "pno"),
    relationship = "one-to-one"
  )

  # The .dta labels of `ypnetchtw` are garbled (codes 3 and 4 both read 4-6
  # hours); the wave 15 youth questionnaire gives it the responses of `ypnetcht`.
  attr(df$ypnetchtw, "labels") <- attr(df$ypnetcht, "labels")

  # `psu`/`strata` -9 (none assigned) is unlabelled, so
  # `bp631_zap_missing()` leaves it.
  df$psu[df$psu < 0] <- NA
  df$strata[df$strata < 0] <- NA
  for (col in c("psu", "strata")) {
    cross_wave <- df[[paste0(col, "_xwave")]]
    use <- !is.na(cross_wave) & cross_wave > 0
    df[[col]][use] <- cross_wave[use]
    df[[paste0(col, "_xwave")]] <- NULL
  }

  attr(df$nnssib_dv, "label") <- paste0(
    attr(df$nnssib_dv, "label"),
    " (BHPS: natural, half, step or adopted, counted from egoalt)"
  )

  # Fill household income and composition, siblings and the mother's activity
  # missed at a wave from the nearest wave interviewed 12 months or less away.
  interview_month <- 12 *
    dplyr::coalesce(bp631_num(df$intdaty_dv), bp631_num(df$intdatey)) +
    dplyr::coalesce(bp631_num(df$intdatm_dv), bp631_num(df$intdatem))
  carried <- c(
    "fihhmngrs_dv",
    "ieqmoecd_dv",
    "hhsize",
    "nkids_dv",
    "nnssib_dv",
    "mother_jbstat"
  )
  for (col in carried) {
    df[[col]] <- bp631_carry_gap(df[[col]], df$pidp, interview_month)
  }

  if (anyDuplicated(df[c("pidp", "wave")]) > 0) {
    stop(
      "BPIPD-631: `pidp` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' Strip a table's wave prefix from its column names and keep it as `wave`
bp631_unprefix <- function(tbl) {
  # Prefixes are the wave letter, after a `b` for BHPS (`bd_` is BHPS wave 4)
  wave <- sub("_hidp$", "", grep("^b?[a-r]_hidp$", names(tbl), value = TRUE))
  names(tbl) <- sub(paste0("^", wave, "_"), "", names(tbl))

  codings <- bp631_older_codings()
  for (item in intersect(names(tbl), names(codings))) {
    older <- Find(function(waves) wave %in% waves, codings[[item]])
    if (!is.null(older)) {
      names(tbl)[names(tbl) == item] <- paste0(item, "_", older[1])
    }
  }

  tbl$wave <- wave
  attr(tbl$wave, "label") <-
    "Wave prefix: bd-br (BHPS waves 4-18) or a-o (UKHLS waves 1-15)"
  tbl
}

#' Items whose codes change meaning between waves: the waves of each older coding
bp631_older_codings <- function() {
  bd_to_bk <- c("bd", "be", "bf", "bg", "bh", "bi", "bj", "bk")
  list(
    # Friends-round bands 1-2/3-5/6+ became 1-3/4-6/7+ at bl
    yppals = list(bd_to_bk),
    # Job values were three-point before bl
    ypjbqa = list(c("bd", "be", "bf")),
    ypjbqb = list(c("bd", "be", "bf")),
    ypjbqd = list(c("bd", "be", "bf")),
    # br offers "at least once a week" and "never" for these leisure items
    ypfclub = list(c("bg", "bh", "bi", "bj", "bk", "bp")),
    ypfdisc = list(c("bg", "bh", "bi", "bj", "bk", "bp")),
    ypfspor = list(c("bg", "bh", "bi", "bj", "bk", "bp")),
    # Four choosers at bd, then yourself or someone else
    ypmenu = list("bd"),
    # br asks how many times felt depressed, not how many days felt unhappy
    ypsad = list(c(bd_to_bk, "bp")),
    # Sport codes 21 and 22 were reassigned at bp
    ypsprt1 = list("bn"),
    ypsprt2 = list("bn"),
    # Party and religion code lists were renumbered
    ypvte3 = list(c(bd_to_bk, "bl", "bm")),
    ypvte3gb = list("c", "e", c("g", "i")),
    ypvte3ni = list("c", "e", c("g", "i")),
    ypreliggb = list("a"),
    ypreligni = list("a"),
    # Step-parents are coded apart from parents from m
    ypupset = list(c("a", "c", "e", "g", "i", "k"))
  )
}

#' Set the release's missing-value codes to NA and drop their value labels
bp631_zap_missing <- function(tbl) {
  # Every negative labelled code is missing (-1 don't know to -9 missing);
  # UKHLS household income has real negative values, unlabelled.
  tbl[] <- lapply(tbl, function(x) {
    labels <- attr(x, "labels", exact = TRUE)
    if (!is.numeric(labels) || !any(labels < 0)) {
      return(x)
    }
    x[unclass(x) %in% labels[labels < 0]] <- NA
    if (all(labels < 0)) {
      return(haven::zap_labels(x))
    }
    attr(x, "labels") <- labels[labels >= 0]
    x
  })
  tbl
}

#' Bind one file type's waves from both surveys, with each column's labels
#' taken from its latest wave
bp631_stack <- function(tables, file) {
  tables <- tables[grepl(paste0("_", file, "__"), names(tables))]
  latest <- bp631_latest_labels(tables)
  tables <- lapply(tables, function(tbl) {
    for (col in intersect(names(tbl), names(latest$labels))) {
      if (is.numeric(attr(tbl[[col]], "labels", exact = TRUE))) {
        attr(tbl[[col]], "labels") <- latest$labels[[col]]
      }
    }
    tbl
  })

  out <- dplyr::bind_rows(tables, .id = "survey")
  # `bind_rows()` drops the variable label of columns without value labels
  for (col in names(latest$label)) {
    attr(out[[col]], "label") <- latest$label[[col]]
  }
  out$survey <- toupper(sub("_.*", "", out$survey))
  attr(out$survey, "label") <- "Survey: BHPS or UKHLS"
  out
}

#' Each column's variable and value labels from its latest wave, adding codes
#' only older waves label and use
bp631_latest_labels <- function(tables) {
  # Wording drifts between waves ("1 - 3", "1-3 hours") but the codes' meaning
  # does not, except for the items in `bp631_older_codings()`.
  label <- list()
  labels <- list()
  for (tbl in rev(tables)) {
    for (col in names(tbl)) {
      if (is.null(label[[col]])) {
        label[[col]] <- attr(tbl[[col]], "label", exact = TRUE)
      }
      codes <- attr(tbl[[col]], "labels", exact = TRUE)
      if (!is.numeric(codes)) {
        next
      }
      used <- codes[!codes %in% labels[[col]] & codes %in% unclass(tbl[[col]])]
      labels[[col]] <- if (is.null(labels[[col]])) {
        codes
      } else {
        sort(c(labels[[col]], used))
      }
    }
  }
  list(label = label, labels = labels)
}

#' Join `tbl` onto `base`, filling gaps in the columns both carry
bp631_add <- function(base, tbl, by, relationship) {
  # Only UKHLS youth files carry region and ethnicity; BHPS rows take them
  # from the household and cross-wave person files.
  shared <- setdiff(intersect(names(base), names(tbl)), by)
  out <- dplyr::left_join(
    base,
    tbl,
    by = by,
    relationship = relationship,
    suffix = c("", ".fill")
  )
  for (col in shared) {
    gap <- is.na(out[[col]])
    out[[col]][gap] <- out[[paste0(col, ".fill")]][gap]
  }
  out[setdiff(names(out), paste0(shared, ".fill"))]
}

#' Each BHPS household member's number of siblings in the household
bp631_siblings <- function(egoalt) {
  # `rel` 10 natural (incl. half), 11 other (adopted, step), 28 half brother or
  # sister (BHPS User Manual, Volume A, Appendices 1.1.6 and 3.3)
  egoalt$sibling <- unclass(egoalt$rel) %in% c(10, 11, 28)
  dplyr::summarise(
    egoalt,
    nnssib_dv = as.numeric(sum(sibling)),
    .by = c("survey", "wave", "hidp", "pno")
  )
}

#' A column as plain numeric with negative (missing) codes set to NA
bp631_num <- function(x) {
  x <- as.numeric(x)
  x[x < 0] <- NA
  x
}

#' Fill NA in `x` from the same participant's nearest other wave, only when
#' that wave's interview is `max_gap` months or less away (`time` in months),
#' else leave NA
bp631_carry_gap <- function(x, pidp, time, max_gap = 12) {
  out <- x
  for (id in unique(pidp[is.na(x)])) {
    idx <- which(pidp == id)
    if (length(idx) < 2) {
      next
    }
    vals <- x[idx]
    times <- time[idx]
    for (i in which(is.na(vals))) {
      if (is.na(times[i])) {
        next
      }
      gap <- abs(times - times[i])
      gap[i] <- NA
      gap[is.na(vals)] <- NA
      candidates <- which(!is.na(gap) & gap <= max_gap)
      if (length(candidates) > 0) {
        out[idx[i]] <- vals[candidates[which.min(gap[candidates])]]
      }
    }
  }
  out
}

#' A person table keyed for joining as a parent, its columns named for them
bp631_parent <- function(tbl, parent, keys) {
  cols <- setdiff(names(tbl), keys)
  out <- tbl[c(keys, cols)]
  for (col in cols) {
    attr(out[[col]], "label") <- paste0(
      tools::toTitleCase(parent),
      ": ",
      attr(out[[col]], "label")
    )
  }
  names(out)[names(out) %in% cols] <- paste0(parent, "_", cols)
  out
}

#' One school-type report per youth from the child files, keyed on `hidp`/`pno`
bp631_child <- function(child, youth) {
  # BHPS rows are one per responding parent and child: `pno` is the parent's
  bhps <- child$survey == "BHPS"
  child$rapno2[bhps] <- child$pno[bhps]
  child$pno[bhps] <- child$scpno[bhps]
  label <- "pno of parent who answered the school questions"
  attr(child$rapno2, "label") <- label

  keys <- c("survey", "wave", "hidp", "pno")
  items <- c("schsta", "stillstate", "stillpriv", "rapno2")
  child <- dplyr::inner_join(
    child[c(keys, items)],
    youth[c(keys, "mnspno", "fnspno")],
    by = keys,
    relationship = "many-to-one"
  )
  # Where both BHPS parents answered, keep the mother's answer, else the
  # father's, else another adult's
  respondent <- dplyr::case_when(
    child$rapno2 == child$mnspno ~ 1,
    child$rapno2 == child$fnspno ~ 2,
    .default = 3
  )
  child <- child[order(is.na(child$schsta), respondent), ]
  child[!duplicated(child[keys]), c(keys, items)]
}

#' One parenting-style report per youth from the parenting-style files, keyed
#' on `hidp`/`pno`
bp631_parstyle <- function(parstyle, youth) {
  # Rows are one per responsible parent and child aged 10: `pno` is the
  # parent's, `childpno` the child's
  parstyle$ps_pno <- parstyle$pno
  attr(parstyle$ps_pno, "label") <-
    "pno of the parent who answered the parenting-style questions"
  parstyle$pno <- parstyle$childpno

  keys <- c("survey", "wave", "hidp", "pno")
  items <- c(
    grep("^ps[0-9]+$", names(parstyle), value = TRUE),
    "psdqa_dv",
    "psdqb_dv",
    "psdqc_dv"
  )
  for (col in items) {
    attr(parstyle[[col]], "label") <- paste0(
      "Parenting style (parent about youth aged 10): ",
      attr(parstyle[[col]], "label")
    )
  }
  context <- c(
    "mnspno",
    "fnspno",
    "rapno2",
    "adresp15_dv",
    "schsta",
    "stillstate",
    "stillpriv"
  )
  parstyle <- dplyr::inner_join(
    parstyle[c(keys, items, "ps_pno")],
    youth[c(keys, context)],
    by = keys,
    relationship = "many-to-one"
  )
  # Where two parents answered about the youth, keep the report of the parent
  # parent_respondent records (who answered the school questions, on rows with
  # a school type), else of the youth's responsible adult (adresp15_dv), else
  # the mother's, else the father's; a report with no answered item comes last
  num <- lapply(parstyle[c(items, "ps_pno", context)], function(x) {
    bp631_num(unclass(x))
  })
  answered <- rowSums(!is.na(as.data.frame(num[items])))
  still <- num$stillstate %in% 1 | num$stillpriv %in% 1
  school_row <- num$schsta %in% c(1, 2) | still
  respondent <- dplyr::case_when(
    school_row & num$ps_pno == num$rapno2 ~ 1,
    num$ps_pno == num$adresp15_dv ~ 2,
    num$ps_pno == num$mnspno ~ 3,
    num$ps_pno == num$fnspno ~ 4,
    .default = 5
  )
  parstyle <- parstyle[order(answered == 0, respondent), ]
  parstyle[!duplicated(parstyle[keys]), c(keys, items, "ps_pno")]
}
