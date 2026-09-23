#' Tidier for BPIPD-631 (UK Understanding Society)
#'
#' Stacks the youth self-completion files of BHPS waves 4-18 and UKHLS waves
#' 1-15, and joins each youth, within the same wave, to their household's
#' interview, their resident mother's and father's adult interviews and, for
#' BHPS, their own age from the household roster.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per youth per wave (`pidp` x `wave`)
tidy_BPIPD_631 <- function(raw_dataset, spec) {
  tables <- lapply(raw_dataset$data, function(tbl) {
    bp631_zap_missing(bp631_unprefix(tbl))
  })
  indresp <- bp631_stack(tables, "indresp")

  # `mnspno`/`fnspno` are the person numbers of the natural, step or adoptive
  # mother and father in the household; 0 (none) matches no `pno`.
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
    dplyr::left_join(
      bp631_parent(indresp, "mother"),
      by = dplyr::join_by("survey", "wave", "hidp", "mnspno" == "pno"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      bp631_parent(indresp, "father"),
      by = dplyr::join_by("survey", "wave", "hidp", "fnspno" == "pno"),
      relationship = "many-to-one"
    ) |>
    dplyr::relocate("pidp", "survey", "wave")

  # The .dta labels of `ypnetchtw` are garbled (codes 3 and 4 both read 4-6
  # hours); the wave 15 youth questionnaire gives it the responses of `ypnetcht`.
  attr(df$ypnetchtw, "labels") <- attr(df$ypnetcht, "labels")

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
  # Every negative code the release labels is a missing code (-1 don't know to
  # -9 missing); UKHLS household income has real negative values, unlabelled.
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

#' Join the columns of `tbl` that `base` does not already have
bp631_add <- function(base, tbl, by, relationship) {
  tbl <- tbl[c(by, setdiff(names(tbl), names(base)))]
  dplyr::left_join(base, tbl, by = by, relationship = relationship)
}

#' Adult interviews keyed on person number, with columns named for the parent
bp631_parent <- function(indresp, parent) {
  keys <- c("survey", "wave", "hidp", "pno")
  cols <- setdiff(names(indresp), keys)
  out <- indresp[c(keys, cols)]
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
