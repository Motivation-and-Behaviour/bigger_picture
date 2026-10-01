#' Tidier for BPIPD-2387 (CHIS, California Health Interview Survey)
#'
#' Repeated cross-section: each cycle's child file (proxy report, ages 0-11)
#' and adolescent file (self-report, 12-17) take their imputation flags and
#' stack as separate participants; `puf_id` is unique across every file.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child or adolescent per CHIS cycle
tidy_BPIPD_2387 <- function(raw_dataset, spec) {
  waves <- vapply(spec$waves, function(w) as.character(w$wave), character(1))
  parts <- unlist(
    lapply(waves, function(wave) {
      lapply(bp2387_cohorts, function(cohort) {
        bp2387_part(raw_dataset, wave, cohort)
      })
    }),
    recursive = FALSE
  )

  # The 2007 child questionnaire fields no screen item, so that part goes.
  parts <- Filter(bp2387_has_screen_item, parts)

  df <- dplyr::bind_rows(bp2387_align_value_labels(parts))
  df <- bp2387_restore_labels(df, parts)
  df <- dplyr::relocate(df, "puf_id", "cohort")

  if (anyDuplicated(df$puf_id) > 0) {
    stop("BPIPD-2387: `puf_id` does not uniquely identify rows.", call. = FALSE)
  }

  tibble::as_tibble(df)
}

#' The two survey components each cycle fields, in file order
bp2387_cohorts <- c("child", "teen")

#' Computer-for-fun hours, the only single-device screen items CHIS fields
bp2387_screen_items <- c(
  "cg9_p",
  "cg11_p",
  "te13_p",
  "te15_p",
  "cg9",
  "cg11",
  "te13",
  "te15"
)

#' Whether anyone in a part answered a single-device screen item
bp2387_has_screen_item <- function(tbl) {
  items <- intersect(bp2387_screen_items, names(tbl))
  answered <- vapply(
    tbl[items],
    function(x) any(haven::zap_labels(x) >= 0, na.rm = TRUE),
    logical(1)
  )
  any(answered)
}

#' One cycle's child or adolescent file with its imputation flags attached
bp2387_part <- function(raw_dataset, wave, cohort) {
  tbl <- raw_dataset$data[[paste0("chis", wave, "_", cohort, "_data")]]
  flags <- raw_dataset$data[[paste0("chis", wave, "_", cohort, "_data_flags")]]

  # CHIS 2001 shipped no imputation-flag file.
  if (!is.null(flags)) {
    flags <- flags[c("puf_id", setdiff(names(flags), names(tbl)))]
    tbl <- dplyr::left_join(
      tbl,
      flags,
      by = "puf_id",
      relationship = "one-to-one"
    )
  }

  tbl$cohort <- cohort
  attr(tbl$cohort, "label") <-
    "CHIS component: child (0-11, parent proxy) or teen (12-17, self-report)"

  bp2387_split_education(tbl, wave)
}

#' Keep the 4-level (2001) and 11-level (2005-2009) codings of `cheduca` apart
bp2387_split_education <- function(tbl, wave) {
  # Only the child files carry `cheduca`.
  if (!"cheduca" %in% names(tbl)) {
    return(tbl)
  }
  suffix <- if (wave == "2001") "_4lvl" else "_11lvl"
  names(tbl)[names(tbl) == "cheduca"] <- paste0("cheduca", suffix)
  tbl
}

#' Columns whose value labels differ between cycles in wording only
# The 2005 PUF labels `te14` code 93 "doesn't have access to a PC", but its
# questionnaire item QT05_E16 offers "doesn't have TV", as 2007 and 2009 do.
bp2387_same_meaning <- c(
  "agegrp_a",
  "cg10",
  "cg11",
  "fv5day",
  "sch_typ",
  "te14"
)

#' Make each column's value labels agree across the parts before binding
bp2387_align_value_labels <- function(parts) {
  for (col in unique(unlist(lapply(parts, names)))) {
    has <- vapply(
      parts,
      function(p) col %in% names(p) && haven::is.labelled(p[[col]]),
      logical(1)
    )
    if (sum(has) < 2L) {
      next
    }
    maps <- lapply(parts[has], function(p) attr(p[[col]], "labels"))
    if (length(unique(maps)) == 1L) {
      next
    }
    labels <- bp2387_unify_labels(maps, col %in% bp2387_same_meaning)
    parts[has] <- lapply(parts[has], function(p) {
      p[[col]] <- bp2387_relabel(p[[col]], labels)
      p
    })
  }
  parts
}

#' One value-label map for every part, or NULL when the codings disagree
bp2387_unify_labels <- function(maps, wording_only) {
  codes <- unique(unlist(lapply(maps, unname)))
  texts <- lapply(codes, function(code) {
    unlist(lapply(maps, function(m) {
      if (code %in% m) names(m)[match(code, m)] else NULL
    }))
  })
  disputed <- vapply(
    texts,
    function(x) length(unique(toupper(gsub("[^[:alnum:]]+", "", x)))) > 1L,
    logical(1)
  )
  # Negative codes are CHIS's missing reserve, worded differently by cycle.
  missing_code <- is.numeric(codes) & codes < 0
  if (!wording_only && any(disputed & !missing_code)) {
    return(NULL)
  }
  # Parts arrive in cycle order, so the latest cycle's wording wins.
  stats::setNames(
    codes,
    vapply(texts, function(x) x[[length(x)]], character(1))
  )
}

#' Give each column the variable label of the latest part that has one
bp2387_restore_labels <- function(df, parts) {
  # `bind_rows()` drops a variable label that differs between parts.
  for (col in names(df)) {
    labels <- unlist(lapply(parts, function(p) {
      if (col %in% names(p)) attr(p[[col]], "label") else NULL
    }))
    if (length(labels) > 0L) {
      attr(df[[col]], "label") <- labels[[length(labels)]]
    }
  }
  df
}

#' Put a value-label map on a column, or take one off, keeping its label
bp2387_relabel <- function(x, labels) {
  label <- attr(x, "label")
  out <- if (is.null(labels)) {
    haven::zap_labels(x)
  } else {
    haven::labelled(as.vector(x), labels = labels)
  }
  attr(out, "label") <- label
  out
}
