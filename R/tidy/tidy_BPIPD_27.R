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
  sweeps <- lapply(spec$waves, function(wave) tidy_mcs_sweep(raw_dataset, wave))
  df <- dplyr::bind_rows(mcs_prefix_shared_columns(sweeps))

  if (anyDuplicated(df[c("participant_id", ".wave")]) > 0) {
    stop(
      "BPIPD-27: `participant_id` and `.wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' Assemble one sweep into a cohort-member table
tidy_mcs_sweep <- function(raw_dataset, wave) {
  modules <- function(...) mcs_modules(raw_dataset, wave$wave, ...)

  cm <- mcs_cognitive_scores(
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
      mcs_add(base, tbl, mcs_cm_key(tbl), dplyr::full_join, "one-to-one")
    },
    cm
  )
  key <- mcs_cm_key(base)

  grid <- modules("hhgrid")

  for (tbl in grid) {
    base <- mcs_add(
      base,
      mcs_hhgrid_cohort_members(tbl),
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
    base <- mcs_add(base, tbl, "MCSID")
  }

  # `parent_derived` carries the parental education variables
  roster_sex <- mcs_person_sex(grid)
  for (tbl in modules("parent_derived")) {
    if (!is.null(roster_sex)) {
      tbl <- mcs_add(tbl, roster_sex, c("MCSID", mcs_person_key(tbl)))
    }
    base <- mcs_add(base, mcs_respondent(tbl, 1L, "MCSID"), "MCSID")
    base <- mcs_add(
      base,
      mcs_respondent(tbl, 2L, "MCSID", suffix = "_PARTNER"),
      "MCSID"
    )
  }

  for (tbl in modules("parent_cm_interview")) {
    base <- mcs_add(
      base,
      mcs_respondent(tbl, 1L, key),
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

#' Give sweep-specific names to columns MCS left unprefixed
mcs_prefix_shared_columns <- function(sweeps) {
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

    prefixed <- paste0(substr(mcs_cm_key(tbl)[2], 1, 1), rename)
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
mcs_modules <- function(raw_dataset, wave, modules = NULL, extra = NULL) {
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
    tbl[ids] <- lapply(tbl[ids], mcs_plain)

    tbl[, setdiff(names(tbl), c(".wave", ".wave_label"))]
  })
}

#' Keep the cohort members' own rows from a household grid
mcs_hhgrid_cohort_members <- function(tbl) {
  cnum <- tbl[[mcs_cm_key(tbl)[2]]]
  tbl[!is.na(cnum) & cnum >= 1, ]
}

#' Each household member's sex, from the rest of the household grid
mcs_person_sex <- function(grids) {
  if (length(grids) == 0) {
    return(NULL)
  }

  tbl <- grids[[1]]
  cnum <- tbl[[mcs_cm_key(tbl)[2]]]
  sex <- grep("PSEX", names(tbl), value = TRUE)
  if (length(sex) != 1) {
    stop(
      "BPIPD-27: expected one household-grid sex column, found ",
      length(sex),
      ".",
      call. = FALSE
    )
  }

  roster <- tbl[is.na(cnum) | cnum < 1, c("MCSID", mcs_person_key(tbl), sex)]
  roster[[sex]] <- mcs_plain(roster[[sex]])
  names(roster)[3] <- "respondent_sex"
  roster
}

#' The `*PNUM00` person-number column for a table, e.g. `"BPNUM00"`
mcs_person_key <- function(tbl) {
  pnum <- grep("^.PNUM00$", names(tbl), value = TRUE)
  if (length(pnum) == 0) {
    stop("BPIPD-27: no `*PNUM00` column to key on.", call. = FALSE)
  }

  pnum[1]
}

#' Strip an identifier or flag column back to a plain vector
mcs_plain <- function(x) {
  x <- unclass(x)
  attributes(x) <- NULL
  if (is.character(x)) x else as.integer(x)
}

#' Drop item-level responses from the cognitive assessment
mcs_cognitive_scores <- function(tables, wave) {
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
    tbl[, grepl(keep, names(tbl)) | names(tbl) %in% mcs_cm_key(tbl)]
  })

  tables
}

#' The family plus cohort-member key for a table, e.g. `c("MCSID", "FCNUM00")`
mcs_cm_key <- function(tbl) {
  cnum <- grep("^.CNUM00$", names(tbl), value = TRUE)
  if (length(cnum) == 0) {
    stop("BPIPD-27: no `*CNUM00` column to key on.", call. = FALSE)
  }

  c("MCSID", cnum[1])
}

#' Keep one respondent's rows from a parent-level table
mcs_respondent <- function(tbl, code, by, suffix = NULL) {
  resp <- grep("RESP00$", names(tbl), value = TRUE)
  pnum <- grep("^.PNUM00$", names(tbl), value = TRUE)

  tbl <- if (length(resp) > 0) {
    flag <- mcs_plain(tbl[[resp[1]]])
    tbl[!is.na(flag) & flag == code, ]
  } else if (length(pnum) > 0) {
    ordered <- tbl[order(mcs_plain(tbl[[pnum[1]]])), ]
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
mcs_add <- function(
  base,
  tbl,
  by,
  join = dplyr::left_join,
  relationship = "many-to-one"
) {
  tbl <- tbl[, c(by, setdiff(names(tbl), names(base)))]
  join(base, tbl, by = by, relationship = relationship)
}
