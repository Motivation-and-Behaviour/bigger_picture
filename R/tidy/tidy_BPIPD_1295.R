#' Tidier for BPIPD-1295 (High School and Beyond)
#'
#' Stacks one slice per wave: 1980 from the base-year files, 1982 from the
#' first follow-up files without their repeated base-year block, and 1984 from
#' the columns only the 08896 release carries; the 1986 wave asks no screen item.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per student per wave attended (1980, 1982, 1984)
tidy_BPIPD_1295 <- function(raw_dataset, spec) {
  data <- raw_dataset$data
  sav <- lapply(
    list(
      sophomore = data$hsb_1980_1986_sophomore,
      senior = data$hsb_1980_1986_senior
    ),
    function(tbl) {
      tbl$ID <- sprintf("%06d", as.integer(haven::zap_labels(tbl$ID)))
      tibble::as_tibble(tbl)
    }
  )
  value_labels <- bp1295_value_labels(sav)
  fwf <- function(name) {
    bp1295_fwf(data[[name]], bp1295_layout(name, spec), value_labels)
  }

  # The school file's BYPART and FU1PART flag the school, not the student.
  school <- fwf("hsb_1982_school")
  stratum <- school[c("SCHLID", "SCHSAMP")]
  school <- school[!names(school) %in% c("SCHSAMP", "BYPART", "FU1PART")]
  # FS* is the 1982 school questionnaire, which seniors and leavers never saw.
  school <- school[!grepl("^FS", names(school))]

  # Every case of the 1980 student file completed the base-year survey.
  y1980 <- fwf("hsb_1980_student") |>
    dplyr::rename(ID = CASEID) |>
    dplyr::mutate(
      wave = "Base year 1980",
      cohort = GRADE,
      school_id = substr(ID, 1, 4)
    ) |>
    dplyr::left_join(
      dplyr::rename(fwf("hsb_1980_language"), ID = CASEID),
      by = "ID",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      school,
      by = c(school_id = "SCHLID"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      fwf("hsb_1982_labour_market"),
      by = c(school_id = "SCHOOLID"),
      relationship = "many-to-one"
    )

  # Setup block 2 of each 1982 file repeats the base-year items of 1980.
  fu1_items <- function(name, cohort) {
    layout <- bp1295_layout(name, spec)
    tbl <- fwf(name)
    tbl <- tbl[setdiff(layout$name[layout$setup_block != 2L], "SCHSAMP")]
    tbl$cohort <- cohort
    tbl[haven::zap_labels(tbl$FU1PART) == 1, , drop = FALSE]
  }
  y1982 <- bp1295_bind(list(
    fu1_items("hsb_1982_sophomore", 1),
    fu1_items("hsb_1982_senior", 2)
  ))
  y1982$wave <- "First follow-up 1982"

  # The 1982 and 1980 files hold every earlier item the .sav files repeat.
  earlier <- unique(c(
    names(data$hsb_1980_student),
    names(data$hsb_1982_sophomore),
    names(data$hsb_1982_senior)
  ))
  # The 1986 third follow-up asks no screen-use item, so its columns go.
  sav_only <- lapply(sav, function(tbl) {
    keep <- setdiff(names(tbl), earlier)
    keep[!grepl("^(TY|TE)[0-9]", keep) & !keep %in% bp1295_fu3_columns()]
  })
  fu1_late <- grep("^(FY|FD|FT|FG|FE)[0-9]", sav_only$sophomore, value = TRUE)
  y1982 <- dplyr::left_join(
    y1982,
    sav$sophomore[c("ID", fu1_late)],
    by = "ID",
    relationship = "one-to-one"
  )

  y1984 <- bp1295_wave_1984(sav, sav_only, fu1_late)

  df <- bp1295_bind(list(y1980, y1982, y1984))
  df$school_id <- substr(df$ID, 1, 4)
  df$SCHSAMP <- stratum$SCHSAMP[match(df$school_id, stratum$SCHLID)]

  # Augmentation twins (TWINDATA 2) are not sample members; blanks are zeros.
  twins <- data$hsb_1980_twins_siblings
  twins <- twins[twins$TWINDATA %in% c(1, 3), , drop = FALSE]
  family <- match(df$ID, gsub(" ", "0", twins$CASEID, fixed = TRUE))
  df$FAMID <- twins$FAMID[family]
  df$SETTYPE <- twins$SETTYPE[family]

  df <- bp1295_fill(df)
  df <- dplyr::relocate(
    df,
    dplyr::all_of(c(
      "ID",
      "wave",
      "cohort",
      "school_id",
      "SCHSAMP",
      "FAMID",
      "SETTYPE",
      "father_education",
      "mother_education"
    ))
  )

  if (anyDuplicated(df[c("ID", "wave")]) > 0L) {
    stop(
      "BPIPD-1295: `ID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }
  bp1295_label_built(df)
}

#' Stack the two cohorts' 1984 columns from the 08896 .sav files
bp1295_wave_1984 <- function(sav, keep, drop) {
  slices <- Map(
    function(tbl, keep, cohort, suffix) {
      tbl <- tbl[c("ID", setdiff(keep, drop))]
      # The two cohorts' income composites band the same codes differently.
      income <- c("FAMINC", "FAMINC2")
      names(tbl)[match(income, names(tbl))] <- paste0(income, suffix)
      tbl$cohort <- cohort
      tbl[haven::zap_labels(tbl$FU2PART) == 1, , drop = FALSE]
    },
    sav,
    keep,
    c(1, 2),
    c("_sophomore", "_senior")
  )

  # The other shared composites differ only in label wording; keep one wording.
  for (column in intersect(names(slices$sophomore), names(slices$senior))) {
    first <- attr(slices$sophomore[[column]], "labels", exact = TRUE)
    second <- attr(slices$senior[[column]], "labels", exact = TRUE)
    if (!is.null(first) && !is.null(second)) {
      attr(slices$senior[[column]], "labels") <- c(
        first,
        second[!second %in% first]
      )
    }
  }

  y1984 <- bp1295_bind(slices)
  y1984$wave <- "Second follow-up 1984"
  dated <- bp1295_sav_dated()
  for (column in names(dated)) {
    attr(y1984[[column]], "label") <- paste0(
      attr(y1984[[column]], "label"),
      " (",
      dated[[column]],
      ")"
    )
  }
  y1984
}

#' Carry parent education within student across the waves
bp1295_fill <- function(df) {
  order <- match(
    df$wave,
    c("Base year 1980", "First follow-up 1982", "Second follow-up 1984")
  )

  # Codes 2-10 are education levels and the only values carried to other rows.
  # Code 1 (does not live with the parent) is an answer and is kept.
  sources <- list(
    father_education = c("BB039", "FY55", "FE20"),
    mother_education = c("BB042", "FY56", "FE21")
  )
  for (stem in names(sources)) {
    x <- do.call(
      dplyr::coalesce,
      unname(lapply(df[sources[[stem]]], haven::zap_labels))
    )
    level <- x
    level[!level %in% 2:10] <- NA
    level <- carry_within(level, df$ID, order)
    # Don't know (11) and reserved codes 96-98 are not answers, so they fill.
    fill <- (is.na(x) | x %in% c(11, 96:98)) & !is.na(level)
    x[fill] <- level[fill]
    df[[stem]] <- haven::labelled(
      x,
      labels = attr(df[[sources[[stem]][[1]]]], "labels", exact = TRUE)
    )
  }
  df
}

#' Label the columns the tidier builds
bp1295_label_built <- function(df) {
  labels <- c(
    ID = "Student ID (base-year CASEID, follow-up ID)",
    wave = "HS&B wave of the row",
    cohort = "Cohort: 1980 grade (1 sophomore, 2 senior)",
    school_id = "Base-year school (first four characters of ID)",
    SCHSAMP = "Original school sample type, from the school file",
    FAMID = "Twin or sibling family ID (sampled twins and siblings)",
    SETTYPE = "Type of twin or sibling set (twins file)",
    father_education = paste(
      "Father's education: BB039 1980, FY55 and FE20 1982; rows with no item,",
      "don't know (11) or a reserved code (96-98) take the earliest of 2-10"
    ),
    mother_education = paste(
      "Mother's education: BB042 1980, FY56 and FE21 1982; rows with no item,",
      "don't know (11) or a reserved code (96-98) take the earliest of 2-10"
    )
  )
  for (column in names(labels)) {
    attr(df[[column]], "label") <- labels[[column]]
  }
  df$cohort <- haven::labelled(
    as.numeric(haven::zap_labels(df$cohort)),
    labels = c(Sophomore = 1, Senior = 2),
    label = labels[["cohort"]]
  )
  df
}

#' Rescale and label one fixed-width table from its layout
bp1295_fwf <- function(tbl, layout, value_labels) {
  tbl <- tibble::as_tibble(tbl)
  # Value labels come from the same-named .sav columns.
  tbl[layout$name] <- lapply(seq_len(nrow(layout)), function(i) {
    x <- tbl[[layout$name[[i]]]]
    if (!is.na(layout$rescale_decimals[[i]])) {
      x <- x / 10^layout$rescale_decimals[[i]]
    }
    labels <- value_labels[[layout$name[[i]]]]
    if (is.numeric(x) && !is.null(labels)) {
      x <- haven::labelled(x, labels = labels)
    }
    if (!is.na(layout$label[[i]])) {
      attr(x, "label") <- layout$label[[i]]
    }
    x
  })
  tbl
}

#' Read the layout CSV a fixed-width resource was read with
bp1295_layout <- function(name, spec) {
  resource <- Filter(function(r) identical(r$name, name), spec$resources)[[1]]
  layout <- readr::read_csv(
    resolve_col_positions(resource$col_positions, spec),
    col_types = readr::cols(.default = readr::col_character()),
    na = ""
  )
  layout$setup_block <- as.integer(layout$setup_block)
  layout$rescale_decimals <- as.integer(layout$rescale_decimals)
  layout
}

#' Value labels of the .sav columns, where the two files label codes alike
bp1295_value_labels <- function(sav) {
  per_file <- lapply(sav, function(tbl) {
    labels <- lapply(tbl, attr, "labels", exact = TRUE)
    labels[!vapply(labels, is.null, logical(1))]
  })
  shared <- intersect(names(per_file$sophomore), names(per_file$senior))
  # A code labelled differently in the two files (BB044 is reversed) is unsafe.
  conflicting <- shared[
    !vapply(
      shared,
      function(column) {
        first <- per_file$sophomore[[column]]
        second <- per_file$senior[[column]]
        common <- intersect(first, second)
        identical(
          names(first)[match(common, first)],
          names(second)[match(common, second)]
        )
      },
      logical(1)
    )
  ]
  labels <- c(
    per_file$sophomore,
    per_file$senior[setdiff(names(per_file$senior), shared)]
  )
  for (column in setdiff(shared, conflicting)) {
    second <- per_file$senior[[column]]
    labels[[column]] <- c(
      labels[[column]],
      second[!second %in% labels[[column]]]
    )
  }
  # BB081E is recoded in the .sav files, so its labels do not fit the 1980 file.
  labels[setdiff(names(labels), c(conflicting, "BB081E"))]
}

#' Bind tables, restoring the variable labels bind_rows() drops from plain columns
bp1295_bind <- function(tables) {
  labels <- unlist(lapply(unname(tables), function(tbl) {
    lapply(tbl, attr, "label", exact = TRUE)
  }))
  labels <- labels[!duplicated(names(labels))]
  out <- dplyr::bind_rows(tables)
  for (column in names(labels)) {
    if (is.null(attr(out[[column]], "label", exact = TRUE))) {
      attr(out[[column]], "label") <- labels[[column]]
    }
  }
  out
}

#' Third follow-up (1986) columns of the .sav files without a TY or TE prefix
bp1295_fu3_columns <- function() {
  # EDATTAIN, FAMFOR86 and DECFLAG are built from 1986 data (08896 codebook).
  c(
    "TYMETHOD",
    "TEMETHOD",
    "FU3PART",
    "FU3STAT",
    "FU3WT",
    "TESTWT3",
    "PANELWT4",
    "RAWWT3",
    "EDATTAIN",
    "FAMFOR86",
    "DECFLAG"
  )
}

#' Occasion of the .sav composites on the 1984 rows that date from earlier
bp1295_sav_dated <- function() {
  c(
    FAMINC_sophomore = "base-year parent or student report",
    FAMINC_senior = "base-year parent or student report",
    FAMINC2_sophomore = "base-year parent or student report",
    FAMINC2_senior = "base-year parent or student report",
    HOMEOWN = "base-year parent or student report",
    DRQFLAG = "1982 dropout questionnaire",
    SESQ = "1982 SES quartile for sophomores, base year for seniors",
    TESTQ = "1982 test quartile for sophomores, base year for seniors"
  )
}
