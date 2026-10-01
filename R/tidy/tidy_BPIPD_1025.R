#' Tidier for BPIPD-1025 (1970 British Cohort Study)
#'
#' Assembles the two sweeps with a child participant and a screen-time
#' duration item: the cohort member at 16 (1986, four releases) and their
#' children in the 2004 Parent & Child survey, each with its derived and
#' supplementary files joined on.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant per sweep (`wave`)
tidy_BPIPD_1025 <- function(raw_dataset, spec) {
  # A UKDA study number is a release, not a sweep, so `wave` replaces `.wave`.
  # The age-10 (TV frequency only) and adult sweeps are left out.
  df <- dplyr::bind_rows(
    bp1025_age16(raw_dataset) |> bp1025_add_age10_items(raw_dataset),
    bp1025_child_2004(raw_dataset)
  )

  attr(df$participant_id, "label") <-
    "BCSID, with the child number appended for a Parent & Child row"
  attr(df$wave, "label") <- "BCS70 sweep"
  df <- bp1025_parent_qualifications(df)
  df <- bp1025_sdq_parent_items(df)

  if (anyDuplicated(df[c("participant_id", "wave")]) > 0) {
    stop(
      "BPIPD-1025: `participant_id` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' The cohort member at 16, from the 1986 sweep and its three later releases
bp1025_age16 <- function(raw_dataset) {
  add <- function(base, name) {
    bp1025_add(base, bp1025_take(raw_dataset, name), "bcsid")
  }

  bp1025_take(raw_dataset, "bcs3535_bcs7016x_sav") |>
    add("bcs3535_bcs4derived_sav") |>
    # The same school-type file also ships with study 7473; that copy is unused.
    add("bcs3535_bcs70_age16_school_type_sav") |>
    add("bcs3535_jiig_cal_occupational_interests_sav") |>
    add("bcs6095_bcs70_16_year_arithmetic_data_sav") |>
    add("bcs8288_bcs1986_reading_matrices_sav") |>
    add("bcs8949_bcs4_leisure_diary_aggregate_sav") |>
    dplyr::mutate(participant_id = bcsid, wave = "age16", .before = 1)
}

#' Attach the 1980 answers on characteristics that do not change
bp1025_add_age10_items <- function(age16, raw_dataset) {
  # Parents' qualifications (always carried), the child's ethnic group and
  # country of birth; no other 1980 item is used once that sweep is dropped.
  items <- c(paste0("c1.", seq_len(22)), "a12.1", "a12.4", "a12.7", "a2.1")
  age10 <- bp1025_take(raw_dataset, "bcs3723_sn3723_sav")[c("bcsid", items)]
  for (column in items) {
    attr(age10[[column]], "label") <- paste0(
      attr(age10[[column]], "label"),
      " [1980 sweep, carried to the age-16 row]"
    )
  }
  dplyr::left_join(age16, age10, by = "bcsid", relationship = "one-to-one")
}

#' A cohort member's child in 2004, from the Parent & Child survey
bp1025_child_2004 <- function(raw_dataset) {
  # Each paper form numbers its questions from one, hence the prefix.
  child <- function(name, prefix = "") {
    tbl <- bp1025_child_key(bp1025_take(raw_dataset, name))
    keys <- names(tbl) %in% c("bcsid", "b7chdid")
    names(tbl) <- ifelse(keys, names(tbl), paste0(prefix, names(tbl)))
    tbl
  }
  by_child <- c("bcsid", "b7chdid")

  parent <- bp1025_add(
    bp1025_take(raw_dataset, "bcs5585_bcs_2004_followup_sav"),
    bp1025_take(raw_dataset, "bcs5585_bcs7derived_sav"),
    "bcsid"
  )

  # The parent interview defines the sample; the cohort member's 2004 interview
  # rides along as the parent's characteristics.
  child("bcs5585_bcs_2004_parent_and_child_sav") |>
    bp1025_add(child("bcs5585_bcs_2004_child_assessment_bas_sav"), by_child) |>
    bp1025_add(child("bcs5585_bcs_2004_pc_yp_10to16_sav", "yp_"), by_child) |>
    bp1025_add(
      child("bcs5585_bcs_2004_pc_0to11mths_sav", "pc0to11_"),
      by_child
    ) |>
    bp1025_add(
      child("bcs5585_bcs_2004_pc_1to2yr11mths_sav", "pc1to2_"),
      by_child
    ) |>
    bp1025_add(
      child("bcs5585_bcs_2004_pc_3to5yr11mths_sav", "pc3to5_"),
      by_child
    ) |>
    bp1025_add(
      child("bcs5585_bcs_2004_pc_6to16yr_sav", "pc6to16_"),
      by_child
    ) |>
    bp1025_add(parent, "bcsid", relationship = "many-to-one") |>
    bp1025_partner_natural_parent() |>
    dplyr::mutate(
      participant_id = paste0(bcsid, "_", b7chdid),
      wave = "child2004",
      .before = 1
    )
}

#' Whether the cohort member's partner is this child's natural parent
bp1025_partner_natural_parent <- function(tbl) {
  # The household grid asks it once per person number; the child's number is
  # `b7chdid` (2 to 10), so pick that person's answer.
  grid <- sapply(
    sprintf("b7wpar%02d", 2:10),
    function(column) as.numeric(haven::zap_labels(tbl[[column]]))
  )
  grid <- matrix(grid, nrow = nrow(tbl))
  tbl$partner_natural_parent <- haven::labelled(
    grid[cbind(seq_len(nrow(tbl)), tbl$b7chdid - 1L)],
    c(Yes = 1, No = 2),
    label = paste(
      "Whether the cohort member's spouse or partner is this child's natural",
      "parent (b7wpar at the child's household person number)"
    )
  )
  tbl
}

#' Name the child number `b7chdid` and strip its value labels
bp1025_child_key <- function(tbl) {
  names(tbl)[names(tbl) %in% c("ChildID", "ch")] <- "b7chdid"
  # Each form labels only its own child numbers, which blocks the join.
  tbl$b7chdid <- as.integer(haven::zap_labels(tbl$b7chdid))
  attr(tbl$b7chdid, "label") <- "Child number within the cohort member's family"
  tbl
}

#' One raw table, without the reader's wave columns and keyed on `bcsid`
bp1025_take <- function(raw_dataset, name) {
  tbl <- tibble::as_tibble(raw_dataset$data[[name]])
  tbl <- tbl[setdiff(names(tbl), c(".wave", ".wave_label"))]
  names(tbl)[names(tbl) == "BCSID"] <- "bcsid"
  tbl
}

#' Add a table's columns that `base` does not already have
bp1025_add <- function(base, tbl, by, relationship = "one-to-one") {
  tbl <- tbl[c(by, setdiff(names(tbl), names(base)))]
  dplyr::left_join(base, tbl, by = by, relationship = relationship)
}

#' Each parent's highest qualification, by role
bp1025_parent_qualifications <- function(df) {
  # Age 16: the 1986 grid, else the 1980 grid; 2004: the cohort member only.
  # Codes rise with the level; 6 and 7 are the two grids' differently worded top boxes.
  levels <- c(
    "No qualifications" = 0,
    "Trade apprenticeship" = 1,
    "O level or equivalent" = 2,
    "A level or equivalent" = 3,
    "Nursing qualification" = 4,
    "Teaching qualification" = 5,
    "Degree" = 6,
    "Degree, diploma or professional membership" = 7,
    "NVQ level 1-3 qualification" = 10,
    "Diploma or other NVQ level 4-5 qualification" = 11,
    "Degree or higher degree" = 12
  )
  num <- function(column) as.numeric(haven::zap_labels(df[[column]]))
  highest <- function(...) pmax(..., na.rm = TRUE)

  # 1980: a tick (1) per box; the 'other' box is classified into the same
  # boxes (9 = no qualifications) or left unranked.
  grid_1980 <- function(none, boxes, other) {
    ticked <- lapply(seq_along(boxes), function(i) {
      ifelse(num(boxes[i]) %in% 1, i, NA)
    })
    classified <- num(other)
    classified <- ifelse(
      classified %in% 1:6,
      classified,
      ifelse(classified %in% 9, 0, NA)
    )
    do.call(
      highest,
      c(list(ifelse(num(none) %in% 1, 0, NA), classified), ticked)
    )
  }
  # 1986: each box records father (1), mother (2) or both (3).
  grid_1986 <- function(codes) {
    boxes <- c(
      t6.9 = 0,
      t6.1 = 1,
      t6.2 = 2,
      t6.3 = 3,
      t6.4 = 4,
      t6.5 = 5,
      t6.6 = 7
    )
    ranks <- lapply(names(boxes), function(box) {
      ifelse(num(box) %in% codes, boxes[[box]], NA)
    })
    do.call(highest, ranks)
  }

  mother_1980 <- grid_1980("c1.20", paste0("c1.", 12:17), "c1.19")
  father_1980 <- grid_1980("c1.9", paste0("c1.", 1:6), "c1.8")
  mother_1986 <- grid_1986(c(2, 3))
  father_1986 <- grid_1986(c(1, 3))

  cohort_member <- dplyr::case_when(
    num("BD7HACHQ") %in% c(7, 8) ~ 12,
    num("BD7HNVQ") %in% c(4, 5) ~ 11,
    num("BD7HNVQ") %in% 1:3 ~ 10,
    num("BD7HNVQ") %in% 0 ~ 0
  )
  child <- df$wave == "child2004"
  mother <- ifelse(
    child,
    ifelse(num("bd7sex") %in% 2, cohort_member, NA),
    dplyr::coalesce(mother_1986, mother_1980)
  )
  father <- ifelse(
    child,
    ifelse(num("bd7sex") %in% 1, cohort_member, NA),
    dplyr::coalesce(father_1986, father_1980)
  )
  parents <- ifelse(
    child,
    NA,
    dplyr::coalesce(
      highest(mother_1986, father_1986),
      highest(mother_1980, father_1980)
    )
  )

  source <- paste(
    "(age 16: 1986 grid t6.*, else 1980 grid c1.*;",
    "2004: the cohort member's BD7HACHQ / BD7HNVQ)"
  )
  df$mother_highest_qual <- haven::labelled(
    mother,
    levels,
    label = paste("Mother figure's highest qualification", source)
  )
  df$father_highest_qual <- haven::labelled(
    father,
    levels,
    label = paste("Father figure's highest qualification", source)
  )
  df$parents_highest_qual <- haven::labelled(
    parents,
    levels,
    label = paste(
      "Higher of the mother and father figures' highest qualifications",
      "(age 16 only, from the same grids)"
    )
  )
  df
}

#' Parent SDQ items 1-25, pooled across the 3-5 and 6-16 paper forms
bp1025_sdq_parent_items <- function(df) {
  # Each child had one form. The 3-5 form numbers the items differently and
  # asks items 18, 21 and 22 in an under-4 (2-4 form wording) and a 4-5 version.
  forms <- list(
    c("pc6to16_q1a", "pc3to5_q1a"),
    c("pc6to16_q1b", "pc3to5_q1b"),
    c("pc6to16_q1c", "pc3to5_q1c"),
    c("pc6to16_q1d", "pc3to5_q1d"),
    c("pc6to16_q1e", "pc3to5_q1e"),
    c("pc6to16_q1f", "pc3to5_q1f"),
    c("pc6to16_q1g", "pc3to5_q1g"),
    c("pc6to16_q1h", "pc3to5_q1h"),
    c("pc6to16_q1i", "pc3to5_q1i"),
    c("pc6to16_q1j", "pc3to5_q1j"),
    c("pc6to16_q1k", "pc3to5_q1k"),
    c("pc6to16_q1l", "pc3to5_q1l"),
    c("pc6to16_q1m", "pc3to5_q1m"),
    c("pc6to16_q1n", "pc3to5_q1n"),
    c("pc6to16_q1o", "pc3to5_q1o"),
    c("pc6to16_q1p", "pc3to5_q1p"),
    c("pc6to16_q1q", "pc3to5_q1q"),
    c("pc6to16_q1r", "pc3to5_q3a", "pc3to5_q4a"),
    c("pc6to16_q1s", "pc3to5_q1r"),
    c("pc6to16_q1t", "pc3to5_q1s"),
    c("pc6to16_q1u", "pc3to5_q3b", "pc3to5_q4b"),
    c("pc6to16_q1v", "pc3to5_q3c", "pc3to5_q4c"),
    c("pc6to16_q1w", "pc3to5_q1t"),
    c("pc6to16_q1x", "pc3to5_q1u"),
    c("pc6to16_q1y", "pc3to5_q1v")
  )
  for (item in seq_along(forms)) {
    columns <- forms[[item]]
    values <- lapply(columns, function(column) {
      x <- as.numeric(haven::zap_labels(df[[column]]))
      ifelse(x %in% 1:3, x, NA)
    })
    df[[sprintf("sdq_p%02d", item)]] <- haven::labelled(
      do.call(dplyr::coalesce, values),
      c("Not true" = 1, "Somewhat true" = 2, "Certainly true" = 3),
      label = sprintf(
        "SDQ parent item %d: %s (3-5 and 6-16 forms pooled: %s)",
        item,
        sub(" over the past 6 months.*$", "", attr(df[[columns[1]]], "label")),
        paste(columns, collapse = ", ")
      )
    )
  }
  df
}
