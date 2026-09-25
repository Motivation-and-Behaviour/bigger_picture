#' Tidier for BPIPD-1612 (Konok & Szőke 2022)
#'
#' Two-wave parent-report study. T1 cross-section (one row/child) plus a
#' longitudinal sheet holding the T1 and T2 blocks of children followed up
#' at T2, side by side. T1 rows come from the cross-section, T2 rows from
#' the longitudinal sheet, linked on the T1 answers it repeats.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per wave (T1, T2)
tidy_BPIPD_1612 <- function(raw_dataset, spec) {
  maps <- bp1612_columns()

  # No identifier in either sheet; id = sheet first seen in (`d`/`l`) + row number.
  t1 <- bp1612_take(raw_dataset$data$data_t1, maps$cross_section)
  t1 <- tibble::add_column(
    t1,
    participant_id = sprintf("d%03d", seq_len(nrow(t1))),
    wave = "T1",
    .before = 1L
  )

  wide <- raw_dataset$data$data
  t2 <- bp1612_take(wide, maps$T2)
  t2 <- tibble::add_column(
    t2,
    participant_id = bp1612_link(bp1612_take(wide, maps$T1), t1),
    wave = "T2",
    .before = 1L
  )

  long <- dplyr::bind_rows(t1, t2)
  long <- bp1612_label_columns(long, raw_dataset$codebook$codebook, maps$T2)

  if (anyDuplicated(long[c("participant_id", "wave")]) > 0) {
    stop(
      "BPIPD-1612: `participant_id` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  long
}

#' Source column for each tidied column, per sheet block
#'
#' Each block names the same construct differently (`Child_age` in the
#' cross-section, `Child_age_T1` and `Child age_T2` in the longitudinal
#' sheet), so each gets its own select/rename map; tidied names follow the
#' cross-section. Household composition, the responding parent's own
#' education and the unreversed copies of the five reversed SDQ items are
#' T2-only.
bp1612_columns <- function() {
  shared <- c(
    "Child_age",
    "Child_sex",
    "Parent_age",
    "Edu_parents",
    "Use_anydevice",
    "deviceminperday"
  )
  scores <- c("emo", "conduct", "hiper", "peer", "prosoc", "sum")
  items <- c(
    "SDQ_1_pr",
    "SDQ_2_hip",
    "SDQ_3_emo",
    "SDQ_4_pr",
    "SDQ_5_cond",
    "SDQ_6_peer",
    "SDQ_7_cond_rev",
    "SDQ_8_emo",
    "SDQ_9_pr",
    "SDQ_10_hip",
    "SDQ_11_peer_rev",
    "SDQ_12_cond",
    "SDQ_13_emo",
    "SDQ_14_peer_rev",
    "SDQ_15_hip",
    "SDQ_16_emo",
    "SDQ_17_pr",
    "SDQ_18_cond",
    "SDQ_19_peer",
    "SDQ_20_pr",
    "SDQ_21_hip_rev",
    "SDQ_22_cond",
    "SDQ_23_peer",
    "SDQ_24_emo",
    "SDQ_25_hip_rev"
  )

  # Cross-section drops the `SDQ_` prefix, calls conduct `beh`; T2 suffixes
  # every item, lower-casing the suffix on item 13.
  items_cross <- sub("_cond", "_beh", sub("^SDQ_", "", items))
  items_t2 <- paste0(items, "_T2")
  items_t2[items == "SDQ_13_emo"] <- "SDQ_13_emo_t2"

  list(
    cross_section = c(
      stats::setNames(shared, shared),
      stats::setNames(items_cross, items),
      stats::setNames(scores, scores)
    ),
    T1 = c(
      stats::setNames(paste0(shared, "_T1"), shared),
      stats::setNames(items, items),
      stats::setNames(paste0(scores, "_T1"), scores)
    ),
    T2 = c(
      Child_age = "Child age_T2",
      Child_sex = "Child sex",
      Parent_age = "Respondent age_T2",
      Edu_parents = "Parents_edu_mean_T2",
      Use_anydevice = "use_anydevice_T2",
      deviceminperday = "deviceminperday_T2",
      stats::setNames(items_t2, items),
      emo = "emo_score_T2",
      conduct = "cond_score_T2",
      hiper = "hiper_score_T2",
      peer = "peer_score_T2",
      prosoc = "prosoc_score_T2",
      sum = "SDQ_sum_T2",
      Respondant_edu = "Respondant edu_T2",
      Spouse_edu = "Spouse_edu_T2",
      Nr_of_mobile = "Nr_of_mobile_T2",
      Nr_of_tablets = "Nr_of_tablets_T2",
      Nr_of_people_in_household = "Nr_of_people_in_household_T2",
      Nr_of_people_under_18 = "Nr_of_people_under_18_T2",
      SDQ_7_cond = "SDQ_7_cond_T2",
      SDQ_11_peer = "SDQ_11_peer_T2",
      SDQ_14_peer = "SDQ_14_peer_T2",
      SDQ_21_hip = "SDQ_21_hip_T2",
      SDQ_25_hip = "SDQ_25_hip_T2"
    )
  )
}

#' Select one sheet's block under the tidied names
bp1612_take <- function(df, map) {
  dplyr::select(tibble::as_tibble(df), dplyr::all_of(map))
}

#' Id of the cross-section row each longitudinal row belongs to
#'
#' The longitudinal sheet repeats all 37 T1 answers, distinct across the
#' cross-section's children, so the sheets link on them despite neither
#' holding an identifier. The 10 children the authors couldn't match to
#' their T1 questionnaire (Konok & Szőke 2022, Section 2.1) have an empty
#' T1 block; they're in the cross-section too, but nothing says where, so
#' they become children of their own.
bp1612_link <- function(block, roster) {
  seen <- rowSums(!is.na(block)) > 0L

  linked <- dplyr::left_join(
    tibble::tibble(.key = bp1612_key(block[seen, ])),
    tibble::tibble(
      .key = bp1612_key(roster[names(block)]),
      participant_id = roster$participant_id
    ),
    by = ".key",
    relationship = "one-to-one"
  )

  if (anyNA(linked$participant_id)) {
    stop(
      "BPIPD-1612: a T1 record in the longitudinal sheet has no matching row ",
      "in `data_T1`.",
      call. = FALSE
    )
  }

  ids <- sprintf("l%03d", seq_along(seen))
  ids[seen] <- linked$participant_id
  ids
}

#' A row's T1 answers as one string
bp1612_key <- function(df) {
  do.call(paste, c(lapply(df, function(x) sprintf("%.6f", x)), list(sep = "|")))
}

#' Label the tidied columns from the workbook's `variables_longit` sheet
#'
#' Data sheets carry no labels; the codebook describes each construct
#' against its T2 column only, so every column takes that T2 description
#' with the trailing wave phrase trimmed.
bp1612_label_columns <- function(df, codebook_path, t2_map) {
  codebook <- readxl::read_excel(
    codebook_path,
    sheet = "variables_longit",
    .name_repair = "minimal"
  )
  meanings <- stats::setNames(
    as.character(codebook[["Meaning"]]),
    as.character(codebook[["Shot name"]])
  )

  labels <- stats::setNames(
    sub("\\s*(\\(T2\\)|in T2|at T2)$", "", meanings[t2_map]),
    names(t2_map)
  )

  for (column in names(labels)) {
    attr(df[[column]], "label") <- labels[[column]]
  }

  df
}
