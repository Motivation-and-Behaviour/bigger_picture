#' Baseline-only columns: the Demographics module and the study's family indices
bp911_background <- c(
  "genderC_1",
  "parentage_1",
  "agechild_1",
  "genderP_1",
  "raceP_1",
  "EDUp_1",
  "INCOME_1",
  "MARITAL_1",
  "x2_1",
  "poverty",
  "edu_d",
  "single",
  "SES",
  "BMI",
  "RACE"
)

#' Baseline columns carried to the follow-ups, with the rule each follows
bp911_carried <- c(
  EDUp_1 = "parent education, always",
  edu_d = "parent education, always",
  INCOME_1 = "household SES, gap of 12 months or less",
  poverty = "household SES, gap of 12 months or less",
  SES = "household SES, gap of 12 months or less",
  MARITAL_1 = "family structure, gap of 12 months or less",
  x2_1 = "family structure, gap of 12 months or less",
  single = "family structure, gap of 12 months or less"
)

#' The fullest variable label each wave-suffixed stem carries
bp911_stem_labels <- function(df, wave_cols) {
  vapply(
    split(wave_cols, sub("_(1|4|8|12)$", "", wave_cols)),
    function(cols) {
      found <- unlist(lapply(df[cols], attr, "label"))
      if (length(found) == 0L) {
        NA_character_
      } else {
        found[[which.max(nchar(found))]]
      }
    },
    character(1)
  )
}

#' Write each named label onto the column of the same name
bp911_set_labels <- function(df, labels) {
  labels <- labels[!is.na(labels)]
  for (nm in names(labels)) {
    attr(df[[nm]], "label") <- labels[[nm]]
  }
  df
}

#' Tidier for BPIPD-911 (Rojo-Wissar, MAP Study Stage 3)
#'
#' One wide SPSS file with the assessment as a column suffix: `_1` baseline,
#' `_4`, `_8`, `_12` the month of follow-up (Stage 3 Measures List).
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per assessment attended
tidy_BPIPD_911 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  # `ST_week`/`ST_weekend` are labelled W1; `TECH_child_2` (n = 7) has no
  # documented occasion, so it is kept at baseline rather than as a wave.
  df <- dplyr::rename(
    df,
    ST_week_1 = "ST_week",
    ST_weekend_1 = "ST_weekend",
    TECH_child_7day_1 = "TECH_child_2"
  )

  wave_cols <- setdiff(
    grep("_(1|4|8|12)$", names(df), value = TRUE),
    bp911_background
  )
  stem_labels <- bp911_stem_labels(df, wave_cols)

  # Wave 1's CSHQ items dropped the `{targetchild}` placeholder from their
  # labels, and that wording difference alone blocks the pivot.
  df <- bp911_set_labels(
    df,
    stats::setNames(stem_labels[sub("_(1|4|8|12)$", "", wave_cols)], wave_cols)
  )

  long <- tidyr::pivot_longer(
    df,
    cols = dplyr::all_of(wave_cols),
    names_to = c(".value", "wave"),
    names_pattern = "^(.*)_(1|4|8|12)$"
  )

  # `pivot_longer()` carries the label through only on the labelled stems.
  long <- bp911_set_labels(long, stem_labels)
  attr(long$wave, "label") <- "Assessment (1 = baseline, then month)"

  # The file has no participation flag, so a child attended where any stem is measured.
  long <- long[rowSums(!is.na(long[names(stem_labels)])) > 0L, , drop = FALSE]

  # The pivot repeats the baseline columns on every row; keep them at baseline.
  months <- c(`1` = 0, `4` = 4, `8` = 8, `12` = 12)[long$wave]
  long[long$wave != "1", bp911_background] <- NA

  for (col in names(bp911_carried)) {
    long[[col]] <- carry_within(long[[col]], long$FID, months)
    attr(long[[col]], "label") <- paste0(
      attr(long[[col]], "label"),
      " [carried from baseline: ",
      bp911_carried[[col]],
      "]"
    )
  }

  # Age was asked at baseline only; follow-ups add the months since baseline.
  long$agechild_1 <- carry_within(long$agechild_1, long$FID, months) +
    unname(months) / 12
  attr(long$agechild_1, "label") <- paste0(
    "How old is child in years",
    " [follow-ups: baseline age + months since baseline / 12]"
  )

  long <- dplyr::relocate(long, dplyr::all_of(c("FID", "wave")))

  if (anyDuplicated(long[c("FID", "wave")]) > 0) {
    stop(
      "BPIPD-911: `FID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  long
}
