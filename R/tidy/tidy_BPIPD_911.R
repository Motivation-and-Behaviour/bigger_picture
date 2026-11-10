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
#' `_4`, `_8`, `_12` the month of follow-up (Stage 3 Measures List); only
#' baseline asked the device items, so the follow-ups are dropped.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child at baseline (key `FID` + `wave`)
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

  # Drop-wave rule: waves 4, 8 and 12 hold only the study's device sum
  # `TECH_child`, not a device item (`TECH2_c`/`TECH3_c` are print media).
  device_items <- grep(
    "^TECH[23]_[abdefg]_(hrs|mins)$",
    names(long),
    value = TRUE
  )
  answered <- rowSums(!is.na(long[device_items])) > 0L
  long <- long[long$wave %in% long$wave[answered], , drop = FALSE]

  long <- dplyr::relocate(long, dplyr::all_of(c("FID", "wave")))

  if (anyDuplicated(long[c("FID", "wave")]) > 0) {
    stop(
      "BPIPD-911: `FID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  long
}
