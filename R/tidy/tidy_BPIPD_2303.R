#' Tidier for BPIPD-2303 (MiMic project)
#'
#' One wide SPSS file holds the three waves, each item's wave marker in varying
#' positions; the tidier moves every marker to a trailing `_w<n>` and pivots long.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent passing the study's attention check per wave they took part in
tidy_BPIPD_2303 <- function(raw_dataset, spec) {
  # `mplus.sav`/`mplus.dat` are an id-less 19-column extract of this file.
  wide <- tibble::as_tibble(raw_dataset$data$`PDBWA SPSS Dataset`)
  names(wide) <- bp2303_wave_suffix(names(wide))

  wave_cols <- grep("_w[123]$", names(wide), value = TRUE)
  wide <- bp2303_align_for_pivot(wide, wave_cols)
  labels <- lapply(wide[wave_cols], attr, "label")

  long <- tidyr::pivot_longer(
    wide,
    cols = dplyr::all_of(wave_cols),
    names_to = c(".value", "wave"),
    names_pattern = "^(.*)_w([123])$"
  )

  # `pivot_longer()` drops the label of plain numeric columns, so it is put back.
  for (col in wave_cols) {
    attr(long[[sub("_w[123]$", "", col)]], "label") <- labels[[col]]
  }

  # `Wave1`-`Wave3` ("Data on wave n") are the study's participation flags.
  flags <- vapply(
    long[c("Wave1", "Wave2", "Wave3")],
    haven::zap_labels,
    numeric(nrow(long))
  )
  long <- long[flags[cbind(seq_len(nrow(long)), as.integer(long$wave))] == 1, ]

  # The study's attention-check exclusion, which reproduces its analytic sample (Rosič et al. 2025).
  long <- long[haven::zap_labels(long$AC_correct) == 1, ]

  wave_label <- "Study wave (1 = Sep-Nov 2021, 2 = Feb-Apr 2022, 3 = Jul-Sep 2022)"
  attr(long$wave, "label") <- wave_label
  long <- dplyr::relocate(long, "P_ID_2", "wave")
  wave_num <- as.integer(long$wave)

  # Gender comes from each participant's first wave (Rosič et al. 2025); the mapping carries it.
  later <- wave_num > stats::ave(wave_num, long$P_ID_2, FUN = min)
  long$GenderFINAL[later] <- NA
  long$GenderFINAL2[later] <- NA

  for (col in bp2303_parent_education) {
    long[[col]] <- carry_within(long[[col]], long$P_ID_2, wave_num)
    attr(long[[col]], "label") <- paste(
      attr(long[[col]], "label"),
      "(wave 1 value carried to later waves)"
    )
  }

  # The waves are documented as 4 months apart (Rosič et al. 2025), so every gap is within 12 months.
  long$Cult_Capital <- bp2303_carry_nearest(
    long$Cult_Capital,
    long$P_ID_2,
    wave_num
  )
  attr(long$Cult_Capital, "label") <- paste(
    attr(long$Cult_Capital, "label"),
    "(missing waves filled from the nearest wave, documented 4-month wave interval)"
  )

  long$EduTrack_wave <- bp2303_track_by_wave(long, wave_num)

  if (anyDuplicated(long[c("P_ID_2", "wave")]) > 0) {
    stop(
      "BPIPD-2303: `P_ID_2` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  long
}

#' Parent education columns, asked at wave 1 only
bp2303_parent_education <- c(
  "EduDad",
  "EduDad_text",
  "EduDAD_recoded",
  "EduMum",
  "EduMUM_text",
  "EduMUM_recoded"
)

#' Move each column's wave marker to a trailing `_w1`/`_w2`/`_w3`
bp2303_wave_suffix <- function(x) {
  renames <- c(
    # survey paradata name the second and third waves `_A` and `_B`
    Progress = "Progress_w1",
    Progress_A = "Progress_w2",
    Progress_B = "Progress_w3",
    Durationinseconds = "Durationinseconds_w1",
    Durationinseconds_A = "Durationinseconds_w2",
    Durationinseconds_B = "Durationinseconds_w3",
    # the wave-2 copy of the self-reported minutes item is named `hm`, not `m`
    Time_hm_self_w2 = "Time_m_self_w2"
  )
  # Unmarked single-wave items, answered only by that wave's participants.
  wave1 <- c(
    "BirthYear",
    "School",
    "School_text",
    "EduYear",
    "EduYearText",
    "Grades",
    "Ethnic",
    "Ethnic_Text",
    "Ethnic_recoded",
    bp2303_parent_education,
    "FAC1_1",
    "FAC2_1",
    "FAC3_1"
  )
  wave2 <- grep("^YM[0-9]{3}S[0-9]{2}$", x, value = TRUE)
  renames <- c(
    renames,
    stats::setNames(paste0(wave1, "_w1"), wave1),
    stats::setNames(paste0(wave2, "_w2"), wave2)
  )
  x <- ifelse(x %in% names(renames), renames[x], x)
  x <- sub("^(.*?)_?[wW]([123])_(.*)$", "\\1_\\3_w\\2", x)
  sub("^(.+?)_?[wW]([123])$", "\\1_w\\2", x)
}

#' Give every wave's copy of an item the earliest wave's label, less its wave marker
bp2303_align_for_pivot <- function(df, wave_cols) {
  for (cols in split(wave_cols, sub("_w[123]$", "", wave_cols))) {
    label <- attr(df[[cols[order(cols)][1]]], "label")
    if (!is.null(label)) {
      label <- trimws(gsub("[ _.]*[wW](ave)? ?[123](_|$)", "\\2", label))
    }
    # `vctrs` will not combine labelled columns whose label or SPSS formats differ.
    df[cols] <- lapply(df[cols], function(x) {
      attr(x, "label") <- label
      attr(x, "format.spss") <- NULL
      attr(x, "display_width") <- NULL
      x
    })
  }
  df
}

#' Secondary track at each wave, following the school stage set by the wave-1 class
bp2303_track_by_wave <- function(long, wave_num) {
  track <- as.character(haven::as_factor(long$EduTrackFINAL))
  # `EduYear` codes 1-4 are basic-school classes 6-9 and 5-8 secondary years 1-4 (Appendix A question 5).
  class_w1 <- carry_within(as.numeric(long$EduYear), long$P_ID_2, wave_num)
  # Wave 3 follows the 2021/22 school year, so classes move up and final-year pupils have left.
  left <- wave_num == 3 &
    class_w1 %in% c(7, 8) &
    (class_w1 == 8 | track %in% "Vocational")
  track[left] <- NA
  track[class_w1 %in% 1:4 & wave_num < 3] <- "Not tracked"
  track[class_w1 %in% 1:3 & wave_num == 3] <- "Not tracked"
  attr(track, "label") <- paste(
    "Secondary track at this wave (EduTrackFINAL; 'Not tracked' in basic school;",
    "missing at wave 3 for wave-1 final-year pupils)"
  )
  track
}

#' Fill missing values from the participant's nearest observed wave, earlier on a tie
bp2303_carry_nearest <- function(x, id, wave) {
  out <- x
  for (i in which(is.na(x))) {
    donors <- which(id == id[i] & !is.na(x))
    if (length(donors) > 0) {
      nearest <- order(abs(wave[donors] - wave[i]), wave[donors])[1]
      out[i] <- x[donors[nearest]]
    }
  }
  out
}
