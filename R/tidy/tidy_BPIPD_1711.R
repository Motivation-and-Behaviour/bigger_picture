#' Tidier for BPIPD-1711 (HERMES)
#'
#' Two wide files, HERMES 1+2 (Stata) and HERMES 3 (RDS), each holding a
#' baseline and a one-year follow-up; each is pivoted long and the two stacked.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent per wave they took part in
tidy_BPIPD_1711 <- function(raw_dataset, spec) {
  hermes12 <- bp1711_hermes12(
    raw_dataset$data$hermes1_2_biggerpicture_shared_stata
  )
  hermes3 <- bp1711_hermes3(raw_dataset$data$hermes3_biggerpicture_rds)

  # The files share no participant or source column, so they stack.
  long <- dplyr::bind_rows(hermes12, hermes3)

  # `bind_rows()` drops the variable label from the factor columns it pads with
  # the other cohort's rows.
  long <- bp1711_apply_labels(
    long,
    c(bp1711_labels_of(hermes12), bp1711_labels_of(hermes3))
  )
  attr(long$participant_id, "label") <- "Row position in the source file"

  if (anyDuplicated(long[c("participant_id", "wave")]) > 0L) {
    stop(
      "BPIPD-1711: `participant_id` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  long
}

#' Each follow-up column of the HERMES 1+2 file and its baseline twin
bp1711_hermes12_map <- function() {
  items <- letters[1:25]
  scales <- c(
    "emotion",
    "conduct",
    "hyper",
    "peer",
    "prosoc",
    "ebdtot",
    "internprob",
    "externprob"
  )

  c(
    fjdatmm = "jdatmm",
    fjdatjjjj = "jdatjjjj",
    fjq3 = "jq8b",
    fjq3a = "jq8a1",
    fjq3b = "jq8a2",
    fjq4 = "jq9",
    fjq4a = "jq9a1",
    fjq4b = "jq9a2",
    fjq12a = "jq17a",
    fjq12b = "jq17b",
    fjq13a = "jq18a",
    fjq13b = "jq18b",
    fjq14 = "jq19",
    fjq14a = "jq19a",
    fjq15a = "jq20a",
    fjq15b = "jq20b",
    fjq17a = "jq22a",
    fjq17b = "jq22b",
    fjq25 = "jq30",
    fjq25a = "jq30a",
    fjq25a3 = "jq30a3",
    fjq25b = "jq30b",
    fjq25b3 = "jq30b3",
    fjq25c = "jq30c",
    fjq25c3 = "jq30c3",
    fjq27a = "jq32a",
    fjq27b = "jq32b",
    fjq29 = "jq34",
    fjq29a = "jq34a",
    fjq29b = "jq34b",
    stats::setNames(paste0("jq40", items), paste0("fjq35", items)),
    stats::setNames(paste0("js", scales), paste0("fjs", scales))
  )
}

#' Reshape the HERMES 1+2 file to one row per adolescent per wave
bp1711_hermes12 <- function(df) {
  map <- bp1711_hermes12_map()

  # Neither shared file carries a participant identifier, so row position is
  # the only link between an adolescent's baseline and follow-up answers.
  df <- dplyr::mutate(
    tibble::as_tibble(df),
    participant_id = sprintf("H12-%04d", seq_len(nrow(df))),
    # HERMES 1 and HERMES 2 are separate school samples (baselines June 2012
    # and April 2014); after the pivot `jdatjjjj` no longer says which.
    cohort = ifelse(jdatjjjj <= 2013, "HERMES1", "HERMES2"),
    .before = 1
  )

  # The two questionnaires number the same items differently, so each construct
  # takes its baseline column name; birth date and school level stay unsuffixed.
  names(df)[match(names(map), names(df))] <- paste0(unname(map), "_fu")
  df <- df |>
    dplyr::rename_with(
      ~ paste0(.x, "_bl"),
      dplyr::all_of(c(unname(map), "jq3"))
    ) |>
    dplyr::rename_with(
      ~ paste0(.x, "_fu"),
      dplyr::all_of(c("fjq16a", "fjq16b", "fjq17"))
    )

  # `jq40g/k/n/u/y` are labelled as if they were not reverse-scored, but their
  # codes already are: `jsconduct`, `jshyper` and `jspeer` are the plain sums.
  reverse <- paste0("jq40", c("g", "k", "n", "u", "y"), "_bl")
  df[reverse] <- Map(
    bp1711_relabel_values,
    df[reverse],
    lapply(df[sub("_bl$", "_fu", reverse)], attr, "labels", exact = TRUE)
  )

  bp1711_pivot_waves(bp1711_align_value_labels(df))
}

#' Reshape the HERMES 3 file to one row per adolescent per wave
bp1711_hermes3 <- function(df) {
  # Recorded once per participant, so repeated on both rows.
  fixed <- c(
    "participant_id",
    "cohort",
    "age_bl_log",
    "lvl_school_log",
    "drop_out_log",
    "highest_gua_edu_g",
    "sp_own_age_sbl"
  )

  df <- tibble::as_tibble(df) |>
    dplyr::mutate(
      participant_id = sprintf("H3-%04d", seq_len(nrow(df))),
      cohort = "HERMES3",
      .before = 1
    ) |>
    # `tl_ww_week_sfu` is the follow-up of `tl_hw_week_sbl`: its own label and
    # the codebook both call it tablet/laptop homework on school days.
    dplyr::rename(tl_hw_week_sfu = "tl_ww_week_sfu") |>
    dplyr::rename_with(
      ~ sub("_[sh](bl|fu)$", "_\\1", .x),
      -dplyr::all_of(fixed)
    )

  long <- bp1711_pivot_waves(df)

  # Parent education comes from the baseline guardian questionnaire only.
  attr(long$highest_gua_edu_g, "label") <- paste(
    attr(df$highest_gua_edu_g, "label", exact = TRUE),
    "(baseline guardian questionnaire, carried to follow-up)"
  )

  # The follow-up has no age item and is one year after baseline (protocol).
  long$age_at_wave <- long$age_bl_log + (long$wave == "follow_up")
  attr(long$age_at_wave, "label") <- paste(
    "age in years at the wave (baseline: age_bl_log;",
    "follow-up: age_bl_log + 1, the documented one-year interval)"
  )
  dplyr::relocate(long, "age_at_wave", .after = "age_bl_log")
}

#' Pivot the `_bl`/`_fu` column pairs to one row per participant per wave
bp1711_pivot_waves <- function(df) {
  wave_cols <- grep("_(bl|fu)$", names(df), value = TRUE)
  stems <- unique(sub("_(bl|fu)$", "", wave_cols))

  # `pivot_longer()` carries attributes only on the `haven_labelled` columns,
  # so every other item would arrive unlabelled.
  labels <- bp1711_column_labels(df, stems)

  long <- df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(wave_cols),
      names_to = c(".value", "wave"),
      names_pattern = "^(.*)_(bl|fu)$"
    ) |>
    dplyr::mutate(
      wave = unname(c(bl = "baseline", fu = "follow_up")[wave]),
      .after = "participant_id"
    ) |>
    bp1711_apply_labels(labels)

  # Every adolescent sits on both waves' roster, so a wave with no measured
  # value at all is a wave they did not take part in.
  long[rowSums(!is.na(long[stems])) > 0L, , drop = FALSE]
}

#' The variable label on every labelled column of a table
bp1711_labels_of <- function(df) {
  labels <- vapply(
    df,
    function(x) {
      label <- attr(x, "label", exact = TRUE)
      if (is.null(label)) NA_character_ else as.character(label)
    },
    character(1)
  )

  labels[!is.na(labels)]
}

#' Set the variable label on each named column
bp1711_apply_labels <- function(df, labels) {
  df[names(labels)] <- Map(
    function(x, label) {
      attr(x, "label") <- label
      x
    },
    df[names(labels)],
    labels
  )

  df
}

#' Give each stem's follow-up copy the value labels of its baseline copy
bp1711_align_value_labels <- function(df) {
  fu <- grep("_fu$", names(df), value = TRUE)
  bl <- sub("_fu$", "_bl", fu)
  paired <- bl %in% names(df)

  df[fu[paired]] <- Map(
    bp1711_relabel_values,
    df[fu[paired]],
    lapply(df[bl[paired]], attr, "labels", exact = TRUE)
  )

  df
}

#' Replace a column's value labels, keeping its variable label
bp1711_relabel_values <- function(x, labels) {
  if (is.null(labels)) {
    return(x)
  }

  haven::labelled(
    haven::zap_labels(x),
    labels = labels,
    label = attr(x, "label", exact = TRUE)
  )
}

#' The variable label to restore on each column after the pivot
bp1711_column_labels <- function(df, stems) {
  label_of <- function(column) {
    label <- attr(df[[column]], "label", exact = TRUE)
    if (is.null(label)) NA_character_ else as.character(label)
  }

  fixed <- setdiff(names(df), grep("_(bl|fu)$", names(df), value = TRUE))

  stem_labels <- vapply(
    stems,
    function(stem) {
      # The baseline SDQ subscale scores are labelled with their own column
      # name, so the follow-up twin's label is taken for those stems.
      found <- vapply(paste0(stem, c("_bl", "_fu")), label_of, character(1))
      found <- found[!is.na(found) & found != stem]
      if (length(found) == 0L) NA_character_ else found[[1]]
    },
    character(1)
  )

  # A stem holds both waves, so its label must not name one of them.
  stem_labels <- stem_labels |>
    sub(pattern = " \\((bl|fu)\\)$", replacement = "") |>
    sub(pattern = " at (baseline|follow-up)", replacement = "") |>
    sub(
      pattern = "^date of (baseline|fup), ",
      replacement = "date of questionnaire, "
    )

  labels <- c(vapply(fixed, label_of, character(1)), stem_labels)
  labels[!is.na(labels)]
}
