#' Source columns renamed so each construct has one stem ending in its wave
bp820_renames <- function() {
  c(
    # Time-invariant, and would collide with the per-wave `sex1`-`sex3`.
    sex_all_waves = "sex",
    # T3 numbers the physical items one higher than the Scale Handbook does.
    aq08p3 = "aq09p3",
    aq09p3 = "aq10p3",
    aq10p3 = "aq11p3",
    # T3 asked aq01v and aq03v as one item, which the handbook names aq31v.
    aq31v3 = "aq01v3",
    # T3 added a sixth media-frequency category, so its codes differ.
    tv_oft_5pt1 = "tv_oft1",
    tv_oft_5pt2 = "tv_oft2",
    tv_oft_6pt3 = "tv_oft3",
    pc_oft_5pt1 = "pc_oft1",
    pc_oft_5pt2 = "pc_oft2",
    pc_oft_6pt3 = "pc_oft3",
    timetv_5pt1 = "timetv1",
    timetv_5pt2 = "timetv2",
    timetv_6pt3 = "timetv3",
    timepc_5pt1 = "timepc1",
    timepc_5pt2 = "timepc2",
    timepc_6pt3 = "timepc3",
    timeme_5pt1 = "timeme1",
    timeme_5pt2 = "timeme2",
    timeme_6pt3 = "timeme3",
    # Scale scores hold the wave mid-name; move it to the end to pivot.
    aqp1 = "aq1p",
    aqr1 = "aq1r",
    aqv1 = "aq1v",
    aqp2 = "aq2p",
    aqr2 = "aq2r",
    aqv2 = "aq2v",
    aq3 = "aq3n",
    aqp3 = "aq3pn",
    aqr3 = "aq3r",
    aqv3 = "aq3vn"
  )
}

#' Copy each stem's T1 variable and value labels onto its later waves
bp820_unify_wave_labels <- function(df) {
  cols <- grep("[123]$", names(df), value = TRUE)
  # Stems left here differ across waves only in wording or translation.
  for (stem in split(cols, sub("[123]$", "", cols))) {
    stem <- stem[order(sub(".*([123])$", "\\1", stem))]
    first <- df[[stem[[1]]]]
    value_labels <- attr(first, "labels", exact = TRUE)
    var_label <- attr(first, "label", exact = TRUE)
    for (col in stem[-1]) {
      x <- as.vector(df[[col]])
      if (is.null(value_labels)) {
        attr(x, "label") <- var_label
        df[[col]] <- x
      } else {
        df[[col]] <- haven::labelled(
          x,
          labels = value_labels,
          label = var_label
        )
      }
    }
  }
  df
}

#' Labels for the score columns, which the pivot drops from plain numerics
bp820_score_labels <- function() {
  # The .sav labels `aqp2`/`aqr2`/`aqv2` as each other's subscale.
  c(
    aq = "Self-reported aggression: total score",
    aqp = "Self-reported aggression: physical aggression",
    aqr = "Self-reported aggression: relational aggression",
    aqv = "Self-reported aggression: verbal aggression",
    pv = "Prosocial behaviour self-report, mean of 15 items (T3 only)",
    timetv_5pt = "TV/video use, tv_oft_5pt * tv_dau (T1, T2)",
    timetv_6pt = "TV/video use, tv_oft_6pt * tv_dau (T3)",
    timepc_5pt = "Computer/video game use, pc_oft_5pt * pc_dau (T1, T2)",
    timepc_6pt = "Computer/video game use, pc_oft_6pt * pc_dau (T3)",
    timeme_5pt = "Total media use, timetv_5pt + timepc_5pt (T1, T2)",
    timeme_6pt = "Total media use, timetv_6pt + timepc_6pt (T3)"
  )
}

#' Tidier for BPIPD-820 (Krahé)
#'
#' One wide SPSS file holding three annual waves of a Berlin school cohort;
#' columns are renamed so each ends in its wave, then pivoted long.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant per wave (T1-T3)
tidy_BPIPD_820 <- function(raw_dataset, spec) {
  long <- raw_dataset$data[[1]] |>
    dplyr::rename(dplyr::all_of(bp820_renames())) |>
    bp820_unify_wave_labels() |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(c("vp", "sex_all_waves")),
      names_to = c(".value", "wave"),
      names_pattern = "^(.*)([123])$"
    ) |>
    dplyr::relocate("wave", .after = "vp")

  score_labels <- bp820_score_labels()
  for (col in names(score_labels)) {
    attr(long[[col]], "label") <- score_labels[[col]]
  }

  # No participation flag: a wave with no measured item was not attended.
  measured <- setdiff(names(long), c("vp", "wave", "sex_all_waves"))
  long <- long[rowSums(!is.na(long[measured])) > 0L, ]

  if (anyDuplicated(long[c("vp", "wave")]) > 0) {
    stop(
      "BPIPD-820: `vp` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  tibble::as_tibble(long)
}
