#' Tidier for BPIPD-1127 (TEPS)
#'
#' Each wave's student, parent and teacher-rating files are joined on sample
#' and `stud_id`, and the four waves (2001-2007) stacked long.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per student per wave they took part in
tidy_BPIPD_1127 <- function(raw_dataset, spec) {
  # Binding labelled columns needs the vctrs methods haven registers on load.
  loadNamespace("haven")
  d <- raw_dataset$data

  # The senior file's `w1w2chg` labels are mis-encoded Big5 of the junior file's.
  w2_senior <- d$wave2_senior_student
  attr(w2_senior$w1w2chg, "labels") <- attr(
    d$wave2_junior_student$w1w2chg,
    "labels"
  )

  # W3's teacher-rating folder holds copies of the student files, and W4's
  # core-panel rating file repeats rows of the combined one.
  waves <- list(
    bp1127_wave(
      bp1127_stack(d$wave1_junior_student, d$wave1_senior_student),
      bp1127_stack(d$wave1_junior_parent, d$wave1_senior_parent),
      bp1127_stack(
        d$wave1_junior_student_performance,
        d$wave1_senior_performance
      ),
      "w1"
    ),
    bp1127_wave(
      bp1127_stack(d$wave2_junior_student, w2_senior),
      bp1127_stack(d$wave2_junior_parent, d$wave2_senior_parent),
      bp1127_stack(
        d$wave2_junior_student_performance,
        d$wave2_senior_performance
      ),
      "w2"
    ),
    bp1127_wave(
      bp1127_panels(d$wave3_senior_student_cpnp, d$wave3_senior_student_cp),
      bp1127_panels(d$wave3_senior_parent_cpnp, d$wave3_senior_parent_cp) |>
        dplyr::rename(faedu_w34 = w3faedu, moedu_w34 = w3moedu),
      NULL,
      "w3"
    ),
    bp1127_wave(
      bp1127_panels(d$wave4_senior_student_cpnp, d$wave4_senior_student_cp),
      bp1127_panels(d$wave4_senior_parent_cpnp, d$wave4_senior_parent_cp) |>
        dplyr::rename(faedu_w34 = w4faedu, moedu_w34 = w4moedu),
      bp1127_panels(d$wave4_senior_performance_cpnp),
      "w4"
    )
  )
  for (i in seq_along(waves)[-1]) {
    waves[[i]] <- bp1127_match_labels(waves[[i]], waves[[1]])
  }

  # `stud_id` repeats between the 2001 junior and senior samples.
  df <- bp1127_bind(waves) |>
    dplyr::mutate(participant_id = paste0(cohort, "-", stud_id), .before = 1) |>
    dplyr::relocate(cohort, stud_id, .wave, .wave_label, .after = 1)

  if (anyDuplicated(df[c("participant_id", ".wave")]) > 0) {
    stop(
      "BPIPD-1127: `participant_id` and `.wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df <- dplyr::left_join(
    df,
    bp1127_sampling_design(d),
    by = c("cohort", "stud_id"),
    relationship = "many-to-one"
  )

  # W3/W4 parent education is on W4's six-level scale (`w4p104`) but carries
  # W1's labels; the core panel fathers' W1 x W3 codes confirm the shift.
  w34_scale <- c(
    attr(d$wave4_senior_parent_cpnp$w4p104, "labels"),
    `illegal value` = 97,
    missing = 99
  )
  attr(df$faedu_w34, "labels") <- w34_scale
  attr(df$moedu_w34, "labels") <- w34_scale

  # The new panel's birth year carries the 2001 senior form's labels (ROC
  # 71-75), which would make 2005 grade-11 students 20-21; its codes match
  # the core panel's ROC 77-78 four codes down, so 1-5 are ROC 75-79.
  attr(df$w3s479, "labels") <- c(
    stats::setNames(1:5, 75:79),
    other = 6,
    `illegal value` = 97,
    missing = 99
  )

  wave_order <- as.integer(df$.wave)
  for (col in c("w1faedu", "w1moedu", "w1s501")) {
    df[[col]] <- bp1127_carry(df[[col]], df$participant_id, wave_order)
  }
  # A later-scale column fills only rows still without a valid W1 value, so
  # each row keeps the earliest value recorded.
  later <- c(faedu_w34 = "w1faedu", moedu_w34 = "w1moedu", w3s479 = "w1s501")
  for (col in names(later)) {
    df[[col]] <- bp1127_carry(
      df[[col]],
      df$participant_id,
      wave_order,
      rows = !bp1127_valid(df[[later[[col]]]])
    )
  }

  df <- bp1127_cross_wave(df)
  df$birth_year <- dplyr::coalesce(
    bp1127_roc_year(df$w1s501),
    bp1127_roc_year(df$w3s479)
  )
  # Each parent's education on one scale, so the higher-educated parent can
  # be taken.
  df$father_education <- bp1127_education(df$faedu_w34, df$w1faedu)
  df$mother_education <- bp1127_education(df$moedu_w34, df$w1moedu)
  df$parents_highest_education <- bp1127_highest(
    df$father_education,
    df$mother_education
  )

  bp1127_label(df)
}

#' Constructs asked in several waves under wave-prefixed names
#'
#' Each entry gives the construct one column; `from` lists its source column
#' in each wave that asked it, and `labels` (if given) replaces the first
#' source's value labels.
bp1127_cross_wave_columns <- function() {
  ethnic_group <- c(
    `Fukienese of Taiwan` = 1,
    `Hakka of Taiwan` = 2,
    Mainlander = 3,
    Aborigine = 4,
    Other = 5,
    `Illegal value` = 97,
    Missing = 99
  )
  list(
    father_ethnic_group = list(
      from = c("w1faethn", "w3faethn"),
      labels = ethnic_group,
      label = paste(
        "Ethnic group of the father figure's own father, parent report",
        "(W1 w1faethn; W3 w3faethn, new panel)"
      )
    ),
    mother_ethnic_group = list(
      from = c("w1moethn", "w3moethn"),
      labels = ethnic_group,
      label = paste(
        "Ethnic group of the mother figure's own father, parent report",
        "(W1 w1moethn; W3 w3moethn, new panel)"
      )
    ),
    household_income = list(
      from = c("w1p515", "w2p508", "w3p602"),
      label = paste(
        "Average monthly household income, 6 bands, parent report",
        "(W1 w1p515, W2 w2p508, W3 w3p602; W3 labels only its lowest band)"
      )
    ),
    lives_with_father = list(
      from = c("w1s2021", "w3s3021"),
      label = "Father currently lives with the student (W1 w1s2021, W3 w3s3021)"
    ),
    lives_with_mother = list(
      from = c("w1s2022", "w3s3022"),
      label = "Mother currently lives with the student (W1 w1s2022, W3 w3s3022)"
    ),
    lives_with_step_parent = list(
      from = c("w1s2023", "w3s3023"),
      label = paste(
        "A step or adoptive parent currently lives with the student",
        "(W1 w1s2023; W3 w3s3023, which adds foster parents)"
      )
    ),
    feel_withdrawn = list(
      from = c("w1s516", "w2s426", "w3s429", "w4s443"),
      label = paste(
        "Symptom checklist: don't want to associate with others",
        "(w1s516, w2s426, w3s429, w4s443)"
      )
    ),
    feel_low = list(
      from = c("w1s517", "w2s427", "w3s430", "w4s444"),
      label = paste(
        "Symptom checklist: feel low (W4 adds low mood)",
        "(w1s517, w2s427, w3s430, w4s444)"
      )
    ),
    urge_scream_hit = list(
      from = c("w1s519", "w2s428", "w3s431", "w4s445"),
      label = paste(
        "Symptom checklist: feel like screaming, smashing things,",
        "quarrelling or hitting someone (w1s519, w2s428, w3s431, w4s445)"
      )
    ),
    feel_lonely = list(
      from = c("w1s522", "w2s430", "w3s433", "w4s447"),
      label = "Symptom checklist: lonely (w1s522, w2s430, w3s433, w4s447)"
    ),
    sleep_problems = list(
      from = c("w1s526", "w2s431", "w3s435", "w4s449"),
      label = paste(
        "Symptom checklist: can't sleep, sleep badly, wake easily or have",
        "nightmares (w1s526, w2s431, w3s435, w4s449)"
      )
    ),
    tight_head_numbness = list(
      from = c("w1s527", "w2s434", "w3s436", "w4s451"),
      label = paste(
        "Symptom checklist: tight head, numbness, tingling, weakness or",
        "trembling (w1s527, w2s434, w3s436, w4s451)"
      )
    )
  )
}

#' Add one column per cross-wave construct, stopping if two waves' source
#' columns are both filled on a row
bp1127_cross_wave <- function(df) {
  map <- bp1127_cross_wave_columns()
  absent <- setdiff(unlist(lapply(map, `[[`, "from")), names(df))
  if (length(absent) > 0) {
    stop(
      "BPIPD-1127: cross-wave source columns missing: ",
      paste(absent, collapse = ", "),
      call. = FALSE
    )
  }
  for (col in names(map)) {
    from <- map[[col]]$from
    values <- lapply(df[from], function(x) as.vector(unclass(x)))
    if (any(Reduce(`+`, lapply(values, Negate(is.na))) > 1)) {
      stop(
        "BPIPD-1127: more than one of ",
        paste(from, collapse = ", "),
        " is filled on a row.",
        call. = FALSE
      )
    }
    labels <- map[[col]]$labels
    if (is.null(labels)) {
      labels <- attr(df[[from[1]]], "labels")
    }
    df[[col]] <- haven::labelled(
      do.call(dplyr::coalesce, unname(values)),
      labels = labels,
      label = map[[col]]$label
    )
  }
  df
}

#' Gregorian year from a birth-year column whose value labels are ROC years
bp1127_roc_year <- function(x) {
  labels <- attr(x, "labels")
  roc <- suppressWarnings(as.numeric(names(labels)))
  roc[match(as.vector(unclass(x)), labels)] + 1911
}

#' Education levels shared by the parent-education columns (W1's scale)
bp1127_education_labels <- function() {
  c(
    `Junior high school or less` = 1,
    `Senior (vocational) high school` = 2,
    `Junior college or institute of technology` = 3,
    University = 4,
    `Graduate school` = 5,
    Other = 6
  )
}

#' One parent's education on W1's scale: the W3/W4 value where valid, else
#' W1's; W3/W4's separate senior and vocational high school codes merge
bp1127_education <- function(w34, w1) {
  from_w34 <- c(1, 2, 2, 3, 4, 5)[match(as.vector(unclass(w34)), 1:6)]
  w1 <- as.vector(unclass(w1))
  from_w1 <- ifelse(w1 %in% 1:6, w1, NA_real_)
  haven::labelled(
    dplyr::coalesce(from_w34, from_w1),
    labels = bp1127_education_labels()
  )
}

#' The higher of two parents' education levels, ignoring 'other'
bp1127_highest <- function(father, mother) {
  levels <- lapply(list(father, mother), function(x) {
    x <- as.vector(unclass(x))
    ifelse(x %in% 1:5, x, NA_real_)
  })
  haven::labelled(
    pmax(levels[[1]], levels[[2]], na.rm = TRUE),
    labels = bp1127_education_labels()[1:5]
  )
}

#' Tag the 2001 junior (J) and senior (S) samples and stack them
bp1127_stack <- function(junior, senior) {
  junior$cohort <- "J"
  senior$cohort <- "S"
  bp1127_bind(list(junior, bp1127_match_labels(senior, junior)))
}

#' Bind rows, restoring the variable labels that differing copies lose
bp1127_bind <- function(tables) {
  out <- dplyr::bind_rows(tables)
  for (col in names(out)) {
    if (is.null(attr(out[[col]], "label"))) {
      found <- unlist(lapply(tables, function(tbl) attr(tbl[[col]], "label")))
      attr(out[[col]], "label") <- found[1]
    }
  }
  out
}

#' Tag the core panel (J) and 2005 new panel (NP) and add the CP-only columns
bp1127_panels <- function(cpnp, cp = NULL) {
  cpnp$cohort <- ifelse(cpnp$cp == 1, "J", "NP")
  if (is.null(cp)) {
    return(cpnp)
  }
  # The CP files' W1 weights are re-standardised, not W1's `w1stwt1`.
  cp <- dplyr::rename(
    cp,
    dplyr::any_of(c(w1stwt1_cp = "w1stwt1", w1stwt3_cp = "w1stwt3"))
  )
  dplyr::left_join(
    cpnp,
    cp[c("stud_id", setdiff(names(cp), names(cpnp)))],
    by = "stud_id",
    relationship = "one-to-one"
  )
}

#' Join one wave's files and keep the students the wave observed
bp1127_wave <- function(student, parent, rating, prefix) {
  reporters <- bp1127_responded(parent, paste0("^", prefix, "p[0-9]"))
  df <- bp1127_join(student, parent, "_parent")
  # W3 has no teacher-rating data.
  if (!is.null(rating)) {
    reporters <- c(
      reporters,
      bp1127_responded(rating, paste0("^", prefix, "t"))
    )
    df <- bp1127_join(df, rating, "_rating")
  }
  # A student took part if they answered the questionnaire, sat the ability
  # test, or have a parent or teacher report with a valid answer.
  took_part <- unclass(df[[paste0(prefix, "refuse")]]) %in%
    0 |
    !is.na(df[[paste0(prefix, "all3p")]]) |
    paste(df$cohort, df$stud_id) %in% reporters
  df[took_part, , drop = FALSE]
}

#' Left-join a parent or rating file, suffixing names the student file uses
bp1127_join <- function(student, other, suffix) {
  other <- other[setdiff(names(other), c(".wave", ".wave_label", "cp"))]
  clash <- setdiff(
    intersect(names(other), names(student)),
    c("cohort", "stud_id")
  )
  other <- dplyr::rename_with(other, ~ paste0(.x, suffix), dplyr::all_of(clash))
  dplyr::left_join(
    student,
    other,
    by = c("cohort", "stud_id"),
    relationship = "one-to-one"
  )
}

#' Sample and `stud_id` of the rows with a valid answer in `pattern` columns
bp1127_responded <- function(tbl, pattern) {
  answered <- vapply(
    tbl[grep(pattern, names(tbl))],
    bp1127_valid,
    logical(nrow(tbl))
  )
  paste(tbl$cohort, tbl$stud_id)[rowSums(answered) > 0]
}

#' Present and not TEPS's illegal-value (97) or missing (99) code
bp1127_valid <- function(x) {
  !is.na(x) & !unclass(x) %in% c(97, 99)
}

#' Give `to` the value labels of `from` where they agree up to case and spacing
bp1127_match_labels <- function(to, from) {
  for (col in intersect(names(to), names(from))) {
    target <- attr(from[[col]], "labels", exact = TRUE)
    current <- attr(to[[col]], "labels", exact = TRUE)
    same <- !is.null(target) &&
      !is.null(current) &&
      all(current %in% target) &&
      identical(
        tolower(trimws(names(current))),
        tolower(trimws(names(target)[match(current, target)]))
      )
    if (same) {
      attr(to[[col]], "labels") <- target
    }
  }
  to
}

#' Design values at each sample's first wave: 2001 for J and S, 2005 for NP
bp1127_sampling_design <- function(d) {
  np <- d$wave3_senior_student_cpnp
  sources <- list(
    list(d$wave1_junior_student, "J", "w1"),
    list(d$wave1_senior_student, "S", "w1"),
    list(np[np$cp == 0, ], "NP", "w3")
  )
  items <- c("stwt1", "urban3", "priv", "pgrm")
  design <- dplyr::bind_rows(lapply(sources, function(s) {
    values <- lapply(s[[1]][paste0(s[[3]], items)], haven::zap_labels)
    names(values) <- paste0("sampling_", items)
    tibble::tibble(cohort = s[[2]], stud_id = s[[1]]$stud_id, !!!values)
  }))

  # W3 labels the same codes, with `urban3` 1 as "country" for "village".
  for (item in items[-1]) {
    col <- paste0("sampling_", item)
    design[[col]] <- haven::labelled(
      as.vector(design[[col]]),
      labels = attr(d$wave1_junior_student[[paste0("w1", item)]], "labels")
    )
  }
  design
}

#' Fill missing cells in `rows` from the participant's earliest valid value
bp1127_carry <- function(x, id, order, rows = TRUE) {
  valid <- x
  valid[!bp1127_valid(x)] <- NA
  carried <- carry_within(valid, id, order)
  fill <- is.na(x) & rows
  x[fill] <- carried[fill]
  x
}

#' Label the constructed columns and append each fill rule
bp1127_label <- function(df) {
  labels <- c(
    participant_id = "Sample-prefixed student id (<cohort>-<stud_id>)",
    cohort = paste(
      "TEPS sample: J = 2001 grade-7 junior-high sample (its core panel,",
      "cp = 1, followed in 2005 and 2007); S = 2001 grade-11 senior-high,",
      "vocational and junior-college sample; NP = 2005 grade-11 new panel"
    ),
    sampling_stwt1 = paste(
      "Standardised student weight at the sample's first wave",
      "(w1stwt1 for J and S, w3stwt1 for NP); carried to every wave"
    ),
    sampling_urban3 = paste(
      "Urbanisation stratum of the school at the sample's first wave",
      "(w1urban3, w3urban3); carried to every wave"
    ),
    sampling_priv = paste(
      "Public or private school at the sample's first wave",
      "(w1priv, w3priv); carried to every wave"
    ),
    sampling_pgrm = paste(
      "School programme at the sample's first wave",
      "(w1pgrm, w3pgrm); carried to every wave"
    ),
    faedu_w34 = paste(
      "Father's (incl. step/adoptive) education, six-level W3/W4 scale",
      "(w3faedu, w4faedu)"
    ),
    moedu_w34 = paste(
      "Mother's (incl. step/adoptive) education, six-level W3/W4 scale",
      "(w3moedu, w4moedu)"
    ),
    w1stwt1_cp = paste(
      "W1 standardised weight for the core panel, estimating W1 students who",
      "entered senior high, vocational school or junior college (w1stwt1 in",
      "the W3/W4 core-panel files)"
    ),
    w1stwt3_cp = paste(
      "W1 standardised weight for the core panel, estimating the whole W1",
      "population (w1stwt3 in the W3/W4 core-panel files)"
    ),
    birth_year = paste(
      "Birth year (Gregorian) from the ROC-year labels of w1s501 (J, S,",
      "core panel) or w3s479 (new panel); carried to every wave"
    ),
    father_education = paste(
      "Father figure's education on W1's scale: faedu_w34 where valid",
      "(W3/W4 senior and vocational high school merged), else w1faedu"
    ),
    mother_education = paste(
      "Mother figure's education on W1's scale: moedu_w34 where valid",
      "(W3/W4 senior and vocational high school merged), else w1moedu"
    ),
    parents_highest_education = paste(
      "Higher of father_education and mother_education ('other' ignored)"
    )
  )
  for (col in names(labels)) {
    attr(df[[col]], "label") <- labels[[col]]
  }

  rules <- c(
    w1faedu = "; carried from W1 to later waves",
    w1moedu = "; carried from W1 to later waves",
    w1s501 = "; carried from W1 to later waves",
    faedu_w34 = "; earliest W3/W4 value carried to rows with no valid W1 value",
    moedu_w34 = "; earliest W3/W4 value carried to rows with no valid W1 value",
    w3s479 = paste(
      "; value labels corrected to ROC 75-79 (released as 71-75);",
      "carried from W3 to W4 rows with no valid W1 birth year"
    )
  )
  for (col in names(rules)) {
    attr(df[[col]], "label") <- paste0(attr(df[[col]], "label"), rules[[col]])
  }
  df
}
