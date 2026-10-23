#' Tidier for BPIPD-176 (Gentile, 2014)
#'
#' One wide `.sav` file holds all three annual waves, wave in a column prefix
#' (`W1_`, `w2_`, `W3_`); this pivots it to long and carries SES items across waves.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per wave they took part in
tidy_BPIPD_176 <- function(raw_dataset, spec) {
  wide <- tibble::as_tibble(raw_dataset$data$data)

  # The unprefixed Raven's items (`ia1`-`ie12`) and `favgm` repeat wave-prefixed
  # stem names but hold other responses, and the file does not say which wave.
  wave_cols <- grep("^[wW][0-9]+_", names(wide), value = TRUE)
  wide <- dplyr::rename_with(
    wide,
    ~ paste0("unassigned_", .x),
    dplyr::all_of(intersect(names(wide), sub("^[wW][0-9]+_", "", wave_cols)))
  )

  # The z-scored gaming-time columns carry the wave mid-name (`ZW1_VG1time`).
  wide <- dplyr::rename_with(
    wide,
    ~ sub("^Z([wW][0-9]+)_", "\\1_Z", .x),
    dplyr::matches("^Z[wW][0-9]+_")
  )

  # `w2_GM3` is wave 1's code (it upper-cases `W1_gm3.q`) and `w1_GM3` is empty.
  wide <- wide |>
    dplyr::select(-"w1_GM3") |>
    dplyr::rename(w1_GM3 = "w2_GM3")

  wide <- bp176_align_wave_types(wide)
  stem_labels <- bp176_stem_labels(wide)
  wide <- bp176_align_wave_attrs(wide)

  # A child who missed a wave keeps a roster row with every questionnaire item
  # missing, while the derived columns stay filled for everyone.
  answered <- vapply(
    1:3,
    function(w) bp176_answered(wide, w),
    logical(nrow(wide))
  )
  colnames(answered) <- paste0("W", 1:3, "_answered_any_item")

  long <- wide |>
    dplyr::bind_cols(tibble::as_tibble(answered)) |>
    tidyr::pivot_longer(
      cols = dplyr::matches("^[wW][0-9]+_"),
      names_to = c("wave", ".value"),
      names_pattern = "^[wW]([0-9]+)_(.*)$"
    ) |>
    dplyr::filter(answered_any_item) |>
    dplyr::select(-"answered_any_item") |>
    dplyr::relocate("sn", "wave")

  # `pivot_longer()` keeps attributes only on the `haven_labelled` copies, so the
  # plain numeric and free-text stems would arrive unlabelled.
  long[names(stem_labels)] <- Map(
    function(x, label) {
      attr(x, "label") <- label
      x
    },
    long[names(stem_labels)],
    stem_labels
  )

  if (anyDuplicated(long[c("sn", "wave")]) > 0) {
    stop(
      "BPIPD-176: `sn` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  # Parent education was asked at wave 1 only.
  wave <- as.integer(long$wave)
  for (column in c("maedu", "paedu")) {
    long[[column]] <- carry_within(long[[column]], long$sn, wave)
    attr(long[[column]], "label") <- paste0(
      attr(long[[column]], "label"),
      "; asked at wave 1, carried to later waves"
    )
  }

  # Waves were a year apart (Gentile et al. 2011), so only adjacent waves are
  # within the 12-month limit for housing type and family structure.
  for (column in c("house", "livwithr")) {
    label <- attr(long[[column]], "label")
    long[[column]] <- bp176_carry_adjacent(long[[column]], long$sn, wave)
    attr(long[[column]], "label") <- paste0(
      label,
      "; missing waves filled from the adjacent wave, earlier first"
    )
  }

  long
}

#' Fill NA from the same child's adjacent wave, preferring the earlier one
bp176_carry_adjacent <- function(x, id, wave) {
  key <- paste(id, wave)
  earlier <- x[match(paste(id, wave - 1L), key)]
  later <- x[match(paste(id, wave + 1L), key)]
  missing <- is.na(x)
  x[missing] <- dplyr::coalesce(earlier, later)[missing]
  x
}

#' The wave-prefixed columns, in wave order so wave 1 leads each stem's group
bp176_wave_cols <- function(df) {
  unlist(lapply(
    1:3,
    function(w) grep(paste0("^[wW]", w, "_"), names(df), value = TRUE)
  ))
}

#' Cast the stems the file typed differently from one wave to the next
bp176_align_wave_types <- function(df) {
  # Free-text answers read as numbers at one wave, text at another, and the
  # sleep clock times as text at wave 2 and `hms` at wave 3.
  stems <- c(
    "height.q",
    "weight.q",
    "slpwk.q",
    "slpwkn.q",
    "wakewk.q",
    "wakewkn.q"
  )
  cols <- names(df)[sub("^[wW][0-9]+_", "", names(df)) %in% stems]
  df[cols] <- lapply(df[cols], function(x) {
    label <- attr(x, "label")
    x <- as.character(x)
    attr(x, "label") <- label
    x
  })
  df
}

#' Make each stem's wave copies combinable by the pivot
bp176_align_wave_attrs <- function(df) {
  cols <- bp176_wave_cols(df)
  for (group in split(cols, sub("^[wW][0-9]+_", "", cols))) {
    labelled <- group[vapply(df[group], haven::is.labelled, logical(1))]
    codes <- unique(lapply(df[labelled], function(x) {
      sort(unname(attr(x, "labels", exact = TRUE)))
    }))
    # Every wave uses the same codes and only the label wording differs, so wave
    # 1's wording stands for all three; a recoded item is left for the pivot.
    if (length(labelled) < 2L || length(codes) > 1L) {
      next
    }
    labels <- attr(df[[labelled[1]]], "labels", exact = TRUE)
    df[labelled] <- lapply(df[labelled], function(x) {
      attr(x, "labels") <- labels
      x
    })
  }

  # `vctrs` will not combine two columns whose SPSS display attributes differ,
  # and every wave stamps its own; the stem's label goes back on after the pivot.
  df[cols] <- lapply(df[cols], function(x) {
    attr(x, "label") <- NULL
    attr(x, "format.spss") <- NULL
    attr(x, "display_width") <- NULL
    x
  })
  df
}

#' Variable label per wave-prefixed stem, from the first wave that carries one
bp176_stem_labels <- function(df) {
  cols <- bp176_wave_cols(df)
  labels <- vapply(
    df[cols],
    function(x) {
      label <- attr(x, "label")
      if (is.null(label)) NA_character_ else as.character(label)[1]
    },
    character(1)
  )
  # Most labels open with the wave they came from ("W1 Total Amount of VG Play").
  labels <- sub("^[wW][0-9]+[_ ]*", "", trimws(labels))
  names(labels) <- sub("^[wW][0-9]+_", "", cols)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  labels[!duplicated(names(labels))]
}

#' Children who answered at least one questionnaire item in a wave
bp176_answered <- function(df, wave) {
  cols <- grep(paste0("^[wW]", wave, "_"), names(df), value = TRUE)
  given <- lapply(df[cols], function(x) {
    if (is.character(x)) !is.na(x) & trimws(x) != "" else !is.na(x)
  })
  # Unlabelled columns filled for the whole roster are derived, not answers.
  items <- vapply(df[cols], haven::is.labelled, logical(1)) |
    !vapply(given, all, logical(1))
  Reduce(`|`, given[items])
}
