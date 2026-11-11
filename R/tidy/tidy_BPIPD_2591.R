#' Tidier for BPIPD-2591 (Kowert)
#'
#' One wide SPSS file holds the three annual CATI waves, wave in a column
#' suffix (`_T1`-`_T3`); this pivots it to long and keeps the attended waves.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant (`ID`) per wave they took part in
tidy_BPIPD_2591 <- function(raw_dataset, spec) {
  # T3's persangaggr3 is a different item (codebook, sheet All waves), so it
  # gets its own stem.
  panel <- dplyr::rename(
    raw_dataset$data$panel,
    persangaggr3new_T3 = persangaggr3_T3
  )
  attr(panel$persangaggr3new_T3, "label") <- paste(
    attr(panel$persangaggr3new_T3, "label", exact = TRUE),
    "(persangaggr3_T3 in the release)"
  )
  panel <- bp2591_unify_value_labels(panel)
  labels <- bp2591_stem_labels(panel)

  long <- panel |>
    tidyr::pivot_longer(
      cols = dplyr::matches("_T[123]$"),
      names_to = c(".value", "wave"),
      names_pattern = "^(.*)_(T[123])$"
    )
  # The pivot drops the variable label of columns without value labels; each
  # item takes its longest wave label, as the waves differ only in wording.
  for (stem in names(labels)) {
    attr(long[[stem]], "label") <- labels[[stem]]
  }
  attr(long$wave, "label") <- "Panel wave (T1-T3), from the column suffix"

  # No participation flag is released, so a wave was attended where any item
  # was answered.
  measured <- setdiff(names(long), c("za_nr", "version", "doi", "ID", "wave"))
  attended <- Reduce(`|`, lapply(long[measured], bp2591_answered))
  long <- long[attended, , drop = FALSE]

  # The 12-month carry rule blanks T1 entrants' household items at T3: the
  # codebook gives them for T1-T2 only and the release repeats entry answers.
  entry <- stats::ave(as.integer(substr(long$wave, 2L, 2L)), long$ID, FUN = min)
  beyond_12_months <- long$wave == "T3" & entry == 1L
  household <- c(
    "incomehouse",
    "houseperson",
    "housechild",
    "famstat",
    "fampartner",
    "wo"
  )
  for (col in household) {
    long[[col]][beyond_12_months] <- NA
  }

  if (anyDuplicated(long[c("ID", "wave")]) > 0) {
    stop(
      "BPIPD-2591: `ID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  long
}

#' Give every wave of an item the union of its value labels, later wording first
bp2591_unify_value_labels <- function(panel) {
  waved <- grep("_T[123]$", names(panel), value = TRUE)
  for (cols in split(waved, sub("_T[123]$", "", waved))) {
    sets <- Filter(
      Negate(is.null),
      lapply(panel[rev(cols)], attr, "labels", exact = TRUE)
    )
    if (length(sets) < 2L) {
      next
    }
    labels <- sort(Reduce(function(a, b) c(a, b[!b %in% a]), sets))
    for (col in cols) {
      panel[[col]] <- haven::labelled(
        haven::zap_labels(panel[[col]]),
        labels = labels,
        label = attr(panel[[col]], "label", exact = TRUE)
      )
    }
  }
  panel
}

#' The longest variable label each item carries in any wave
bp2591_stem_labels <- function(panel) {
  waved <- grep("_T[123]$", names(panel), value = TRUE)
  labels <- vapply(
    panel[waved],
    function(x) paste(attr(x, "label", exact = TRUE), collapse = ""),
    character(1)
  )
  labels <- vapply(
    split(labels, sub("_T[123]$", "", waved)),
    function(x) x[which.max(nchar(x))],
    character(1)
  )
  labels[nchar(labels) > 0L]
}

#' TRUE where a value was answered
bp2591_answered <- function(x) {
  if (is.character(x)) {
    return(!is.na(x) & x != "")
  }
  # 42 T2 columns leave -99 undeclared, and there it marks non-participants.
  !is.na(x) & unclass(x) != -99
}
