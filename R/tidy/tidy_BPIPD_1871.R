#' Tidier for BPIPD-1871 (Adolescent Nutritional Assessment Longitudinal Study (ELANA))
#'
#' Two school cohorts, one file per cohort and wave: the elementary cohort's
#' waves 1, 3 and 4 (2010, 2012, 2013) and the high school cohort's wave 1 (2010).
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent per wave, keyed by `participant_id` and
#'   `wave`
tidy_BPIPD_1871 <- function(raw_dataset, spec) {
  w1 <- raw_dataset$data$elementary_w1
  w3 <- raw_dataset$data$elementary_w3
  w4 <- raw_dataset$data$elementary_w4
  hs <- raw_dataset$data$high_school_w1

  # One name per construct: screen, KIDSCREEN and CATS bullying items take the
  # elementary wave 1 names, GHQ-12 items the wave 3 names.
  kidscreen <- grep("^k[0-9]+", names(w1), value = TRUE)
  cats <- grep("^l[0-9]+[za]", names(w1), value = TRUE)
  ghq <- grep("^n[0-9]+", names(w3), value = TRUE)
  item <- "^[a-z]([0-9]+).*$"
  cats_item <- "^[a-z]([0-9]+[za]).*$"

  w1$wave <- as.integer(w1$wave)
  # W1 shortens option 1, which its questionnaire words 'nunca ou quase nunca'.
  for (column in c("d1televi", "d3comput")) {
    labels <- attr(w1[[column]], "labels", exact = TRUE)
    names(labels)[labels == 1] <- "nunca ou quase nunca"
    attr(w1[[column]], "labels") <- labels
  }

  # W3 labels hours code 9 as no answer, against the W1 questionnaire's
  # instruction to enter 9.0 for more than nine hours; the code is kept.
  w3 <- w3 |>
    dplyr::rename(wave = onda) |>
    dplyr::mutate(
      wave = as.integer(haven::zap_labels(wave)),
      dplyr::across(c(d2htelev, d4hcompu), bp1871_hours)
    ) |>
    bp1871_rename_items(
      grep("^o[0-9]+[za]", names(w3), value = TRUE),
      cats,
      cats_item
    )

  w4 <- w4 |>
    dplyr::rename(wave = onda) |>
    dplyr::mutate(
      wave = as.integer(wave),
      dplyr::across(c(d2htelev, d4hcompu), bp1871_hours)
    ) |>
    bp1871_rename_items(
      grep("^m[0-9]+", names(w4), value = TRUE),
      kidscreen,
      item
    ) |>
    bp1871_rename_items(grep("^r[0-9]+", names(w4), value = TRUE), ghq, item)

  hs <- hs |>
    dplyr::rename(
      wave = onda,
      a91tesco = a91tescol,
      d1televi = d1televis,
      d2htelev = d2htelevis,
      d3comput = d3computa,
      d4hcompu = d4hcomputa
    ) |>
    dplyr::mutate(wave = as.integer(haven::zap_labels(wave))) |>
    bp1871_rename_items(grep("^m[0-9]+", names(hs), value = TRUE), ghq, item) |>
    bp1871_rename_items(
      grep("^(r[0-9]+[za]|n2[01]z)", names(hs), value = TRUE),
      cats,
      cats_item
    )

  tables <- list(w1, w3, w4, hs)
  tables <- Map(
    function(tbl, cohort) dplyr::mutate(tbl, cohort = cohort, .before = 1),
    tables,
    c("elementary", "elementary", "elementary", "high_school")
  )
  # The files word the same codes differently, and W4 gives its GHQ-12 items
  # five-point labels though only codes 1-4 occur; the first file's labels win.
  tables <- bp1871_unify_labels(tables)
  df <- dplyr::bind_rows(tables)
  # bind_rows() drops the variable label of a column without value labels.
  for (column in names(df)) {
    attr(df[[column]], "label") <- Find(
      Negate(is.null),
      lapply(tables, function(tbl) attr(tbl[[column]], "label", exact = TRUE))
    )
  }

  # `id` restarts in each cohort, so the key needs the cohort as well.
  df <- dplyr::mutate(
    df,
    participant_id = paste0(cohort, "_", sprintf("%.0f", id)),
    .before = 1
  )
  df <- dplyr::relocate(df, cohort, wave, id, data, .after = participant_id)
  if (anyDuplicated(df[c("participant_id", "wave")]) > 0) {
    stop(
      "BPIPD-1871: `participant_id` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  # A row with every questionnaire item missing carries no observation; the
  # high school file's `dummy_ghq` (complete GHQ-12, yes/no) is derived, and
  # code 9 on a CATS item is no answer, as the elementary files label it.
  background <- c(
    "participant_id",
    "cohort",
    "wave",
    "id",
    "data",
    "a2nasc",
    "a3sexo",
    "a91tesco",
    "idade",
    "sw",
    "dummy_ghq"
  )
  measured <- setdiff(names(df), background)
  answered <- !is.na(df[measured])
  answered[, cats] <- answered[, cats] &
    as.matrix(haven::zap_labels(df[cats])) != 9
  df <- df[rowSums(answered) > 0L, , drop = FALSE]

  attr(df$participant_id, "label") <- "Cohort and `id`"
  attr(df$cohort, "label") <-
    "Cohort: elementary (6th year in 2010) or high_school (1st year of high school in 2010)"
  attr(df$wave, "label") <-
    "Study wave (elementary 1 = 2010, 3 = 2012, 4 = 2013; high school 1 = 2010)"
  df
}

#' Round an hours item to its one-decimal entry, dropping float storage error
bp1871_hours <- function(x) {
  structure(
    round(as.numeric(haven::zap_labels(x)), 1),
    label = attr(x, "label", exact = TRUE)
  )
}

#' Rename items to the reference names that share their item number
bp1871_rename_items <- function(tbl, items, reference, key) {
  to <- reference[match(sub(key, "\\1", items), sub(key, "\\1", reference))]
  dplyr::rename(tbl, dplyr::all_of(stats::setNames(items, to)))
}

#' Give a column the value labels of its first labelled copy in every table
bp1871_unify_labels <- function(tables) {
  columns <- unlist(lapply(tables, names))
  for (column in unique(columns[duplicated(columns)])) {
    labels <- Find(
      Negate(is.null),
      lapply(tables, function(tbl) attr(tbl[[column]], "labels", exact = TRUE))
    )
    if (is.null(labels)) {
      next
    }
    tables <- lapply(tables, function(tbl) {
      # Not every file holds every column.
      if (!column %in% names(tbl)) {
        return(tbl)
      }
      x <- tbl[[column]]
      tbl[[column]] <- haven::labelled(
        as.double(haven::zap_labels(x)),
        labels = labels,
        label = attr(x, "label", exact = TRUE)
      )
      tbl
    })
  }
  tables
}
