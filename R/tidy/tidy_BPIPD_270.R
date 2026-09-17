#' Tidier for BPIPD-270 (KCYPS)
#'
#' The elementary (e4) and middle/high (m1) cohorts each have a main-panel and
#' a guardian file per wave, plus a sibling file from wave 2. The two youth
#' files stack as extra participants and the guardian file joins onto them.
#' The released data files carry no labels, so the variable and value labels
#' are read from the shared codebook workbook.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per youth per wave (`ID` + `wave`)
tidy_BPIPD_270 <- function(raw_dataset, spec) {
  index <- bp270_spec_index(spec)

  # Every file has both a .csv and .rda. They are basically the same so the
  # .rda are the ones used. Every column except ID/HID/PID carries a
  # wave suffix (YGENDERw3, WEIGHTA1w1).
  read_respondent <- function(wave, cohort, respondent) {
    name <- paste("kcyps", wave, cohort, respondent, "data_rda", sep = "_")
    raw <- raw_dataset$data[[name]]
    if (is.null(raw)) {
      # Siblings were not surveyed at wave 1.
      if (!(respondent == "sibling" && wave == "w1")) {
        stop(
          paste0("BPIPD-270: resource `", name, "` is missing."),
          call. = FALSE
        )
      }
      return(NULL)
    }
    raw |>
      tibble::as_tibble() |>
      dplyr::rename_with(\(x) sub("w[0-9]+$", "", x))
  }

  # The released files keep the full panel roster, so a youth who did not take
  # part in a wave still has a row: `SURVEY1 == 2` with every questionnaire item
  # missing. Those rows carry no observation, so drop them. `SURVEY1` is absent
  # at wave 1, where every panel member responded.
  # (Codebook sheet CB_e/CB_m: `SURVEY1` "Survey participation (Youth)",
  # 1 = Yes, 2 = No.)
  drop_nonparticipants <- function(df) {
    if (is.null(df) || !"SURVEY1" %in% names(df)) {
      return(df)
    }
    df[!(!is.na(df$SURVEY1) & df$SURVEY1 == 2), , drop = FALSE]
  }

  read_wave_cohort <- function(this_wave, this_cohort) {
    # `main` and `sibling` are the same youth questionnaire put to two different
    # children in the same household: they share most of their columns and none
    # of their IDs, so they stack as extra participants rather than joining as
    # extra variables.
    youth <- dplyr::bind_rows(
      main = drop_nonparticipants(
        read_respondent(this_wave, this_cohort, "main")
      ),
      sibling = drop_nonparticipants(
        read_respondent(this_wave, this_cohort, "sibling")
      ),
      .id = "respondent"
    )

    # The guardian file holds at most one row per child across both youth
    # files, keyed on the child's ID, so it joins one-to-one. It is joined from
    # the left so that guardians of a non-participating youth do not reappear
    # as rows with no youth data. The reader stamps `.wave`/`.wave_label` on
    # both sides, so the guardian copies are dropped rather than carried
    # through as `.x`/`.y` pairs.
    guardian <- read_respondent(this_wave, this_cohort, "guardian") |>
      dplyr::select(-dplyr::any_of(c(".wave", ".wave_label")))

    # `cohort` is the file-derived e4/m1 label. The study's `COHORT` codes
    # cohort and respondent together (1/3 = elementary main/sibling, 2/4 =
    # middle main/sibling; codebook CB_e) and is absent in the guardian files.
    youth |>
      dplyr::left_join(
        guardian,
        by = c("ID", "HID", "PID"),
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(wave = this_wave, cohort = this_cohort, .before = 1)
  }

  combos <- expand.grid(
    wave = index$waves,
    cohort = index$cohorts,
    stringsAsFactors = FALSE
  )

  df <- dplyr::bind_rows(
    unname(Map(read_wave_cohort, combos$wave, combos$cohort))
  )

  # Participants keep their ID across waves, so ID + wave is the row key.
  if (anyDuplicated(df[c("ID", "wave")]) > 0) {
    stop(
      "BPIPD-270: `ID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  bp270_apply_codebook_labels(df, raw_dataset)
}

#' Wave and cohort tokens the spec declares
#'
#' The spec's wave values read `<n>-<year>` ("1-2018"), while every data
#' resource is named `kcyps_w<n>_<cohort>_<respondent>_data_<format>`, so both
#' the wave token and the cohort token are read off the spec.
bp270_spec_index <- function(spec) {
  pattern <- "^kcyps_(w[0-9]+)_([a-z0-9]+)_([a-z]+)_data_rda$"

  resources <- unlist(
    lapply(spec$waves, function(wave) {
      vapply(wave$resources, \(resource) as.character(resource$name), "")
    }),
    use.names = FALSE
  )
  resources <- grep(pattern, resources, value = TRUE)

  waves <- vapply(spec$waves, \(wave) as.character(wave$wave), "")
  waves <- paste0("w", sub("-.*$", "", waves))
  cohorts <- unique(sub(pattern, "\\2", resources))

  if (length(waves) == 0 || anyDuplicated(waves) > 0 || length(cohorts) == 0) {
    stop(
      "BPIPD-270: cannot derive wave and cohort tokens from the spec.",
      call. = FALSE
    )
  }

  combos <- expand.grid(
    wave = waves,
    cohort = cohorts,
    stringsAsFactors = FALSE
  )
  backed <- vapply(
    seq_len(nrow(combos)),
    function(i) {
      prefix <- paste0("kcyps_", combos$wave[[i]], "_", combos$cohort[[i]], "_")
      any(startsWith(resources, prefix))
    },
    logical(1)
  )
  if (!all(backed)) {
    stop(
      paste0(
        "BPIPD-270: the spec declares no data resources for wave/cohort ",
        paste0(
          combos$wave[!backed],
          "/",
          combos$cohort[!backed],
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  list(waves = waves, cohorts = cohorts)
}

#' The codebook sheet that describes each respondent's columns
#'
#' The shared workbook holds one codebook sheet per questionnaire: `CB_e` the
#' elementary youth form, `CB_m` the middle/high youth form and `CB_p` the
#' guardian form (the spec names the same three sheets on its codebook
#' resources). Elementary comes first because the two youth forms word some
#' shared columns differently and the elementary wording is the one kept.
bp270_codebook_sheets <- function() {
  c(elem = "CB_e", mid = "CB_m", guardian = "CB_p")
}

#' Attach the codebook's variable and value labels to the assembled table
bp270_apply_codebook_labels <- function(df, raw_dataset) {
  sheets <- bp270_codebook_sheets()
  book <- dplyr::bind_rows(lapply(names(sheets), function(part) {
    bp270_read_codebook(bp270_codebook_path(raw_dataset, part), sheets[[part]])
  }))

  # First sheet wins, so a column the elementary and middle/high forms word
  # differently keeps the elementary wording.
  book <- book[!duplicated(book$variable), , drop = FALSE]
  book <- book[book$variable %in% names(df), , drop = FALSE]

  for (i in seq_len(nrow(book))) {
    column <- book$variable[[i]]
    df[[column]] <- bp270_label_column(
      df[[column]],
      book$label[[i]],
      book$values[[i]]
    )
  }

  tibble::as_tibble(df)
}

#' The workbook holding one respondent's codebook sheet
#'
#' Every wave declares the same three codebook resources and they all point at
#' the one shared workbook, so any wave's copy serves.
bp270_codebook_path <- function(raw_dataset, part) {
  resources <- grep(
    paste0("_", part, "_codebook$"),
    names(raw_dataset$codebook),
    value = TRUE
  )
  paths <- unique(unlist(raw_dataset$codebook[resources], use.names = FALSE))
  if (length(paths) != 1) {
    stop(
      paste0(
        "BPIPD-270: expected one `",
        part,
        "` codebook workbook, found ",
        length(paths),
        "."
      ),
      call. = FALSE
    )
  }
  paths
}

#' Variable and value labels from one codebook sheet
bp270_read_codebook <- function(path, sheet) {
  raw <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal"
  )
  if (ncol(raw) < 6) {
    stop(
      paste0(
        "BPIPD-270: codebook sheet `",
        sheet,
        "` has fewer than 6 columns."
      ),
      call. = FALSE
    )
  }
  cells <- lapply(raw[, seq_len(6)], \(x) bp270_clean_text(as.character(x)))
  names(cells) <- c(
    "question",
    "part",
    "item",
    "variable",
    "value",
    "description"
  )
  cells <- as.data.frame(cells, stringsAsFactors = FALSE)

  header <- match("Variable name", cells$variable)
  if (is.na(header) || header >= nrow(cells)) {
    stop(
      paste0(
        "BPIPD-270: codebook sheet `",
        sheet,
        "` has no `Variable name` header row."
      ),
      call. = FALSE
    )
  }
  body <- cells[seq.int(header + 1, nrow(cells)), , drop = FALSE]
  body <- body[rowSums(!is.na(body)) > 0, , drop = FALSE]

  question <- bp270_fill_down(body$question)
  part <- bp270_fill_down(body$part, !is.na(body$question))
  item <- bp270_fill_down(
    body$item,
    !is.na(body$question) | !is.na(body$part)
  )

  starts <- which(!is.na(body$variable))
  block <- cumsum(!is.na(body$variable))

  dplyr::bind_rows(lapply(seq_along(starts), function(i) {
    at <- starts[[i]]
    rows <- block == i
    code <- suppressWarnings(as.numeric(body$value[rows]))
    description <- body$description[rows]
    coded <- !is.na(code) & !is.na(description)

    parts <- c(question[[at]], part[[at]], item[[at]])
    if (!any(coded)) {
      parts <- c(parts, description[!is.na(description)])
    }
    parts <- unique(parts[!is.na(parts) & parts != "-"])

    values <- stats::setNames(code[coded], description[coded])
    tibble::tibble(
      variable = bp270_expand_variable(body$variable[[at]]),
      label = if (length(parts) == 0) {
        NA_character_
      } else {
        paste(parts, collapse = " - ")
      },
      values = list(values[!duplicated(values)])
    )
  }))
}

#' Carry a merged cell down its rows, restarting wherever its parent restarts
bp270_fill_down <- function(x, reset = FALSE) {
  x[reset & is.na(x)] <- ""
  last <- cummax(ifelse(is.na(x), 0L, seq_along(x)))
  out <- rep(NA_character_, length(x))
  out[last > 0] <- x[last[last > 0]]
  out[!is.na(out) & out == ""] <- NA_character_
  out
}

#' Expand the codebook's `PRELATE1~8` range notation into one name per column
bp270_expand_variable <- function(x) {
  parts <- regmatches(x, regexec("^(.*?)([0-9]+)~([0-9]+)$", x))[[1]]
  if (length(parts) != 4) {
    return(x)
  }
  paste0(parts[[2]], seq.int(as.integer(parts[[3]]), as.integer(parts[[4]])))
}

#' Attach one column's label, and its value labels when the codebook codes it
bp270_label_column <- function(x, label, values) {
  label <- if (is.na(label)) NULL else label
  if (length(values) > 0 && is.numeric(x)) {
    return(haven::labelled(
      as.numeric(x),
      labels = stats::setNames(as.numeric(values), names(values)),
      label = label
    ))
  }
  attr(x, "label") <- label
  x
}

#' Trim the codebook's stray whitespace and line breaks out of a cell
bp270_clean_text <- function(x) {
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x[!is.na(x) & x == ""] <- NA_character_
  x
}
