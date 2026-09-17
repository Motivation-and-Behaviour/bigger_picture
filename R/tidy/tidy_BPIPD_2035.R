#' Tidier for BPIPD-2035 (National Survey on Cyber Violence Among Adolescents)
#'
#' Three independent cross-sections of school students (2020, 2021, 2023), one
#' Excel workbook each with its own questionnaire and column names; the adult,
#' teacher and parent workbooks are separate samples and are not combined here.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per respondent per survey year
tidy_BPIPD_2035 <- function(raw_dataset, spec) {
  id_columns <- bp2035_surveys()
  codebook_names <- paste0(names(id_columns), "_codebook")

  missing <- c(
    setdiff(names(id_columns), names(raw_dataset$data)),
    setdiff(codebook_names, names(raw_dataset$codebook))
  )
  if (length(missing) > 0) {
    stop(
      "BPIPD-2035: youth survey resources missing from the ingested dataset: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  labels <- lapply(raw_dataset$codebook[codebook_names], bp2035_read_labels)

  parts <- lapply(names(id_columns), function(name) {
    bp2035_shape_survey(
      raw_dataset$data[[name]],
      name = name,
      id_column = id_columns[[name]],
      labels = labels[[paste0(name, "_codebook")]]
    )
  })

  df <- dplyr::bind_rows(parts)

  if (anyDuplicated(df$participant_id) > 0) {
    stop(
      "BPIPD-2035: `participant_id` does not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' The youth surveys and the respondent key each one uses
bp2035_surveys <- function() {
  c(
    cyber_violence_2020_students = "ID",
    cyber_violence_2021_students = "idx",
    cyber_violence_2023_adolescents = "id"
  )
}

#' English labels for the 2021 design columns the codebook leaves blank
bp2035_design_labels <- function() {
  stats::setNames(
    c(
      "Respondent serial number",
      "Province or metropolitan city",
      "School level",
      "High school type (high school respondents only)",
      "School sex composition (secondary respondents only)",
      "Settlement size",
      "Region code"
    ),
    c(
      "idx",
      "시도",
      "학교급",
      "고등학교유형",
      "남녀공학구분",
      "지역규모",
      "AREA"
    )
  )
}

#' Shape one survey file into its slice of the combined table
bp2035_shape_survey <- function(tbl, name, id_column, labels) {
  tbl <- tibble::as_tibble(tbl)

  if (!id_column %in% names(tbl)) {
    stop(
      "BPIPD-2035: respondent key `",
      id_column,
      "` is missing from ",
      name,
      call. = FALSE
    )
  }

  # `population` is the trailing token of the resource name; `wave` comes from
  # the spec, which the reader has already stamped onto every row.
  population <- sub("^cyber_violence_[0-9]{4}_", "", name)
  wave <- if (".wave" %in% names(tbl) && nrow(tbl) > 0L) {
    as.character(tbl$.wave[[1]])
  } else {
    NA_character_
  }

  if (population == name || is.na(wave)) {
    stop(
      "BPIPD-2035: cannot read population and wave from ",
      name,
      call. = FALSE
    )
  }

  if (!is.null(labels)) {
    tbl <- bp2035_apply_labels(tbl, labels)
  }

  # `.wave`/`.wave_label` are the reader's, not the study's, so they are
  # replaced by the identifier columns rather than carried through prefixed.
  body <- dplyr::select(tbl, -dplyr::any_of(c(".wave", ".wave_label")))

  # Each year is a different questionnaire, so the same name means a different
  # item from year to year (2020 `Q1` is an hours grid, 2021 `Q1` a band); the
  # prefix keeps them apart and `variables.csv` aligns the items across years.
  body <- dplyr::rename_with(body, ~ paste(population, wave, .x, sep = "_"))

  dplyr::bind_cols(
    tibble::tibble(
      participant_id = paste(population, wave, tbl[[id_column]], sep = "_"),
      wave = wave,
      population = population
    ),
    body
  )
}

#' Variable and value labels from a cyber-violence codebook
#'
#' The three youth workbooks carry the same two blocks in three layouts: the
#' 2023 codebook keeps them on a sheet each (`변수정보`, `변수값`), the 2021
#' workbook on an English-named pair (`Variable`, `Value`), and the 2020
#' workbook stacks both in one `변수가이드` sheet, with the marker row
#' `작업 파일의 변수` between them.
bp2035_read_labels <- function(path) {
  sheets <- readxl::excel_sheets(path)

  if (all(c("변수정보", "변수값") %in% sheets)) {
    info <- bp2035_read_sheet(path, "변수정보")[-(1:3), ]
    values <- bp2035_read_sheet(path, "변수값")
  } else if (all(c("Variable", "Value") %in% sheets)) {
    info <- bp2035_read_sheet(path, "Variable")[-1, ]
    values <- bp2035_read_sheet(path, "Value")
  } else if ("변수가이드" %in% sheets) {
    guide <- bp2035_read_sheet(path, "변수가이드")
    end <- match("작업 파일의 변수", guide$variable)
    if (is.na(end)) {
      stop("BPIPD-2035: no variable/value marker in 변수가이드.", call. = FALSE)
    }
    info <- guide[3:(end - 1), ]
    values <- guide
  } else {
    stop("BPIPD-2035: unrecognised codebook layout in ", path, call. = FALSE)
  }

  list(
    variable = bp2035_variable_labels(info),
    value = bp2035_value_labels(values)
  )
}

#' The first three columns of a codebook sheet, unheaded
#'
#' Every block is `variable`, `code`, `label`, but the header rows sit at a
#' different depth in each layout, so the sheets are read without column names
#' and sliced by the caller. `code` is the variable's position in the variable
#' block and the value being labelled in the value block.
bp2035_read_sheet <- function(path, sheet) {
  columns <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )[, 1:3]

  stats::setNames(columns, c("variable", "code", "label"))
}

#' Variable labels, with each item block's label carried onto its siblings
bp2035_variable_labels <- function(info) {
  info <- bp2035_fill_block_labels(info)
  info <- info[!is.na(info$variable) & !is.na(info$label), ]

  stats::setNames(as.character(info$label), as.character(info$variable))
}

#' Carry a multi-column item's label onto the rest of its block
#'
#' The 2021 `Variable` sheet writes the question text once, against the first
#' column of a block (`Q2_1` for `Q2_1`-`Q2_3`, `Q7_1_1 TO Q7_1_8` for that
#' block), and leaves the sibling columns' label cell empty. A sibling is
#' recognised by sharing the block's stem, so a column that merely follows a
#' block without belonging to it — `qa7`, after `Q6_1a` — keeps its empty label
#' rather than inheriting a question it does not ask. Only the block's question
#' text is inherited: the four ranked blocks (`Q2_1`, `Q5_1`, `Q22_1`, `Q23_1`)
#' open with a `[1순위]` marker that belongs to the first-choice column
#' alone, and the sheet states no rank for the siblings, so the marker is
#' dropped rather than repeated onto the second- and third-choice columns.
bp2035_fill_block_labels <- function(info) {
  stem <- NA_character_
  text <- NA_character_

  for (row in seq_len(nrow(info))) {
    variable <- as.character(info$variable[[row]])
    label <- as.character(info$label[[row]])

    if (!is.na(label)) {
      stem <- sub("_[^_]+$", "", variable)
      text <- sub("^\\S+( TO \\S+)?\\.[[:space:]]*", "", label)
      text <- sub("^\\[[0-9]+순위\\][[:space:]]*", "", text)
      next
    }

    sibling <- !is.na(variable) &&
      !is.na(stem) &&
      startsWith(variable, paste0(stem, "_"))
    if (sibling) {
      info$label[[row]] <- paste0(variable, ". ", text)
    }
  }

  info
}

#' Value labels, as one named numeric vector per variable
#'
#' The value block starts two rows below its `변수값` heading in all three
#' layouts, and names its variable only on the block's first row.
bp2035_value_labels <- function(values) {
  start <- match("변수값", values$variable)
  if (is.na(start) || start + 2 > nrow(values)) {
    stop("BPIPD-2035: no value block in the codebook sheet.", call. = FALSE)
  }

  block <- values[seq(start + 2, nrow(values)), ]
  block$variable <- bp2035_fill_down(as.character(block$variable))
  keep <- !is.na(block$variable) & !is.na(block$code) & !is.na(block$label)
  block <- block[keep, ]

  codes <- stats::setNames(
    suppressWarnings(as.numeric(block$code)),
    as.character(block$label)
  )

  split(codes, block$variable)
}

#' Repeat the last non-missing entry over the gaps that follow it
bp2035_fill_down <- function(x) {
  filled <- x
  for (i in seq_along(filled)) {
    if (is.na(filled[[i]]) && i > 1L) {
      filled[[i]] <- filled[[i - 1L]]
    }
  }

  filled
}

#' Attach codebook variable and value labels to the columns they describe
#'
#' Value labels are attached to numeric columns only. The 2021 workbook writes
#' its missing marker as the text `#NULL!`, so the Excel reader types most of
#' that year's coded columns as character; `as.numeric()` on a `haven_labelled`
#' character vector errors, and `variables.csv` reads those columns that way,
#' so labelling them would break the mapping. A list whose codes are not all
#' distinct numbers is left off too — no youth codebook has one today, but a
#' `haven::labelled()` built from one would be ambiguous or would error.
bp2035_apply_labels <- function(tbl, labels) {
  for (column in intersect(names(tbl), names(labels$variable))) {
    attr(tbl[[column]], "label") <- unname(labels$variable[[column]])
  }

  design <- bp2035_design_labels()
  for (column in intersect(names(tbl), names(design))) {
    if (is.null(attr(tbl[[column]], "label"))) {
      attr(tbl[[column]], "label") <- unname(design[[column]])
    }
  }

  for (column in intersect(names(tbl), names(labels$value))) {
    codes <- labels$value[[column]]
    usable <- is.numeric(tbl[[column]]) &&
      !anyNA(codes) &&
      anyDuplicated(codes) == 0L
    if (!usable) {
      next
    }

    tbl[[column]] <- haven::labelled(
      as.numeric(tbl[[column]]),
      labels = codes,
      label = attr(tbl[[column]], "label")
    )
  }

  tbl
}
