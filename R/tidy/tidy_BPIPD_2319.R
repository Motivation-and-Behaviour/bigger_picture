#' Tidier for BPIPD-2319 (Health behaviours of the Icelandic youth study)
#'
#' Two cohort studies, Heilsuhegdun (2006-2017) and Atgervi (2003, 2011), each a
#' spine of long files on `code` + `year`, stacked; wide twins are ignored.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant per measurement year they attended
tidy_BPIPD_2319 <- function(raw_dataset, spec) {
  tables <- raw_dataset$data

  heilsuhegdun <- tables$anthropometrics_long_data |>
    bp2319_add(bp2319_split_total_time(tables$blood_fitness_long_data)) |>
    bp2319_add(tables$questionnaire_long_data) |>
    bp2319_add(bp2319_actigraphy_long(tables$actigraphy_sleep_wide_data)) |>
    bp2319_add(bp2319_actigraphy_long(tables$actigraphy_activity_wide_data)) |>
    bp2319_add(tables$participants_data, by = "code")

  atgervi <- bp2319_numeric_columns(tables$atgervi_anthropometrics_long_data) |>
    bp2319_add(bp2319_numeric_columns(
      tables$atgervi_blood_fitness_long_data
    )) |>
    bp2319_add(tables$atgervi_questionnaire_long_data) |>
    bp2319_add(bp2319_numeric_columns(tables$atgervi_actigraphy_long_data)) |>
    bp2319_add(
      bp2319_numeric_columns(tables$atgervi_participants_data),
      by = "code"
    )

  df <- dplyr::bind_rows(heilsuhegdun, atgervi) |>
    dplyr::relocate("code", "year", "study", "cohort")

  # Every participant has a roster row in every year of their study, and the
  # `present_*` flags disagree with the data, so attendance is any measured item.
  design <- c(
    "code",
    "year",
    "age_group",
    "age_years",
    names(tables$participants_data),
    names(tables$atgervi_participants_data)
  )
  measured <- setdiff(names(df), design)
  observed <- rowSums(!is.na(df[measured])) > 0L
  # Ages at 2006 and 2008 cover more children than were invited (method report 3.3).
  age_attended <- !is.na(df$age_years) & !df$year %in% c(2006, 2008)
  df <- df[observed | age_attended, , drop = FALSE]

  if (anyDuplicated(df[c("code", "year")]) > 0) {
    stop(
      "BPIPD-2319: `code` and `year` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df <- bp2319_label_columns(
    tibble::as_tibble(df),
    bp2319_codebook_labels(raw_dataset)
  )

  # The dictionary labels `year` from its Atgervi sheet, naming only 2003 and 2011.
  attr(df$year, "label") <- "Measurement year"

  bp2319_carry_forward(bp2319_realign_weekend_2008(df))
}

#' Move the 2008 weekend screen values back under the items they measure
bp2319_realign_weekend_2008 <- function(df) {
  # Each 2008 value sits one column right of its label: weekday items correlate
  # with the column to their right (games 0.73, TV 0.53, internet 0.75, music
  # 0.88), not the labelled pair (0.23, 0.09, 0.12), while 2006 matches as labelled.
  columns <- c(
    "AQ20a_hoursweeke_compgames",
    "AQ20b_hoursweeke_tele",
    "AQ20c_hoursweeke_inter",
    "AQ20d_hoursweeke_music",
    "AQ20e_hoursweeke_read"
  )
  labels <- c(
    "Weekend hours band: computer or video games",
    "Weekend hours band: TV or videos",
    "Weekend hours band: internet",
    "Weekend hours band: music",
    "Weekend hours band: reading"
  )
  rows <- df$year == 2008
  for (i in seq_along(columns)) {
    moved <- if (i < length(columns)) df[[columns[i + 1L]]][rows] else NA_real_
    df[[columns[i]]][rows] <- moved
    attr(df[[columns[i]]], "label") <- paste0(
      labels[i],
      "; 2008 realigned, values moved one column left to the item they measure",
      if (i == length(columns)) " (no 2008 value for this item)"
    )
  }

  df
}

#' Carry parent education and fill the Atgervi 2011 age within participant
bp2319_carry_forward <- function(df) {
  carried <- "; carried within participant from the earliest year recorded"

  # Q5 and Q8 tick one or more levels, so each parent's block is carried whole.
  df <- bp2319_carry_block(df, grep("^Q5_fathered_", names(df), value = TRUE))
  df <- bp2319_carry_block(df, grep("^Q8_mothered_", names(df), value = TRUE))
  for (column in grep("^Q[58]_(fa|mo)thered_", names(df), value = TRUE)) {
    attr(df[[column]], "label") <- paste0(
      "Parent education level ticked (",
      column,
      ")",
      carried
    )
  }

  df$ses_parent_university <- carry_within(
    df$ses_parent_university,
    df$code,
    df$year
  )
  attr(df$ses_parent_university, "label") <- paste0(
    "Either parent university (self-report, 2017)",
    carried
  )

  # Atgervi 2011 has no age item; its waves are 8 years apart (cohorts.docx).
  age_2003 <- df$age_years[df$year == 2003]
  age_2003 <- age_2003[match(df$code, df$code[df$year == 2003])]
  fill <- df$year == 2011 & is.na(df$age_years)
  df$age_years[fill] <- age_2003[fill] + 8
  attr(df$age_years, "label") <-
    "Age at measurement in years; 2011 filled as the 2003 age + 8 years"

  df
}

#' Fill rows that answered none of `columns` from the earliest row that did
bp2319_carry_block <- function(df, columns) {
  answered <- rowSums(!is.na(df[columns])) > 0L
  source_row <- carry_within(
    ifelse(answered, seq_len(nrow(df)), NA_integer_),
    df$code,
    df$year
  )
  fill <- !answered & !is.na(source_row)
  for (column in columns) {
    df[[column]][fill] <- df[[column]][source_row[fill]]
  }

  df
}

#' Add a table's new columns on the participant-year (or participant) key
bp2319_add <- function(base, tbl, by = c("code", "year")) {
  tbl <- tbl[, c(by, setdiff(names(tbl), names(base)))]
  relationship <- if (identical(by, "code")) "many-to-one" else "one-to-one"

  dplyr::left_join(base, tbl, by = by, relationship = relationship)
}

#' Turn a wide Heilsuhegdun actigraphy file into one row per participant-year
bp2319_actigraphy_long <- function(tbl) {
  dates <- grep("date_20(15|17)$", names(tbl), value = TRUE)
  tbl[dates] <- lapply(tbl[dates], bp2319_as_date)

  tidyr::pivot_longer(
    tbl,
    cols = -"code",
    names_to = c(".value", "year"),
    names_pattern = "^(.*)_(2015|2017)$",
    names_transform = list(year = as.numeric)
  )
}

#' Read a date that Excel gave as a serial number in one year and text in another
bp2319_as_date <- function(x) {
  if (is.numeric(x)) {
    as.Date(x, origin = "1899-12-30")
  } else {
    as.Date(x, format = "%m/%d/%Y")
  }
}

#' Split the ergometer `total_time` into its mm:ss and decimal-minute codings
bp2319_split_total_time <- function(tbl) {
  clock <- grepl("^[0-9]{1,2}:[0-9]{2}$", tbl$total_time)
  tbl$total_time_mmss <- ifelse(clock, tbl$total_time, NA_character_)
  tbl$total_time_min <- suppressWarnings(as.numeric(
    ifelse(clock, NA_character_, tbl$total_time)
  ))
  tbl$total_time <- NULL

  attr(tbl$total_time_mmss, "label") <- "Time on ergometer, mm:ss (2006, 2008)"
  attr(tbl$total_time_min, "label") <- "Time on ergometer, minutes (2015, 2017)"
  tbl
}

#' Make numeric the Atgervi columns typed as text by a literal `NA` string
bp2319_numeric_columns <- function(tbl) {
  for (column in names(tbl)[vapply(tbl, is.character, logical(1))]) {
    values <- tbl[[column]]
    values[values == "NA"] <- NA_character_
    numbers <- suppressWarnings(as.numeric(values))
    if (!any(is.na(numbers) & !is.na(values))) {
      tbl[[column]] <- numbers
    }
  }

  tbl
}

#' Variable labels from the two codebook workbooks, dictionary first
bp2319_codebook_labels <- function(raw_dataset) {
  dictionary <- bp2319_sheet_labels(
    raw_dataset$codebook$variable_dictionary_codebook_atgervi,
    name = 2L,
    label = 3L
  )
  questions <- bp2319_sheet_labels(
    raw_dataset$codebook$questionnaire_codebook_atg_2003,
    name = 1L,
    label = 3L
  )

  c(dictionary, questions[setdiff(names(questions), names(dictionary))])
}

#' Name and label columns from every sheet of one codebook workbook
bp2319_sheet_labels <- function(path, name, label) {
  sheets <- lapply(readxl::excel_sheets(path), function(sheet) {
    tbl <- readxl::read_excel(
      path,
      sheet = sheet,
      col_types = "text",
      .name_repair = "minimal"
    )
    stats::setNames(as.character(tbl[[label]]), as.character(tbl[[name]]))
  })

  labels <- unlist(sheets, use.names = TRUE)
  labels <- labels[!is.na(labels) & !is.na(names(labels))]
  labels[!duplicated(names(labels))]
}

#' Attach codebook labels to the columns they describe
bp2319_label_columns <- function(tbl, labels) {
  for (column in intersect(names(tbl), names(labels))) {
    attr(tbl[[column]], "label") <- unname(labels[[column]])
  }

  tbl
}
