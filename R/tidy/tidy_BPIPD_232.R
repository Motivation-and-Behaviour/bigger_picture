#' Tidier for BPIPD-232 (NSCH)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per sampled child per survey year (the NSCH is a
#'   repeated cross-section)
tidy_BPIPD_232 <- function(raw_dataset, spec) {
  dfs <- raw_dataset$data

  needed <- paste0("nsch_", 2016:2023, "_data")
  absent <- setdiff(needed, names(dfs))
  if (length(absent) > 0) {
    stop(
      paste0(
        "BPIPD-232: missing data resource(s): ",
        paste(absent, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  df_2016 <- dfs$nsch_2016_data |>
    # 2016 codes the age form T1/T2/T3 where every later year uses 1/2/3.
    dplyr::mutate(FORMTYPE = readr::parse_number(FORMTYPE)) |>
    dplyr::rename_with(~ bp232_align_names(.x, "16"))
  df_2017 <- dfs$nsch_2017_data |>
    dplyr::rename_with(~ bp232_align_names(.x, "17"))
  df_2018 <- dfs$nsch_2018_data |>
    dplyr::rename_with(~ bp232_align_names(.x, "18"))
  df_2019 <- dfs$nsch_2019_data |>
    dplyr::rename_with(~ bp232_align_names(.x, "19"))
  df_2020 <- dfs$nsch_2020_data |>
    dplyr::rename_with(~ bp232_align_names(.x, "20"))
  df_2021 <- dfs$nsch_2021_data |>
    dplyr::rename_with(~ bp232_align_names(.x, "21"))
  df_2022 <- dfs$nsch_2022_data |>
    dplyr::rename_with(~ bp232_align_names(.x, "22"))
  df_2023 <- dfs$nsch_2023_data |>
    dplyr::rename_with(~ bp232_align_names(.x, "23"))

  # Two columns have a year that is not the file's: `schlsafe_17` is the 2018
  # file's second copy of its `SchlSafe_18`. `fasd_2022` is an all-NA
  #placeholder that appears only in the 2021 file, and is not the 2022 file's
  # `FASD_22` (which aligns to `fasd_nschyr`).
  out <- dplyr::bind_rows(
    df_2016,
    df_2017,
    df_2018,
    df_2019,
    df_2020,
    df_2021,
    df_2022,
    df_2023
  )

  # The NSCH samples one child per household, and `hhid` is distinct within
  # and across the eight annual files
  if (anyDuplicated(out$hhid) > 0) {
    stop("BPIPD-232: `hhid` does not uniquely identify rows.", call. = FALSE)
  }

  # The CSV releases carry no labels; each year's workbook supplies them.
  bp232_apply_labels(out, bp232_labels(raw_dataset))
}

#' One name per construct across the eight annual releases
bp232_align_names <- function(x, year_suffix) {
  # `TOTAGE_12_17` is an age range, not a 2017 release stamp: it is the third
  # of the triple `TOTAGE_0_5` / `TOTAGE_6_11` / `TOTAGE_12_17` that every one
  # of the eight files has
  renamed <- ifelse(
    x == "TOTAGE_12_17",
    x,
    sub(paste0("_", year_suffix, "$"), "_nschyr", x)
  )

  tolower(renamed)
}

#' Variable labels for the stacked table, keyed by aligned column name
#'
#' Each year declares a "Variable labels" workbook among its `docs`. Where the
#' wording of a label differs between years, the latest year in which the
#' column appears is used.
bp232_labels <- function(raw_dataset) {
  matches <- raw_dataset$meta$matches
  books <- matches[
    matches$role == "docs" & grepl("\\.xlsx$", matches$file),
    ,
    drop = FALSE
  ]

  labels <- character(0)
  for (year in 2016:2023) {
    path <- books$file[books$resource_name == paste0("nsch_", year, "_docs")]
    if (length(path) != 1L) {
      stop(
        paste0("BPIPD-232: no single variable-label workbook for ", year),
        call. = FALSE
      )
    }
    year_labels <- bp232_read_labels(path)
    names(year_labels) <- bp232_align_names(
      names(year_labels),
      substr(as.character(year), 3L, 4L)
    )
    labels[names(year_labels)] <- year_labels
  }

  labels
}

#' Read one year's variable-label workbook
bp232_read_labels <- function(path) {
  # One sheet of name/label pairs, but only four of the eight workbooks put a
  # `Name`/`Label` header on it, so the sheet is read without one and the
  # header dropped where it is present.
  info <- readxl::read_excel(
    path,
    sheet = 1L,
    col_names = FALSE,
    .name_repair = "minimal"
  )[, 1:2]
  names(info) <- c("variable", "label")
  info <- info[!is.na(info$variable) & !is.na(info$label), , drop = FALSE]
  header <- tolower(info$variable) == "name" &
    tolower(info$label) %in% c("label", "labels")
  info <- info[!header, , drop = FALSE]

  stats::setNames(as.character(info$label), as.character(info$variable))
}

#' Attach the variable labels to the columns they describe
bp232_apply_labels <- function(tbl, labels) {
  for (column in intersect(names(tbl), names(labels))) {
    attr(tbl[[column]], "label") <- unname(labels[[column]])
  }

  tbl
}
