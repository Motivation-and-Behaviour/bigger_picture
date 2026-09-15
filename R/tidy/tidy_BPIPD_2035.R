#' Tidier for BPIPD-2035 (National Survey on Cyber Violence)
#'
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble
tidy_BPIPD_2035 <- function(raw_dataset, spec) {
  id_columns <- c(
    cyber_violence_2020_students = "ID",
    cyber_violence_2021_students = "idx",
    cyber_violence_2023_adolescents = "id"
  )

  missing <- setdiff(names(id_columns), names(raw_dataset$data))
  if (length(missing) > 0) {
    stop(
      "BPIPD-2035: youth survey missing from the ingested data: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  labels <- lapply(raw_dataset$codebook, read_cyber_violence_labels)

  parts <- lapply(names(id_columns), function(name) {
    tidy_cyber_violence_survey(
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

#' Shape one survey file into its slice of the combined table
tidy_cyber_violence_survey <- function(tbl, name, id_column, labels) {
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
  wave <- as.character(tbl$.wave[[1]])

  if (population == name || is.na(wave)) {
    stop(
      "BPIPD-2035: cannot read population and wave from ",
      name,
      call. = FALSE
    )
  }

  if (!is.null(labels)) {
    tbl <- apply_cyber_violence_labels(tbl, labels)
  }

  # `.wave`/`.wave_label` are the reader's, not the study's, so they are
  # replaced by the identifier columns rather than carried through prefixed.
  body <- dplyr::select(tbl, -dplyr::any_of(c(".wave", ".wave_label")))
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

#' Variable labels from a cyber-violence codebook
read_cyber_violence_labels <- function(path) {
  sheets <- readxl::excel_sheets(path)

  info <- if ("변수정보" %in% sheets) {
    readxl::read_excel(path, sheet = "변수정보", skip = 2)[, 1:3]
  } else if ("Variable" %in% sheets) {
    readxl::read_excel(path, sheet = "Variable")[, 1:3]
  } else if ("변수가이드" %in% sheets) {
    guide <- readxl::read_excel(
      path,
      sheet = "변수가이드",
      col_names = FALSE
    )[, 1:3]
    end <- match("작업 파일의 변수", guide[[1]])
    if (is.na(end)) {
      stop("BPIPD-2035: no variable/value marker in 변수가이드.", call. = FALSE)
    }
    guide[3:(end - 1), ]
  } else {
    stop("BPIPD-2035: unrecognised codebook layout in ", path, call. = FALSE)
  }

  names(info) <- c("variable", "position", "label")
  info <- info[!is.na(info$variable) & !is.na(info$label), ]

  stats::setNames(as.character(info$label), as.character(info$variable))
}

#' Attach codebook labels to the columns they describe
apply_cyber_violence_labels <- function(tbl, labels) {
  for (column in intersect(names(tbl), names(labels))) {
    attr(tbl[[column]], "label") <- unname(labels[[column]])
  }

  tbl
}
