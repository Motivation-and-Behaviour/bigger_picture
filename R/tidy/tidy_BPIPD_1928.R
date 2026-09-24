#' Tidier for BPIPD-1928 (PeNSE, Pesquisa Nacional de Saude do Escolar)
#'
#' Repeated cross-section: each edition draws an independent sample, so
#' editions stack and no participant is followed. From 2012 the merged
#' student-plus-school file is used (the separate student/school files are
#' not read); 2009 predates the school questionnaire and ships one flat
#' fixed-width student file. Labels come from each edition's dictionary
#' sheets, since the raw releases carry none.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per student per edition
tidy_BPIPD_1928 <- function(raw_dataset, spec) {
  editions <- bp1928_editions()

  parts <- Map(
    function(wave, edition) bp1928_take(raw_dataset$data, wave, edition),
    names(editions),
    editions
  )
  df <- dplyr::relocate(dplyr::bind_rows(parts), "participant_id")

  sheets <- bp1928_sheets(spec)
  by_edition <- lapply(editions, function(edition) {
    bp1928_labels(raw_dataset$codebook, sheets, edition$codebooks)
  })
  labels <- Reduce(
    function(carried, edition) {
      carried[names(edition)] <- edition
      carried
    },
    by_edition
  )
  for (column in names(df)) {
    label <- bp1928_label(labels, by_edition, column, names(editions))
    if (!is.na(label)) {
      attr(df[[column]], "label") <- label
    }
  }

  if (anyDuplicated(df$participant_id) > 0) {
    stop("BPIPD-1928: `participant_id` is not unique.", call. = FALSE)
  }

  df
}

#' The merged student file, dictionary sheets and row key of each edition
#'
#' `data` is the student-plus-school file (the union of the separate
#' student/school files, so those aren't read). `codebooks` names each dictionary sheet with the columns
#' holding the variable name and description: 2009/2012 are file layouts with
#' description in column 6; 2024 leads with type and width. `id` is the
#' study's own row key (2009/2012 have none). `na` is 2009's own missing
#' marker. `consent` names 2009's consent item; only 2009 still holds
#' students who declined to take part.
bp1928_editions <- function() {
  list(
    "2009" = list(
      data = "PeNSE 2009 Data",
      codebooks = list("PeNSE 2009 Dictionary" = c(1L, 6L)),
      id = NULL,
      na = ".",
      consent = "B00P01"
    ),
    "2012" = list(
      data = "PeNSE 2012 Data - EstudantesEscolas",
      codebooks = list("PeNSE 2012 Dictionary - EstudantesEscolas" = c(1L, 6L)),
      id = NULL
    ),
    "2015_sample_1" = list(
      data = "PeNSE 2015 Sample 1 - AlunoEscola",
      codebooks = list(
        "PeNSE 2015 Sample 1 Dictionary - Aluno" = c(1L, 2L),
        "PeNSE 2015 Sample 1 Dictionary - Escola" = c(1L, 2L)
      ),
      id = "aluno"
    ),
    "2015_sample_2" = list(
      data = "PeNSE 2015 Sample 2 - AlunoEscola",
      codebooks = list(
        "PeNSE 2015 Sample 2 Dictionary - Aluno" = c(1L, 2L),
        "PeNSE 2015 Sample 2 Dictionary - Escola" = c(1L, 2L)
      ),
      id = "aluno"
    ),
    "2019" = list(
      data = "PeNSE 2019 Data",
      codebooks = list(
        "PeNSE 2019 Dictionary - Variaveis cadastrais e amostra" = c(1L, 2L),
        "PeNSE 2019 Dictionary - Questionario ALUNO" = c(1L, 2L),
        "PeNSE 2019 Dictionary - Questionario ESCOLA" = c(1L, 2L)
      ),
      id = c("ESTRATO", "ESCOLA", "TURMA", "ALUNO")
    ),
    "2024" = list(
      data = "PeNSE 2024 Data",
      codebooks = list("PeNSE 2024 Dictionary" = c(3L, 4L)),
      id = c("ESTRATO_EXP", "ESCOLA", "TURMA", "ALUNO")
    )
  )
}

#' One edition's merged student table, keyed and typed for binding
bp1928_take <- function(tables, wave, edition) {
  tbl <- tibble::as_tibble(tables[[edition$data]])

  # 2009's SAS flat file writes missing as `.` for every numeric field
  # (`Input SAS/SAS_2009.xls` reads all but two fields as numeric).
  if (!is.null(edition$na)) {
    tbl[] <- lapply(tbl, function(x) {
      x[!is.na(x) & x == edition$na] <- NA
      x
    })
  }

  # Keep only valid questionnaires: from 2019 the file is padded with rows for
  # enrolled but non-attending students (`IND_EXPANSAO`), and 2009 still holds
  # students who declined, which later releases already exclude.
  keep <- rep(TRUE, nrow(tbl))
  if ("IND_EXPANSAO" %in% names(tbl)) {
    keep <- keep & as.numeric(tbl$IND_EXPANSAO) %in% 1
  }
  if (!is.null(edition$consent)) {
    keep <- keep & as.numeric(tbl[[edition$consent]]) %in% 1
  }
  position <- which(keep)
  tbl <- tbl[keep, , drop = FALSE]

  coded <- setdiff(names(tbl), c(".wave", ".wave_label"))
  tbl[coded] <- lapply(tbl[coded], bp1928_as_number)

  recoded <- intersect(bp1928_recoded_items(), names(tbl))
  tbl <- dplyr::rename_with(
    tbl,
    ~ paste0(.x, "_", wave),
    dplyr::all_of(recoded)
  )

  # 2009/2012 have no student identifier (2009's `ID` is the school), so row
  # position completes their key.
  key <- if (is.null(edition$id)) {
    list(position)
  } else {
    lapply(unname(as.list(tbl[edition$id])), bp1928_key_part)
  }
  tbl$participant_id <- do.call(paste, c(list(wave), key, list(sep = "-")))

  tbl
}

#' Items whose codes changed meaning between editions without a new name
#'
#' PeNSE renames a revised item (`B03009`, `VB03009A`, `B03009B`, `B03009C`),
#' but these six keep their name while the code list shifts: `B01003` moves
#' from single years to age bands; `B04003`, `B08007` and `E01P01` gain or
#' lose a leading category; `V0008` swaps Federal with Municipal between the
#' two 2015 samples; `CAPITAL` is the capital's state code in 2009 but a
#' capital/not-capital flag in 2012 (1/2) and 2024 (0/1). Each edition's copy
#' is suffixed with the edition.
bp1928_recoded_items <- function() {
  c("B01003", "B04003", "B08007", "CAPITAL", "E01P01", "V0008")
}

#' Read a column of digits stored as text back as a number
#'
#' Some coded items are text in one edition and numeric in the next, which
#' `bind_rows()` refuses. Genuinely textual columns (`DEPENDADM`, `POSEST`)
#' don't parse and are left alone.
bp1928_as_number <- function(x) {
  parsed <- suppressWarnings(as.numeric(x))
  if (!is.character(x) || anyNA(parsed[!is.na(x)])) {
    x
  } else {
    parsed
  }
}

#' One part of a row key, written out rather than rendered in scientific notation
bp1928_key_part <- function(x) {
  if (is.numeric(x)) {
    sprintf("%.0f", x)
  } else {
    as.character(x)
  }
}

#' The sheet each spec resource names
bp1928_sheets <- function(spec) {
  resources <- unlist(lapply(spec$waves, `[[`, "resources"), recursive = FALSE)
  sheets <- vapply(
    resources,
    function(resource) {
      if (is.null(resource$sheet)) NA_character_ else resource$sheet
    },
    character(1)
  )

  stats::setNames(sheets, vapply(resources, `[[`, character(1), "name"))
}

#' Variable labels from one edition's dictionary sheets
bp1928_labels <- function(paths, sheets, codebooks) {
  labels <- lapply(names(codebooks), function(name) {
    bp1928_read_labels(paths[[name]], sheets[[name]], codebooks[[name]])
  })

  Reduce(
    function(carried, sheet) {
      c(carried, sheet[!names(sheet) %in% names(carried)])
    },
    labels
  )
}

#' Variable names and descriptions from one dictionary sheet
#'
#' Sheets interleave a variable's row with its response-category rows;
#' category rows leave the name cell empty, so a pair is kept only where
#' both cells are filled.
bp1928_read_labels <- function(path, sheet, columns) {
  rows <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )

  name <- toupper(trimws(as.character(rows[[columns[[1]]]])))
  label <- trimws(gsub("[[:space:]]+", " ", as.character(rows[[columns[[2]]]])))
  keep <- !is.na(name) &
    !is.na(label) &
    nzchar(name) &
    nzchar(label) &
    !duplicated(name)

  stats::setNames(label[keep], name[keep])
}

#' The dictionary description of one column
#'
#' A `bp1928_recoded_items()` column suffixed with its edition uses that
#' edition's own dictionary, since the suffix marks exactly where editions
#' disagree; every other column takes the merged (most recent) description.
#' 2019 lower-cases the suffix (`B03009b` for `B03009B`); the 2012 merged
#' file suffixes `N` onto four items renumbered when it merged the student
#' and school files (`B01010N` for `B01010`).
bp1928_label <- function(labels, by_edition, column, waves) {
  suffix <- paste0("_(", paste(waves, collapse = "|"), ")$")
  edition <- regmatches(column, regexpr(suffix, column))
  key <- toupper(sub(suffix, "", column))
  if (length(edition) > 0L) {
    labels <- by_edition[[substring(edition, 2L)]]
  }

  label <- unname(labels[key])
  if (is.na(label)) {
    label <- unname(labels[sub("N$", "", key)])
  }

  label
}
