#' The largest `guess_max` readxl accepts without warning
excel_guess_max <- function() {
  .Machine$integer.max %/% 100
}

#' Which readers honour each spec-level read option
#'
#' Only the options introduced with the `fwf`/`por` readers are checked at
#' read time. `sheet`/`range`/`table`/`object` predate the check and were
#' always ignored by readers that do not use them; refusing them now would
#' change how existing specs build.
read_opt_readers <- function() {
  list(
    col_names = c("csv", "csv2", "tsv"),
    col_positions = "fwf",
    encoding = c("csv", "csv2", "tsv", "fwf", "stata", "spss", "sas")
  )
}

#' Refuse a read option the chosen reader cannot honour
#'
#' A silently ignored option is how three of the four datasets that motivated
#' these readers broke: the read "worked" and returned the wrong table.
check_read_opts <- function(reader, opts, path) {
  supported <- read_opt_readers()
  for (opt in names(supported)) {
    if (!is.null(opts[[opt]]) && !reader %in% supported[[opt]]) {
      stop(
        "Read option `",
        opt,
        "` is not supported by reader `",
        reader,
        "`; it applies to: ",
        paste(supported[[opt]], collapse = ", "),
        " (",
        path,
        ")",
        call. = FALSE
      )
    }
  }
  invisible(opts)
}

read_locale <- function(opts) {
  readr::locale(encoding = opts$encoding %||% "UTF-8")
}

#' Column-type letters `readr` accepts in a compact `col_types` string
fwf_type_letters <- function() {
  c("c", "d", "i", "l", "n", "D", "T", "t", "?", "_", "-")
}

#' Read and validate a fixed-width layout CSV
#'
#' The layout is the contract between a `.dat` file and its columns: `name`,
#' `start`, `end` (1-based, inclusive) and optionally `type`. Anything else in
#' the file is carried for provenance and ignored here. A bad layout fails
#' loudly, because `read_fwf()` itself would not: overlapping or misordered
#' fields read as plausible-looking garbage.
read_fwf_layout <- function(layout_path, data_path) {
  if (is.null(layout_path)) {
    stop(
      "Reader `fwf` needs `col_positions` in the dataset spec (",
      data_path,
      ")",
      call. = FALSE
    )
  }
  if (!fs::file_exists(layout_path)) {
    stop(
      "Fixed-width layout file not found: ",
      layout_path,
      " (for ",
      data_path,
      ")",
      call. = FALSE
    )
  }

  fail <- function(...) {
    stop("Invalid fixed-width layout ", layout_path, ": ", ..., call. = FALSE)
  }

  layout <- readr::read_csv(
    layout_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

  required <- c("name", "start", "end")
  missing <- setdiff(required, names(layout))
  if (length(missing) > 0) {
    fail("missing required column(s) ", paste(missing, collapse = ", "))
  }
  if (nrow(layout) == 0) {
    fail("no fields defined")
  }

  name <- trimws(layout$name)
  if (anyNA(name) || any(!nzchar(name))) {
    fail("every field needs a `name`")
  }
  dupes <- unique(name[duplicated(name)])
  if (length(dupes) > 0) {
    fail("duplicate field names: ", paste(dupes, collapse = ", "))
  }

  start <- suppressWarnings(as.integer(layout$start))
  end <- suppressWarnings(as.integer(layout$end))
  if (anyNA(start) || anyNA(end)) {
    fail("`start` and `end` must be whole numbers")
  }
  if (any(start < 1L)) {
    fail("`start` positions are 1-based; found ", min(start))
  }
  bad_span <- name[start > end]
  if (length(bad_span) > 0) {
    fail("`start` is after `end` for: ", paste(bad_span, collapse = ", "))
  }
  if (is.unsorted(start, strictly = TRUE)) {
    fail("fields must be listed in ascending `start` order")
  }
  overlap <- which(start[-1] <= end[-length(end)])
  if (length(overlap) > 0) {
    fail(
      "overlapping fields: ",
      paste0(name[overlap], "/", name[overlap + 1L], collapse = ", ")
    )
  }

  type <- NULL
  if ("type" %in% names(layout)) {
    type <- trimws(layout$type)
    type[is.na(type) | !nzchar(type)] <- "?"
    bad_type <- unique(type[!type %in% fwf_type_letters()])
    if (length(bad_type) > 0) {
      fail(
        "unknown `type` value(s) ",
        paste(bad_type, collapse = ", "),
        "; use one of ",
        paste(fwf_type_letters(), collapse = " ")
      )
    }
  }

  list(name = name, start = start, end = end, type = type)
}

#' The compact `col_types` string for a layout, or NULL to let readr guess
fwf_col_types <- function(layout) {
  if (is.null(layout$type)) {
    return(NULL)
  }
  paste(layout$type, collapse = "")
}

#' Warn when a layout reaches past the end of the first record
#'
#' `read_fwf()` returns NA for any field beyond the end of a line, so a layout
#' that is one column too wide reads cleanly and silently loses that field.
#' The width is measured in bytes because that is how `read_fwf()` counts
#' positions. Records wider than the layout are fine: trailing filler is
#' common.
check_fwf_record_width <- function(data_path, layout, layout_path) {
  first <- readLines(data_path, n = 1L, warn = FALSE)
  if (length(first) == 0) {
    return(invisible(NULL))
  }
  width <- nchar(first, type = "bytes")
  if (max(layout$end) > width) {
    warning(
      "Fixed-width layout ",
      layout_path,
      " ends at position ",
      max(layout$end),
      " but the first record of ",
      data_path,
      " is ",
      width,
      " bytes wide; fields past the end of a record read as NA.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Refuse a read that left parse failures behind
#'
#' `readr` records every cell it could not parse as the column's type in a
#' `problems` attribute, sets the cell to NA and moves on with a one-line
#' warning. The attribute lives only on the `spec_tbl_df` readr returns: the
#' `as_tibble()` below and the `.wave` mutate downstream both drop it, so by
#' the time a raw table reaches the store the failures are indistinguishable
#' from genuine missing values. This is the last place they are visible, so
#' it is where a read with failures stops. `problems()` is empty for anything
#' not read by readr, so the check is safe on every reader.
report_parse_problems <- function(out, path) {
  problems <- readr::problems(out)
  if (nrow(problems) == 0) {
    return(invisible(NULL))
  }

  col_names <- names(out)[problems$col]
  col_names[is.na(col_names)] <- paste0(
    "<column ",
    problems$col[is.na(col_names)],
    ">"
  )
  by_col <- table(col_names)

  stop(
    nrow(problems),
    " cell(s) in ",
    path,
    " did not parse as their column type and would silently become NA. ",
    "Columns affected: ",
    paste0(names(by_col), " (", as.integer(by_col), ")", collapse = ", "),
    ". First failure: row ",
    problems$row[1],
    ", expected ",
    problems$expected[1],
    ", got `",
    problems$actual[1],
    "`. Fix the layout or column type, or recode the value in the tidier ",
    "once it is read as text.",
    call. = FALSE
  )
}

#' Read one data file as a tibble
#'
#' `opts` is the resource's `read_opts` from the resource index: the optional
#' spec keys (`sheet`, `range`, `table`, `object`, `col_names`,
#' `col_positions`, `encoding`) as a named list, absent keys being NULL. The
#' set of keys is closed by `harmonisation/dataset.schema.json`; passing them
#' as a list just means a new option touches this file and the schema rather
#' than every caller in between.
read_tabular_file <- function(path, reader, opts = list()) {
  reader <- tolower(reader)
  opts <- opts %||% list()
  check_read_opts(reader, opts, path)

  out <- switch(
    reader,
    "csv" = readr::read_csv(
      path,
      col_names = opts$col_names %||% TRUE,
      locale = read_locale(opts),
      show_col_types = FALSE,
      guess_max = Inf
    ),
    "csv2" = readr::read_csv2(
      # ; delimited
      path,
      col_names = opts$col_names %||% TRUE,
      locale = read_locale(opts),
      show_col_types = FALSE,
      guess_max = Inf
    ),
    "tsv" = readr::read_tsv(
      path,
      col_names = opts$col_names %||% TRUE,
      locale = read_locale(opts),
      show_col_types = FALSE,
      guess_max = Inf
    ),
    "fwf" = {
      layout <- read_fwf_layout(opts$col_positions, path)
      check_fwf_record_width(path, layout, opts$col_positions)
      readr::read_fwf(
        path,
        col_positions = readr::fwf_positions(
          layout$start,
          layout$end,
          layout$name
        ),
        col_types = fwf_col_types(layout),
        locale = read_locale(opts),
        show_col_types = FALSE,
        guess_max = Inf
      )
    },
    "stata" = haven::read_dta(path, encoding = opts$encoding),
    "spss" = haven::read_sav(path, encoding = opts$encoding),
    "por" = haven::read_por(path),
    "sas" = haven::read_sas(path, encoding = opts$encoding),
    "rds" = readRDS(path),
    "parquet" = arrow::read_parquet(path),
    "excel" = {
      # readxl types each column from the first 1000 rows by default, so a
      # column that is blank early and populated later is typed `logical` and
      # its values are silently read as NA. Guess from the whole sheet, as the
      # delimited readers above already do. `Inf` would work but warns on every
      # read; excel_guess_max() is the ceiling readxl clamps it to anyway.
      args <- list(path = path, guess_max = excel_guess_max())
      if (!is.null(opts$sheet)) {
        args$sheet <- opts$sheet
      }
      if (!is.null(opts$range)) {
        args$range <- opts$range
      }
      do.call(readxl::read_excel, args)
    },
    "mdb" = {
      table <- opts$table
      if (is.null(table)) {
        tables <- mdbr::mdb_tables(path)
        if (length(tables) == 0) {
          stop("No tables found in mdb file (", path, ")", call. = FALSE)
        }
        if (length(tables) > 1) {
          stop(
            "Multiple tables in mdb file; set `table` in the dataset spec. ",
            "Available tables: ",
            paste(tables, collapse = ", "),
            " (",
            path,
            ")",
            call. = FALSE
          )
        }
        table <- tables
      }
      mdbr::read_mdb(path, table)
    },
    "rda" = {
      env <- new.env(parent = emptyenv())
      objects <- load(path, envir = env)
      object <- opts$object
      if (is.null(object)) {
        if (length(objects) == 0) {
          stop("No objects found in rda file (", path, ")", call. = FALSE)
        }
        if (length(objects) > 1) {
          stop(
            "Multiple objects in rda file; set `object` in the dataset spec. ",
            "Available objects: ",
            paste(objects, collapse = ", "),
            " (",
            path,
            ")",
            call. = FALSE
          )
        }
        object <- objects
      } else if (!object %in% objects) {
        stop(
          "Object not found in rda file: ",
          object,
          ". Available objects: ",
          paste(objects, collapse = ", "),
          " (",
          path,
          ")",
          call. = FALSE
        )
      }
      get(object, envir = env)
    },
    stop("Unsupported reader: ", reader, " (", path, ")", call. = FALSE)
  )

  report_parse_problems(out, path)
  tibble::as_tibble(out)
}
