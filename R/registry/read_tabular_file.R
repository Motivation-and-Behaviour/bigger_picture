read_tabular_file <- function(
  path,
  reader,
  sheet = NULL,
  range = NULL,
  table = NULL,
  object = NULL
) {
  reader <- tolower(reader)

  out <- switch(
    reader,
    "csv" = readr::read_csv(path, show_col_types = FALSE, guess_max = Inf),
    "csv2" = readr::read_csv2(path, show_col_types = FALSE, guess_max = Inf), # ; delimited
    "tsv" = readr::read_tsv(path, show_col_types = FALSE, guess_max = Inf),
    "stata" = haven::read_dta(path),
    "spss" = haven::read_sav(path),
    "sas" = haven::read_sas(path),
    "rds" = readRDS(path),
    "parquet" = arrow::read_parquet(path),
    "excel" = {
      args <- list(path = path)
      if (!is.null(sheet)) {
        args$sheet <- sheet
      }
      if (!is.null(range)) {
        args$range <- range
      }
      do.call(readxl::read_excel, args)
    },
    "mdb" = {
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

  tibble::as_tibble(out)
}
