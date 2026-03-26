read_tabular_file <- function(path, reader, sheet = NULL, range = NULL) {
  reader <- tolower(reader)

  out <- switch(
    reader,
    "csv" = readr::read_csv(path, show_col_types = FALSE),
    "csv2" = readr::read_csv2(path, show_col_types = FALSE), # ; delimited
    "tsv" = readr::read_tsv(path, show_col_types = FALSE),
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
    stop("Unsupported reader: ", reader, " (", path, ")", call. = FALSE)
  )

  tibble::as_tibble(out)
}
