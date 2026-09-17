#' Read the files named by a slice of a resource index
#'
#' Takes index rows rather than a spec so the pipeline can branch over batches
#' of rows and read them on separate workers. Returns one record per row,
#' carrying `seq` so `assemble_raw_dataset()` can restore spec order no matter
#' which order the branches finish in.
#'
#' Only `data` rows need reading; `codebook` and `docs` resources are recorded
#' by path alone, and `assemble_raw_dataset()` takes those straight from the
#' index.
read_resource_files <- function(index_rows) {
  lapply(seq_len(nrow(index_rows)), function(i) {
    row <- index_rows[i, , drop = FALSE]
    opts <- row$read_opts[[1]]

    tbl <- read_tabular_file(
      row$file,
      reader = row$reader,
      sheet = opts$sheet,
      range = opts$range,
      table = opts$table,
      object = opts$object
    )

    if (!is.na(row$wave)) {
      # `label` is optional in the spec; `build_resource_index()` has already
      # fallen back to the wave identifier.
      tbl <- dplyr::mutate(
        tbl,
        .wave = as.character(row$wave),
        .wave_label = as.character(row$wave_label)
      )
    }

    list(seq = row$seq, name = row$element_name, table = tbl)
  })
}

#' Rebuild the raw dataset structure from read batches and the resource index
#'
#' `parts` is the list of `read_resource_files()` results, one per branch. Order
#' is restored from `seq`, not from the order branches arrive in.
assemble_raw_dataset <- function(parts, index, spec, dataset_dir) {
  records <- unlist(parts, recursive = FALSE, use.names = FALSE)

  data <- list()
  if (length(records) > 0) {
    order <- order(vapply(records, function(r) r$seq, integer(1)))
    records <- records[order]
    data <- lapply(records, function(r) r$table)
    names(data) <- vapply(records, function(r) r$name, character(1))
  }

  codebook_rows <- index[index$role == "codebook", , drop = FALSE]
  codebook <- list()
  if (nrow(codebook_rows) > 0) {
    codebook <- as.list(codebook_rows$file)
    names(codebook) <- codebook_rows$element_name
  }

  list(
    data = data,
    codebook = codebook,
    docs = unique(index$file[index$role == "docs"]),
    meta = list(
      spec = spec,
      dataset_dir = dataset_dir,
      matches = index[, c("resource_name", "role", "base_dir", "file")]
    )
  )
}

#' Read a whole dataset in one call
#'
#' The pipeline builds the index, batches, and assembles as separate targets so
#' reads can run in parallel. This wrapper does the same work in one process,
#' for tests and interactive use.
read_dataset_from_spec <- function(dataset_dir, spec) {
  index <- build_resource_index(dataset_dir, spec)
  data_rows <- index[index$role == "data", , drop = FALSE]

  assemble_raw_dataset(
    list(read_resource_files(data_rows)),
    index,
    spec,
    dataset_dir
  )
}
