read_dataset_from_spec <- function(dataset_dir, spec) {
  data <- list()
  codebook <- list()
  docs <- character()
  meta <- tibble::tibble()

  # dataset-level resources
  if (!is.null(spec$resources)) {
    for (res in spec$resources) {
      chunk <- read_resource_table(res, base_dir = dataset_dir)
      data <- c(data, chunk$data)
      codebook <- c(codebook, chunk$codebook)
      docs <- unique(c(docs, chunk$docs))
      meta <- dplyr::bind_rows(meta, chunk$meta)
    }
  }

  # wave-level resources
  if (!is.null(spec$waves)) {
    for (w in spec$waves) {
      wave_subdir <- w$wave_dir
      if (is.null(wave_subdir) || !nzchar(wave_subdir)) {
        stop("Each wave must define `wave_dir`.", call. = FALSE)
      }
      wave_dir <- fs::path(dataset_dir, wave_subdir)

      for (res in w$resources) {
        chunk <- read_resource_table(
          res,
          base_dir = wave_dir,
          wave = w$wave,
          wave_label = w$label %||% NULL
        )

        data <- c(data, chunk$data)
        codebook <- c(codebook, chunk$codebook)
        docs <- unique(c(docs, chunk$docs))
        meta <- dplyr::bind_rows(meta, chunk$meta)
      }
    }
  }

  list(
    data = data,
    codebook = codebook,
    docs = docs,
    meta = list(spec = spec, dataset_dir = dataset_dir, matches = meta)
  )
}
