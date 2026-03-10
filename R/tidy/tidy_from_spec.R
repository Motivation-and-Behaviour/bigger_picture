tidy_from_spec <- function(raw_dataset, spec) {
  fn_name <- spec$tidier %||% NA_character_

  if (is.na(fn_name) || !nzchar(fn_name)) {
    return(as_tidied_dataset(
      list(data = raw_dataset$data, meta = raw_dataset$meta),
      spec,
      tidier_name = "identity"
    ))
  }

  if (!exists(fn_name, mode = "function")) {
    stop("Tidier function not found: ", fn_name, call. = FALSE)
  }

  tidier <- get(fn_name, mode = "function")
  as_tidied_dataset(
    tidier(raw_dataset, spec),
    spec = spec,
    tidier_name = fn_name
  )
}

as_tidied_dataset <- function(x, spec, tidier_name) {
  if (!is.list(x) || !"data" %in% names(x)) {
    stop(
      "Tidier output must be a list with a `data` element.",
      call. = FALSE
    )
  }

  if (!is.list(x$data)) {
    stop(
      "Tidier output `data` must be a named list of tables.",
      call. = FALSE
    )
  }

  if (!is.null(x$meta) && !is.list(x$meta)) {
    stop(
      "Tidier output `meta` must be NULL or a list.",
      call. = FALSE
    )
  }

  meta <- x$meta %||% list()
  meta$spec <- meta$spec %||% spec
  meta$tidier <- tidier_name

  list(
    data = lapply(x$data, tibble::as_tibble),
    meta = meta
  )
}
