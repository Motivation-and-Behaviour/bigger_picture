tidy_from_spec <- function(raw_dataset, spec) {
  fn_name <- conventional_tidier_name(spec$dataset_id)

  if (!exists(fn_name, mode = "function")) {
    stop("Tidier function not found: ", fn_name, call. = FALSE)
  }

  tidier <- get(fn_name, mode = "function")
  as_tidied_dataset(tidier(raw_dataset, spec), tidier_name = fn_name)
}

as_tidied_dataset <- function(x, tidier_name) {
  if (!inherits(x, "data.frame")) {
    stop(
      "Tidier `",
      tidier_name,
      "` must return a data frame or tibble.",
      call. = FALSE
    )
  }

  tibble::as_tibble(x)
}
