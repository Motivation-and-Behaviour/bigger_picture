#' Row-bind one named element from a set of per-dataset lists
#'
#' @param parts A list of per-dataset lists, such as `summarise_dataset()`
#'   returns.
#' @param name The element to bind.
#' @return A tibble; elements that are NULL are skipped.
bind_part <- function(parts, name) {
  dplyr::bind_rows(lapply(parts, function(part) part[[name]]))
}
