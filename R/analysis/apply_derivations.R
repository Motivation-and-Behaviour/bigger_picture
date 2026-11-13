#' Compute outcome rows a dataset records only in parts (DR-0008)
#'
#' Applies the rules in `analysis/derivations.csv`. A rule runs only when the
#' dataset has no value on the target and has values on every source; a rule
#' naming the dataset takes precedence over a general one. A row gets a
#' derived value only when all of its sources are present.
#'
#' @param scores A tibble of scored outcome columns.
#' @param derivations The table from `read_derivations()`.
#' @param dataset_id The dataset's ID.
#' @return A list: `scores` with any derived columns filled, and `derived`, a
#'   tibble of `target`, `sources` and `method` for the rules that ran.
apply_derivations <- function(scores, derivations, dataset_id) {
  dataset_id <- as.character(dataset_id)
  applies <- is.na(derivations$dataset_id) |
    derivations$dataset_id %in% dataset_id
  rules <- derivations[applies, ]
  # A dataset's own rule first.
  rules <- rules[order(is.na(rules$dataset_id)), ]
  rules <- rules[!duplicated(rules$target), ]
  has_values <- function(variable) {
    variable %in% names(scores) && any(!is.na(scores[[variable]]))
  }

  ran <- list()
  for (i in seq_len(nrow(rules))) {
    target <- rules$target[i]
    sources <- strsplit(rules$sources[i], "|", fixed = TRUE)[[1]]
    if (has_values(target) || !all(vapply(sources, has_values, logical(1)))) {
      next
    }
    parts <- as.matrix(scores[sources])
    scores[[target]] <- switch(
      rules$method[i],
      sum = rowSums(parts),
      mean = rowMeans(parts),
      weighted_mean = {
        weights <- as.numeric(strsplit(rules$weights[i], "|", fixed = TRUE)[[
          1
        ]])
        as.numeric(parts %*% weights) / sum(weights)
      }
    )
    ran[[target]] <- tibble::tibble(
      target = target,
      sources = rules$sources[i],
      method = rules$method[i]
    )
  }
  derived <- dplyr::bind_rows(
    tibble::tibble(
      target = character(),
      sources = character(),
      method = character()
    ),
    dplyr::bind_rows(ran)
  )
  list(scores = scores, derived = derived)
}
