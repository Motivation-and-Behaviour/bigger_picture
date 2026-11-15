#' Tiered false discovery rate: q-values that carry the tests above them
#'
#' The preregistered procedure (DR-0017). Tests are arranged in layers, each
#' layer leading into the next: the five outcome families, then the content
#' classes within a family, then the specific outcomes within a class, and
#' so on. A test's layer and everything above it is its `path`, written as
#' the layers' names joined by `" > "`, for example
#' `"families > classes:wellbeing > specific:wellbeing:st_watch"`.
#'
#' Tests that share a path are one group. Within a group the q-values are
#' Benjamini and Hochberg's, computed as `p.adjust(p, "BH", n = K)`: the
#' ranks come from the group's own p-values, and `K` is the number of tests
#' in the group plus the number in every layer on the path above it. So a
#' deeper test is penalised for the testing that led to it, and a small
#' p-value higher up cannot lower a q-value further down.
#'
#' The first layer of every path has a fixed count (`fixed`), because the
#' five families are tested whether or not each can be estimated. A layer
#' that holds no test adds nothing.
#'
#' Lower-layer q-values are a descriptive penalty, not a guaranteed error
#' rate.
#'
#' @param p P-values; NA for a test that could not be run, which gets no
#'   q-value and is not counted.
#' @param path The path of each test.
#' @param fixed Named counts for layers whose size is set in advance.
#' @return A tibble, one row per element of `p`: `q` and `k_total`, the `K`
#'   used.
adjust_tiered_fdr <- function(p, path, fixed = c(families = 5L)) {
  if (length(p) != length(path)) {
    stop("adjust_tiered_fdr(): `p` and `path` differ in length.", call. = FALSE)
  }
  if (anyNA(path)) {
    stop("adjust_tiered_fdr(): every test needs a path.", call. = FALSE)
  }
  bad <- !is.na(p) & (p < 0 | p > 1)
  if (any(bad)) {
    stop("adjust_tiered_fdr(): p-values must lie in [0, 1].", call. = FALSE)
  }
  tested <- !is.na(p)
  counts <- table(path[tested])
  layer_count <- function(layer) {
    if (layer %in% names(fixed)) {
      observed <- if (layer %in% names(counts)) counts[[layer]] else 0L
      if (observed > fixed[[layer]]) {
        stop(
          "adjust_tiered_fdr(): layer `",
          layer,
          "` holds more tests than its fixed count.",
          call. = FALSE
        )
      }
      return(as.integer(fixed[[layer]]))
    }
    if (layer %in% names(counts)) as.integer(counts[[layer]]) else 0L
  }
  # A path's K: its own layer and each layer above it.
  path_total <- function(one) {
    parts <- strsplit(one, " > ", fixed = TRUE)[[1]]
    prefixes <- vapply(
      seq_along(parts),
      function(i) paste(parts[seq_len(i)], collapse = " > "),
      character(1)
    )
    sum(vapply(prefixes, layer_count, integer(1)))
  }
  paths <- unique(path)
  totals <- stats::setNames(vapply(paths, path_total, numeric(1)), paths)

  q <- rep(NA_real_, length(p))
  for (one in paths) {
    index <- which(path == one & tested)
    if (length(index) > 0) {
      q[index] <- stats::p.adjust(p[index], method = "BH", n = totals[[one]])
    }
  }
  tibble::tibble(
    q = q,
    k_total = ifelse(tested, as.integer(totals[path]), NA_integer_)
  )
}
