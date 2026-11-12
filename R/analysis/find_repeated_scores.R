#' Find outcome rows that repeat another row's score within a wave
#'
#' The harmonisation guide repeats some instrument-specific rows onto their
#' generic row, so the same score can sit in two columns. Two rows are a
#' repeat when every co-observed value is equal, on at least `min_rows` rows
#' that are not all one value. Different instruments measured on one occasion
#' are not repeats and both stay.
#'
#' Of a repeated pair the generic row is `keep`: the row named for its
#' informant (`_self`, `_parent`, `_teacher`, `_task`) and not for an
#' instrument; between two such rows, the one with the shorter name. `drop`
#' is TRUE only when both rows sit in the same node of the outcome tree,
#' where two copies would count one score twice. A repeat across nodes, or
#' between rows with no node, is reported and left alone: one item may
#' rightly serve two outcomes.
#'
#' @param scores A tibble of scored outcome columns.
#' @param wave The wave of each row.
#' @param nodes A named character vector: each row's node, NA for none.
#' @param min_rows Fewest co-observed rows for a comparison.
#' @return A tibble of `wave`, `specific`, `keep` and `drop`, one row per
#'   repeat found.
find_repeated_scores <- function(
  scores,
  wave,
  nodes = character(),
  min_rows = analysis_settings()$repeat_min_rows
) {
  empty <- tibble::tibble(
    wave = character(),
    specific = character(),
    keep = character(),
    drop = logical()
  )
  if (ncol(scores) < 2 || nrow(scores) == 0) {
    return(empty)
  }
  generic_first <- function(a, b) {
    pair <- c(a, b)
    by_informant <- grepl("_(self|parent|teacher|task)$", pair)
    tokens <- lengths(strsplit(pair, "_", fixed = TRUE))
    pair[order(!by_informant, tokens, pair)]
  }
  node_of <- function(variable) {
    if (variable %in% names(nodes)) nodes[[variable]] else NA_character_
  }

  wave_key <- dplyr::coalesce(as.character(wave), "")
  found <- list()
  for (key in unique(wave_key)) {
    index <- which(wave_key == key)
    block <- scores[index, , drop = FALSE]
    present <- names(block)[vapply(block, function(x) any(!is.na(x)), TRUE)]
    dropped <- character()
    for (i in seq_along(present)) {
      for (j in seq_along(present)) {
        a <- present[i]
        b <- present[j]
        if (j <= i || a %in% dropped || b %in% dropped) {
          next
        }
        both <- !is.na(block[[a]]) & !is.na(block[[b]])
        if (sum(both) < min_rows) {
          next
        }
        x <- block[[a]][both]
        if (!all(x == block[[b]][both]) || all(x == x[1])) {
          next
        }
        pair <- generic_first(a, b)
        same_node <- !is.na(node_of(a)) && identical(node_of(a), node_of(b))
        if (same_node) {
          dropped <- c(dropped, pair[2])
        }
        found[[length(found) + 1]] <- tibble::tibble(
          wave = wave[index[1]],
          specific = pair[2],
          keep = pair[1],
          drop = same_node
        )
      }
    }
  }
  dplyr::bind_rows(empty, dplyr::bind_rows(found))
}
