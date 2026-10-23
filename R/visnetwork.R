#' `tar_visnetwork()` with each study's root files placed beside their consumers
#'
#' `tidier_file_<key>` and `harmonisation_files_<key>` have no upstream
#' targets, so vis.js puts them in the root column with long edges across the
#' graph. This pins every node to its longest-path depth, then moves each of
#' those files (and `harmonisation_config_<key>`) to the column just before the
#' target that consumes it. Viewing aid only: the pipeline never calls it.
bp_visnetwork <- function(targets_only = TRUE, ...) {
  widget <- targets::tar_visnetwork(targets_only = targets_only, ...)
  nodes <- widget$x$nodes
  edges <- widget$x$edges
  if (nrow(nodes) == 0) {
    return(widget)
  }

  level <- stats::setNames(rep(0, nrow(nodes)), nodes$id)
  repeat {
    new <- level
    for (i in seq_len(nrow(edges))) {
      new[[edges$to[[i]]]] <- max(
        new[[edges$to[[i]]]],
        new[[edges$from[[i]]]] + 1
      )
    }
    if (identical(new, level)) {
      break
    }
    level <- new
  }

  # Each prefix moves to the column before its consumer; consumers that are
  # themselves moved come first.
  consumers <- c(
    harmonisation_config_ = "harmonised_",
    harmonisation_files_ = "harmonisation_config_",
    tidier_file_ = "tidied_"
  )
  for (prefix in names(consumers)) {
    moved <- grep(paste0("^", prefix), names(level), value = TRUE)
    consumer <- sub(paste0("^", prefix), consumers[[prefix]], moved)
    keep <- consumer %in% names(level)
    level[moved[keep]] <- level[consumer[keep]] - 1
  }

  widget$x$nodes$level <- unname(level[nodes$id])
  widget
}
