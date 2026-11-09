#' Render targets for the private funder report, when it is present
#'
#' The report's sources live in a private repository, cloned (or linked) into
#' the project as `DoEreport/`. Its `targets.R` returns the list of targets that
#' render it. Without it the pipeline simply has no report, so public clones and
#' CI build everything else unchanged.
#'
#' @param dir Folder holding the private report project.
#' @return A list of targets, empty when the report is absent.
private_report_targets <- function(dir = bp_private_report_dir()) {
  entry <- file.path(dir, "targets.R")
  if (file.exists(entry)) {
    # Global env, as tar_source() uses, so the targets' commands resolve.
    return(source(entry)$value)
  }
  # Sys.readlink() is "" for a real folder and NA for a missing path.
  link <- Sys.readlink(dir)
  if (dir.exists(dir) || (!is.na(link) && nzchar(link))) {
    warning(
      dir,
      " has no targets.R (or is a broken link); building without the report.",
      call. = FALSE
    )
  }
  list()
}
