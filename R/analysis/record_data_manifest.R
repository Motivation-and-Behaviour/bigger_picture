#' Record the current data manifest under a label
#'
#' Run by hand for each delivered report and at the freeze, then commit the
#' file with that state of the code (ANALYSIS_PLAN.md, Step 2). The file holds
#' dataset IDs, counts and hashes only.
#'
#' @param label Name for the record, such as `doe-report`.
#' @param manifest The manifest; read from the pipeline's store by default.
#' @param dir Folder for recorded manifests.
#' @return The path written, invisibly.
record_data_manifest <- function(
  label,
  manifest = targets::tar_read_raw("data_manifest"),
  dir = bp_manifest_dir()
) {
  if (!grepl("^[a-z0-9][a-z0-9-]*$", label)) {
    stop(
      "`label` must be lower-case letters, digits and hyphens.",
      call. = FALSE
    )
  }
  if (nrow(manifest) == 0) {
    stop("The data manifest is empty; nothing to record.", call. = FALSE)
  }
  fs::dir_create(dir)
  path <- fs::path(dir, paste0(label, ".csv"))
  manifest$recorded <- format(Sys.Date(), "%Y-%m-%d")
  readr::write_csv(manifest, path)
  invisible(path)
}
