#' The recorded manifest a build is compared with, if there is one
#'
#' The report's datasets are fixed by recording the data manifest under
#' the report's label (`record_data_manifest()`).
#' From then on the pipeline compares the live manifest with it
#' (`compare_data_manifest()`).
#'
#' @param label The manifest's label.
#' @param dir Folder for recorded manifests.
#' @param has_manifest Not used: the pipeline passes whether the file
#'   existed when it was defined, so that the target reruns once it does.
#' @return The file's path, or a zero-length path when none has been
#'   recorded.
find_recorded_manifest <- function(
  label = bp_report_manifest_label(),
  dir = bp_manifest_dir(),
  has_manifest = NULL
) {
  path <- fs::path(dir, paste0(label, ".csv"))
  if (file.exists(path)) as.character(path) else character()
}
