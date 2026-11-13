#' The scale of the harmonisation work, counted from the repository
#'
#' For the report's account of what the project built (ANALYSIS_PLAN.md,
#' Step 6). Every number is counted from tracked files or pipeline targets,
#' never typed.
#'
#' @param resource_counts One row per dataset, role and reader, with `files`.
#' @param mapping_counts One row per dataset and harmonisation status, from
#'   `count_mapping()`.
#' @param dataschema The dataschema.
#' @param manifest The data manifest.
#' @param prepared_summary The combined `summarise_prepared()` table.
#' @param tidier_dir Folder of tidiers.
#' @param lookup_dirs Folders holding lookup tables.
#' @return A named list of tibbles: `files` (raw files read, by format, and
#'   documents consulted), `schema` (dataschema rows by domain), `mapping`
#'   (mapped rows by status) and `totals` (single counts).
summarise_work_scale <- function(
  resource_counts,
  mapping_counts,
  dataschema,
  manifest,
  prepared_summary,
  tidier_dir = "R/tidy",
  lookup_dirs = c(bp_shared_lookup_dir(), bp_dataset_specs_dir())
) {
  ids <- manifest$dataset_id
  included <- resource_counts[resource_counts$dataset_id %in% ids, ]
  files <- dplyr::summarise(
    included,
    datasets = dplyr::n_distinct(.data$dataset_id),
    files = sum(.data$files),
    .by = c("role", "reader")
  )
  files <- files[order(files$role, -files$files), ]

  mapped <- mapping_counts[mapping_counts$dataset_id %in% ids, ]
  mapping <- dplyr::summarise(
    mapped,
    rows = sum(.data$rows),
    datasets = dplyr::n_distinct(.data$dataset_id),
    .by = "status"
  )
  columns <- dplyr::distinct(mapped, .data$dataset_id, .data$tidied_columns)
  schema <- dplyr::count(dataschema, .data$domain, name = "rows")

  tidiers <- list.files(tidier_dir, pattern = "^tidy_BPIPD_[0-9]+\\.R$")
  tidier_ids <- sub("^tidy_BPIPD_([0-9]+)\\.R$", "\\1", tidiers)
  lookups <- unlist(lapply(lookup_dirs, function(dir) {
    all <- list.files(
      dir,
      pattern = "\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
    all[basename(dirname(all)) == "lookups"]
  }))
  # A dataset's own lookups count only when the dataset is included.
  owner <- sub("^BPIPD-", "", basename(dirname(dirname(lookups))))
  shared <- !grepl("^[0-9]+$", owner)
  lookups <- lookups[shared | owner %in% ids]

  totals <- tibble::tibble(
    item = c(
      "Datasets included",
      "Participants",
      "Observations (a participant at one wave)",
      "Observations after the age and trial rules",
      "Raw data files read",
      "Codebooks and other documents supplied with the data",
      "Variables in the shared schema",
      "Mapping decisions recorded (a variable in a dataset)",
      "Of those, mapped with data",
      "Columns of the tidied data those mappings draw on",
      "Study-specific tidying scripts",
      "Lookup tables"
    ),
    count = c(
      nrow(manifest),
      sum(manifest$participants),
      sum(manifest$primary_rows),
      sum(prepared_summary$rows_kept),
      sum(included$files[included$role %in% "data"]),
      sum(included$files[included$role %in% c("codebook", "docs")]),
      nrow(dataschema),
      sum(mapped$rows),
      sum(mapped$rows[mapped$status %in% bp_evaluated_statuses()]),
      sum(columns$tidied_columns),
      sum(tidier_ids %in% ids),
      length(lookups)
    )
  )
  list(files = files, schema = schema, mapping = mapping, totals = totals)
}
