#' The scale of the harmonisation work, counted from the repository
#'
#' For the report's account of what the project built (ANALYSIS_PLAN.md,
#' Step 6). Every number is counted from tracked files or pipeline targets,
#' never typed.
#'
#' @param resource_counts One row per dataset, role and reader, with `files`.
#' @param mapping_counts One row per dataset and harmonisation status, with
#'   `rows` and `source_columns` (distinct source columns the mapped rows
#'   name).
#' @param dataschema The dataschema.
#' @param manifest The data manifest.
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
  tidier_dir = "R/tidy",
  lookup_dirs = c(bp_shared_lookup_dir(), bp_dataset_specs_dir())
) {
  included <- resource_counts[
    resource_counts$dataset_id %in% manifest$dataset_id,
  ]
  files <- dplyr::summarise(
    included,
    datasets = dplyr::n_distinct(.data$dataset_id),
    files = sum(.data$files),
    .by = c("role", "reader")
  )
  files <- files[order(files$role, -files$files), ]

  mapped <- mapping_counts[mapping_counts$dataset_id %in% manifest$dataset_id, ]
  mapping <- dplyr::summarise(
    mapped,
    rows = sum(.data$rows),
    datasets = dplyr::n_distinct(.data$dataset_id),
    .by = "status"
  )
  evaluated <- c("compatible", "partial", "proximate", "inferred")
  schema <- dplyr::count(dataschema, .data$domain, name = "rows")

  lookups <- unlist(lapply(lookup_dirs, function(dir) {
    all <- list.files(
      dir,
      pattern = "\\.csv$",
      recursive = TRUE,
      full.names = TRUE
    )
    all[basename(dirname(all)) == "lookups"]
  }))
  totals <- tibble::tibble(
    item = c(
      "Datasets included",
      "Participants",
      "Observations",
      "Raw data files read",
      "Codebooks and documents consulted",
      "Dataschema variables",
      "Mapping decisions recorded",
      "Variables mapped with data",
      "Source columns drawn on",
      "Study-specific tidying scripts",
      "Lookup tables"
    ),
    count = c(
      nrow(manifest),
      sum(manifest$participants),
      sum(manifest$primary_rows),
      sum(included$files[included$role %in% "data"]),
      sum(included$files[included$role %in% c("codebook", "docs")]),
      nrow(dataschema),
      sum(mapped$rows),
      sum(mapped$rows[mapped$status %in% evaluated]),
      sum(mapped$source_columns),
      length(list.files(tidier_dir, pattern = "^tidy_BPIPD_.*\\.R$")),
      length(lookups)
    )
  )
  list(files = files, schema = schema, mapping = mapping, totals = totals)
}
