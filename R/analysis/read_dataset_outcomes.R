#' Read the outcome of every dataset the project sought
#'
#' The top of the dataset flow, which the repository cannot count for itself:
#' how many of the datasets identified ended in each outcome. The counts are
#' typed into `analysis/dataset_outcomes.csv` by hand, one line per outcome,
#' and can be left blank until they are known. `included` is checked against
#' the pipeline when it is filled in.
#'
#' @param path Path to `dataset_outcomes.csv`.
#' @return A tibble of `outcome`, `label`, `datasets` (NA where blank) and
#'   `definition`, in the order of `bp_dataset_outcomes()`.
read_dataset_outcomes <- function(path = bp_dataset_outcomes_path()) {
  outcomes <- read_registry_csv(path, c("outcome", "datasets", "definition"))
  known <- bp_dataset_outcomes()
  fail <- function(...) stop(basename(path), ": ", ..., call. = FALSE)
  if (!setequal(outcomes$outcome, names(known))) {
    fail(
      "needs exactly one line for each of: ",
      paste(names(known), collapse = ", ")
    )
  }
  counts <- suppressWarnings(as.numeric(outcomes$datasets))
  bad <- !is.na(outcomes$datasets) &
    (is.na(counts) | counts < 0 | counts %% 1 != 0)
  if (any(bad)) {
    fail("`datasets` must be a whole number or blank")
  }
  outcomes$datasets <- counts
  outcomes <- outcomes[match(names(known), outcomes$outcome), ]
  outcomes$label <- unname(known)
  outcomes[c("outcome", "label", "datasets", "definition")]
}

# The outcomes a sought dataset can end in, in reporting order, with the
# labels the project tracks them under.
bp_dataset_outcomes <- function() {
  c(
    included = "Included",
    agreed_awaiting_data = "Agreed and awaiting data",
    contacting_authors = "Contacting authors",
    declined = "Declined",
    non_response = "Non-response",
    cannot_access = "Cannot access"
  )
}
