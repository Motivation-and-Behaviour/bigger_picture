#' How many rows of each unit have an exposure together with an outcome
#'
#' The count behind the feasibility report (ANALYSIS_PLAN.md, Step 7 task
#' 7): for every analysed exposure and every analysed outcome row a unit
#' holds, the rows and participants that have both. It fits no model and
#' shows no association, so it runs on every cell before the freeze.
#'
#' Counts are of the prepared rows, after the exclusions and derivations of
#' the preparation step, and before the row rules of stage 1 (weights, age
#' and sex), so a cell can be countable here and still not fitted.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @param settings From `analysis_settings()`.
#' @return A list of two tibbles, or NULL when the dataset is not included.
#'   `cells`: one row per unit, exposure and outcome row with at least one
#'   row having both, with `n_rows` and `n_participants`. `units`: one row
#'   per unit, with its rows and the covariate measures each further tier
#'   would use (`ses_measure`, `ethnicity_measure`, `urbanicity_measure`; NA
#'   where the unit records none well enough).
count_cells <- function(
  prepared,
  outcome_registry,
  exposure_registry,
  settings = analysis_settings()
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  base <- prepared$base
  analysed <- outcome_registry$rows$variable_name[
    outcome_registry$rows$use %in% c("core", "report")
  ]
  outcomes <- intersect(names(prepared$outcomes), analysed)
  exposures <- intersect(
    names(prepared$exposures),
    c(
      exposure_registry$variable_name[exposure_registry$include %in% TRUE],
      "total_screen"
    )
  )
  has <- function(table, columns) {
    out <- !is.na(as.matrix(table[columns]))
    storage.mode(out) <- "numeric"
    out
  }
  x <- has(prepared$exposures, exposures)
  y <- has(prepared$outcomes, outcomes)
  participant <- as.character(base$participant_id)
  unit_rows <- split(seq_len(nrow(base)), base$unit)

  cells <- lapply(names(unit_rows), function(unit) {
    index <- unit_rows[[unit]]
    rows <- crossprod(x[index, , drop = FALSE], y[index, , drop = FALSE])
    found <- which(rows > 0, arr.ind = TRUE)
    if (nrow(found) == 0) {
      return(NULL)
    }
    # One row per participant: any of their rows with both.
    first <- !duplicated(participant[index])
    repeats <- !all(first)
    participants <- if (repeats) {
      by_person <- function(matrix) {
        rowsum(matrix[index, , drop = FALSE], participant[index]) > 0
      }
      crossprod(by_person(x) * 1, by_person(y) * 1)
    } else {
      rows
    }
    tibble::tibble(
      unit = unit,
      exposure = exposures[found[, 1]],
      outcome = outcomes[found[, 2]],
      n_rows = as.integer(rows[found]),
      n_participants = as.integer(participants[found])
    )
  })
  cells <- dplyr::bind_rows(cells)

  column <- function(name) {
    if (name %in% names(base)) as.character(base[[name]]) else NULL
  }
  first_recorded <- function(index, candidates) {
    for (candidate in candidates) {
      values <- column(candidate)
      if (!is.null(stage1_covariate_groups(values[index], settings))) {
        return(candidate)
      }
    }
    NA_character_
  }
  units <- dplyr::bind_rows(lapply(names(unit_rows), function(unit) {
    index <- unit_rows[[unit]]
    tibble::tibble(
      unit = unit,
      rows = length(index),
      ses_measure = first_recorded(
        index,
        c("ses_household", "parental_education", "ses_area")
      ),
      ethnicity_measure = first_recorded(index, c("ethnicity", "minority")),
      urbanicity_measure = first_recorded(index, "urbanicity")
    )
  }))
  dataset_id <- as.character(base$dataset_id[1])
  cells$dataset_id <- rep(dataset_id, nrow(cells))
  units$dataset_id <- rep(dataset_id, nrow(units))
  list(
    cells = dplyr::relocate(cells, "dataset_id"),
    units = dplyr::relocate(units, "dataset_id")
  )
}
