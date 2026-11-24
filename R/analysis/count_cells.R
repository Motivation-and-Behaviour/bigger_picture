#' How many rows of each unit have an exposure together with an outcome
#'
#' The count behind the feasibility report: for every exposure and every
#' outcome row a unit holds, the rows and participants that have both, and
#' whether stage 1 could use them. It fits no model and shows no
#' association, so it runs on every cell before the freeze.
#'
#' Counted exposures are the rows the registry includes, the school-context
#' rows (for the school against home question) and the derived total.
#' Counted outcome rows are the `core` and `report` rows and the
#' negative-control rows. A row is counted on the column its model uses:
#' the standardised score for a linear model where there is one.
#'
#' `not_usable` gives the first of stage 1's checks the cell fails that can
#' be known from counts: a score type stage 1 does not model, fewer rows
#' than the minimum, a binary outcome with fewer than ten rows in a class,
#' or an exposure that does not vary in the unit. Stage 1 also leaves out
#' rows by its weight, age and sex rules, which are not applied here, so a
#' usable cell can still lose rows or fail there.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @param dataschema The dataschema, for the level count of ordinal rows.
#' @param settings From `analysis_settings()`.
#' @return A list of two tibbles, or NULL when the dataset is not included.
#'   `cells`: one row per unit, exposure and outcome row with at least one
#'   row having both, with `n_rows`, `n_participants`, the `model` stage 1
#'   would fit and `not_usable`. `units`: one row per unit, with its rows
#'   and the covariate measures each further tier would use, judged on all
#'   of the unit's rows (`ses_measure`, `ethnicity_measure`,
#'   `urbanicity_measure`; NA where the unit records none well enough).
count_cells <- function(
  prepared,
  outcome_registry,
  exposure_registry,
  dataschema,
  settings = default_analysis_settings()
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  base <- prepared$base
  registry <- outcome_registry$rows
  analysed <- registry$variable_name[
    registry$use %in%
      c("core", "report") |
      registry$role %in% "negative_control"
  ]
  outcomes <- intersect(names(prepared$outcomes), analysed)
  exposures <- intersect(
    names(prepared$exposures),
    c(
      exposure_registry$variable_name[
        exposure_registry$include %in%
          TRUE |
          exposure_registry$context %in% "school"
      ],
      "total_screen"
    )
  )
  dataset_id <- as.character(base$dataset_id[1])
  unit_rows <- split(seq_len(nrow(base)), base$unit)
  model <- vapply(
    outcomes,
    function(outcome) {
      score_type <- registry$score_type[match(outcome, registry$variable_name)]
      stage1_model_type(score_type, schema_level_count(outcome, dataschema))
    },
    character(1)
  )
  values <- lapply(outcomes, function(outcome) {
    standardised <- model[[outcome]] == "linear" &&
      outcome %in% names(prepared$outcomes_z)
    if (standardised) {
      prepared$outcomes_z[[outcome]]
    } else {
      prepared$outcomes[[outcome]]
    }
  })
  as_matrix <- function(columns, test) {
    out <- vapply(
      columns,
      function(column) test(column) * 1,
      numeric(nrow(base))
    )
    matrix(out, nrow = nrow(base), dimnames = list(NULL, names(columns)))
  }
  present <- function(column) !is.na(column)
  x <- as_matrix(as.list(prepared$exposures[exposures]), present)
  y <- as_matrix(stats::setNames(values, outcomes), present)
  # The rows in the class coded 1, for the class sizes of binary rows.
  y_one <- as_matrix(stats::setNames(values, outcomes), function(column) {
    !is.na(column) & as.numeric(column) == 1
  })
  participant <- as.character(base$participant_id)

  cells <- lapply(names(unit_rows), function(unit) {
    index <- unit_rows[[unit]]
    x_unit <- x[index, , drop = FALSE]
    y_unit <- y[index, , drop = FALSE]
    rows <- crossprod(x_unit, y_unit)
    found <- which(rows > 0, arr.ind = TRUE)
    if (nrow(found) == 0) {
      return(NULL)
    }
    ones <- crossprod(x_unit, y_one[index, , drop = FALSE])
    # A participant counts once, for any of their rows that has both.
    people <- participant[index]
    participants <- if (anyDuplicated(people) > 0) {
      counts <- vapply(
        seq_len(ncol(x_unit)),
        function(e) colSums(rowsum(x_unit[, e] * y_unit, people) > 0),
        numeric(ncol(y_unit))
      )
      t(matrix(counts, nrow = ncol(y_unit)))
    } else {
      rows
    }
    varies <- vapply(
      exposures,
      function(exposure) {
        isTRUE(
          stats::sd(prepared$exposures[[exposure]][index], na.rm = TRUE) > 0
        )
      },
      logical(1)
    )
    n_rows <- as.integer(rows[found])
    cell_model <- unname(model[found[, 2]])
    smaller <- pmin(ones[found], rows[found] - ones[found])
    tibble::tibble(
      unit = unit,
      exposure = exposures[found[, 1]],
      outcome = outcomes[found[, 2]],
      n_rows = n_rows,
      n_participants = as.integer(participants[found]),
      model = cell_model,
      not_usable = dplyr::case_when(
        cell_model == "none" ~ "score type not modelled here",
        n_rows < settings$min_unit_rows ~
          "fewer rows than the minimum unit size",
        cell_model == "logistic" & smaller < 10 ~
          "fewer than 10 rows in one class",
        !varies[found[, 1]] ~ "no variation in the exposure",
        .default = NA_character_
      )
    )
  })
  cells <- dplyr::bind_rows(cells)

  column <- function(name) {
    if (name %in% names(base)) as.character(base[[name]]) else NULL
  }
  first_recorded <- function(index, candidates) {
    for (candidate in candidates) {
      recorded <- column(candidate)
      if (!is.null(stage1_covariate_groups(recorded[index], settings))) {
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
  cells$dataset_id <- rep(dataset_id, nrow(cells))
  units$dataset_id <- rep(dataset_id, nrow(units))
  list(
    cells = dplyr::relocate(cells, "dataset_id"),
    units = dplyr::relocate(units, "dataset_id")
  )
}
