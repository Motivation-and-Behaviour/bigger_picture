#' Turn the model list into the exposure and outcome cells each model fits
#'
#' A model names sets, and the sets are read from the registries, so a new
#' schema row joins a model with no edit to the list:
#'
#' - exposures: `total` is the derived total; `content_classes` the
#'   usual-day activity rows of the prereg's content classes that are
#'   included and in scope for the report; `social_media` that class
#'   alone; `other_included` the other
#'   included usual-day rows, bar the dedicated total; `school` the rows for
#'   use at school;
#' - outcomes: `core` the outcome rows pooled up the tree, and `core_h5b`,
#'   `core_h5c` and `core_other_h5c` those of the families the registered
#'   moderation hypotheses name, or do not; `report` the
#'   outcome rows reported on their own; `negative_control` the rows of that
#'   role. `core_rare_binary` and `headline_cells` depend on the data and
#'   are settled where their models are built, so they expand to nothing
#'   here.
#'
#' Until the specification is frozen only the development cells are kept
#' (ANALYSIS_PLAN.md, Step 5): a model's cells are then the development
#' cells that fall in its sets.
#'
#' @param model_spec From `read_model_spec()`.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @param development_cells From `read_development_cells()`.
#' @param frozen Whether the specification is frozen.
#' @return A tibble of `model_name`, `exposure` and `outcome`.
expand_model_cells <- function(
  model_spec,
  outcome_registry,
  exposure_registry,
  development_cells,
  frozen = FALSE
) {
  usual <- exposure_registry[
    exposure_registry$day %in% "usual" & exposure_registry$unit %in% "hours",
  ]
  included <- usual[usual$include %in% TRUE & usual$context %in% "any", ]
  classes <- included$variable_name[
    included$headline %in%
      TRUE &
      included$axis %in% "activity" &
      !is.na(included$content_class)
  ]
  exposures <- list(
    total = bp_total_exposure(),
    content_classes = classes,
    social_media = included$variable_name[
      included$content_class %in% "social_media" & included$axis %in% "activity"
    ],
    other_included = setdiff(
      included$variable_name,
      c(classes, included$variable_name[included$exposure %in% "total"])
    ),
    school = exposure_registry$variable_name[
      exposure_registry$context %in% "school"
    ]
  )
  rows <- outcome_registry$rows
  is_outcome <- rows$role %in% "outcome"
  pooled <- is_outcome & rows$use %in% "core" & rows$synthesis %in% TRUE
  in_family <- function(families) {
    rows$variable_name[pooled & rows$prereg_family %in% families]
  }
  h5c <- c("learning", "mental_health", "behaviour")
  outcomes <- list(
    core = rows$variable_name[pooled],
    # The families the registered moderation hypotheses name.
    core_h5b = in_family(c("mental_health", "wellbeing")),
    core_h5c = in_family(h5c),
    core_other_h5c = rows$variable_name[
      pooled & !rows$prereg_family %in% h5c
    ],
    report = rows$variable_name[
      is_outcome & rows$use %in% c("core", "report") & !pooled
    ],
    negative_control = rows$variable_name[rows$role %in% "negative_control"],
    core_rare_binary = character(),
    headline_cells = character()
  )

  cells <- lapply(seq_len(nrow(model_spec)), function(i) {
    out <- tidyr::expand_grid(
      exposure = unique(unlist(exposures[model_spec$exposure_set[[i]]])),
      outcome = outcomes[[model_spec$outcome_set[i]]]
    )
    out$model_name <- rep(model_spec$model_name[i], nrow(out))
    out
  })
  cells <- dplyr::bind_rows(cells)
  if (!frozen) {
    cells <- dplyr::semi_join(
      cells,
      development_cells,
      by = c("exposure", "outcome")
    )
  }
  dplyr::relocate(cells, "model_name")
}
