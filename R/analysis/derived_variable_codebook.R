#' Codebook of the variables the preparation step derives
#'
#' One row per derived variable, with the rule that produces it. It later
#' becomes part of the public codebook (ANALYSIS_PLAN.md, Step 4).
#'
#' @param derivations The derivation rules.
#' @param settings From `analysis_settings()`.
#' @return A tibble of `variable`, `table`, `definition` and `source`.
derived_variable_codebook <- function(
  derivations,
  settings = analysis_settings()
) {
  fixed <- tibble::tribble(
    ~variable                , ~table       , ~definition                                                                                                                                                                                                                                                                                                                                       , ~source   ,
    "unit"                   , "base"       , "The stage 1 unit: dataset, wave and country."                                                                                                                                                                                                                                                                                                    , "DR-0009" ,
    "wave_order"             , "base"       , "Rank of the wave in time within its dataset: by median collection year, then median age, then label."                                                                                                                                                                                                                                            , "Step 4"  ,
    "is_baseline"            , "base"       , "The row's wave is the dataset's baseline wave in analysis/datasets.csv."                                                                                                                                                                                                                                                                         , "Step 4"  ,
    "school_stage"           , "base"       , paste0("NSW school stage from age, with lower bounds at ", paste(settings$school_stage_breaks[2:5], collapse = ", "), " years.")                                                                                                                                                                                                                  , "Step 4"  ,
    "survey_weight_norm"     , "base"       , "The released survey weight divided by its mean within the unit; zero weights stay zero."                                                                                                                                                                                                                                                         , "Step 4"  ,
    "total_screen"           , "exposures"  , paste0("Total screen time in hours a day: the dedicated item where the wave has one, otherwise the sum of the wave's usual-day activity rows for rows with all of them, needing ", paste(settings$total_required, collapse = " and "), " at minimum; sums above ", settings$exposure_max_hours, " are set to ", settings$exposure_max_hours, ".") , "DR-0013" ,
    "total_source"           , "exposures"  , "How total_screen was built in the row's wave: dedicated or summed."                                                                                                                                                                                                                                                                              , "DR-0013" ,
    "total_rows"             , "exposures"  , "The dataschema rows that make up total_screen in the row's wave."                                                                                                                                                                                                                                                                                , "DR-0013" ,
    "total_coverage"         , "exposures"  , "The number of activity rows summed; missing for a dedicated item."                                                                                                                                                                                                                                                                               , "DR-0013" ,
    "total_capped"           , "exposures"  , paste0("The summed total exceeded ", settings$exposure_max_hours, " hours and was set to it.")                                                                                                                                                                                                                                                    , "DR-0013" ,
    "other_<exposure>"       , "exposures"  , "The sum of the wave's other usual-day activity rows, leaving this exposure out, for models that adjust one type of screen use for the rest."                                                                                                                                                                                                     , "DR-0013" ,
    "<outcome> (outcomes_z)" , "outcomes_z" , "A continuous outcome's score standardised within its standardise_by groups, by default wave and country within the dataset."                                                                                                                                                                                                                     , "DR-0009"
  )
  rules <- tibble::tibble(
    variable = derivations$target,
    table = "outcomes",
    definition = paste0(
      derivations$method,
      " of ",
      gsub("|", ", ", derivations$sources, fixed = TRUE),
      ifelse(
        is.na(derivations$weights),
        "",
        paste0(
          " (weights ",
          gsub("|", ", ", derivations$weights, fixed = TRUE),
          ")"
        )
      ),
      ifelse(
        is.na(derivations$dataset_id),
        "",
        paste0(", in dataset ", derivations$dataset_id)
      ),
      ", where the dataset records the parts and not the whole."
    ),
    source = "DR-0008"
  )
  dplyr::bind_rows(fixed, rules)
}
