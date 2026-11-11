#' Registry defaults for dataschema rows entering the outcome registry
#'
#' Fills only what the dataschema settles mechanically: the schema domain, a
#' `measure` role for instrument-description rows, the score type, the
#' informant where the row name states it, and the default standardisation
#' keys for continuous rows. Everything that needs judgement (node, direction,
#' synthesis, `use`, family, parts) is left blank for review, and the row
#' waits under a placeholder node.
#'
#' @param schema_rows Rows of the dataschema.
#' @param node Placeholder node for the new rows: `unassigned` before the
#'   freeze, `post_freeze` after it.
#' @return A tibble in `outcome_rows.csv` column order.
outcome_row_defaults <- function(schema_rows, node = "unassigned") {
  name <- as.character(schema_rows$variable_name)
  is_measure <- grepl("_measure$", name)

  allowed <- as.character(schema_rows$allowed_values)
  allowed[is.na(allowed)] <- ""
  levels_n <- lengths(strsplit(allowed, "|", fixed = TRUE))
  score_type <- dplyr::case_when(
    is_measure ~ NA_character_,
    schema_rows$data_type == "double" ~ "continuous",
    grepl("_diagnosed$", name) ~ "diagnosis",
    levels_n == 2 ~ "binary",
    levels_n >= 3 ~ "ordinal",
    .default = NA_character_
  )

  # Only what the name states; other informants are set at review.
  task_pattern <- paste0(
    "_task$|_nihtb|_nepsy$|_bas_|_htks$|_stdtest$|_digitspan$|",
    "_iq_fullscale$|_bpvs$|_ppvt$|_bracken$"
  )
  informant <- dplyr::case_when(
    is_measure ~ NA_character_,
    grepl("_self$|_selfrated$", name) ~ "self",
    grepl("_parent$|_parentrated$", name) ~ "parent",
    grepl("_teacher$|_teacherrated$", name) ~ "teacher",
    grepl(task_pattern, name) ~ "task",
    .default = NA_character_
  )

  # Typed vectors, not ifelse(), so zero rows still bind to real ones.
  node_col <- rep(node, length(name))
  node_col[is_measure] <- NA_character_
  role <- rep("outcome", length(name))
  role[is_measure] <- "measure"
  standardise_by <- rep(NA_character_, length(name))
  standardise_by[score_type %in% "continuous"] <- "dataset|wave|country"

  tibble::tibble(
    variable_name = name,
    node = node_col,
    informant = informant,
    role = role,
    score_type = score_type,
    direction = NA_character_,
    standardise_by = standardise_by,
    part_of = NA_character_,
    synthesis = NA,
    use = NA_character_,
    prereg_family = NA_character_,
    schema_domain = as.character(schema_rows$domain),
    frozen = FALSE,
    notes = NA_character_
  )
}
