#' The outcome tables of one dataset, aligned with its base table
#'
#' Applies the outcome rules of the preparation step (ANALYSIS_PLAN.md,
#' Step 4), in this order:
#'
#' 1. score each analysed row (`score_outcome_values()`);
#' 2. apply the dataset's `exclude` exceptions;
#' 3. derive rows the dataset records only in parts (`apply_derivations()`);
#' 4. drop continuous values outside the dataschema's bounds, unless an
#'    exception keeps them (DR-0011);
#' 5. within each wave, drop repeats of another row's score, letter grades
#'    where numeric grades exist, and a part where its whole is in the same
#'    node;
#' 6. standardise continuous rows within their `standardise_by` groups.
#'
#' Rows the registry marks `archive`, and nominal rows, are not prepared;
#' the audit says so.
#'
#' @param rows The dataset's kept primary rows, in base-table order.
#' @param base The dataset's base table.
#' @param outcome_registry The outcome registry.
#' @param derivations The derivation rules.
#' @param dataschema The dataschema.
#' @param exceptions The dataset's analysis exceptions.
#' @param settings From `analysis_settings()`.
#' @return A list: `outcomes` (scores, one column per row used), `outcomes_z`
#'   (standardised scores for continuous rows), `audit` (one row per wave and
#'   outcome row with any value), `repeats` (the repeated scores found) and
#'   `derived` (the derivation rules that ran).
prepare_outcomes <- function(
  rows,
  base,
  outcome_registry,
  derivations,
  dataschema,
  exceptions,
  settings = analysis_settings()
) {
  registry <- outcome_registry$rows
  registry <- registry[registry$role %in% c("outcome", "negative_control"), ]
  registry <- dplyr::left_join(
    registry,
    dataschema[c("variable_name", "allowed_values", "min_value", "max_value")],
    by = "variable_name"
  )
  analysed <- registry$role %in% "negative_control" |
    registry$use %in% c("core", "report")
  scored_types <- c("continuous", "ordinal", "binary", "diagnosis")
  wave <- as.character(base$wave)
  n <- nrow(base)
  audit <- list()
  note <- function(variable, status, detail = NA_character_, waves = NULL) {
    tibble::tibble(
      variable_name = variable,
      wave = if (is.null(waves)) NA_character_ else waves,
      all_waves = is.null(waves),
      status = status,
      detail = detail
    )
  }
  has_values <- function(variable) {
    variable %in% names(rows) && any(!is.na(rows[[variable]]))
  }

  # 1. Score.
  scores <- tibble::tibble(.rows = n)
  for (i in seq_len(nrow(registry))) {
    variable <- registry$variable_name[i]
    if (!has_values(variable)) {
      next
    }
    if (!analysed[i]) {
      audit[[length(audit) + 1]] <- note(variable, "archive", "not analysed this round")
      next
    }
    if (!registry$score_type[i] %in% scored_types) {
      audit[[length(audit) + 1]] <- note(variable, "descriptive_only", "nominal rows are not modelled")
      next
    }
    scores[[variable]] <- score_outcome_values(
      rows[[variable]],
      registry$score_type[i],
      registry$allowed_values[i]
    )
  }
  observed <- lapply(scores, function(x) !is.na(x))

  # 2. Exclusions by exception.
  in_waves <- function(exception_wave) {
    if (is.na(exception_wave)) rep(TRUE, n) else wave %in% exception_wave
  }
  excluded <- exceptions[exceptions$action %in% "exclude", ]
  for (i in seq_len(nrow(excluded))) {
    variable <- excluded$variable_name[i]
    if (variable %in% names(scores)) {
      hit <- in_waves(excluded$wave[i])
      scores[[variable]][hit] <- NA_real_
      audit[[length(audit) + 1]] <- note(
        variable,
        "excluded",
        paste0("exception: ", excluded$reason[i]),
        if (is.na(excluded$wave[i])) NULL else excluded$wave[i]
      )
    }
  }

  # 3. Derivations.
  derived <- apply_derivations(scores, derivations, base$dataset_id[1])
  targets <- derived$derived$target
  targets <- targets[targets %in% registry$variable_name[analysed]]
  for (target in setdiff(names(derived$scores), c(names(scores), targets))) {
    derived$scores[[target]] <- NULL
  }
  scores <- derived$scores
  for (target in targets) {
    observed[[target]] <- !is.na(scores[[target]])
  }

  # 4. Bounds, for continuous rows.
  keep_bounds <- exceptions$variable_name[
    exceptions$action %in% "keep_out_of_bounds"
  ]
  out_of_bounds <- list()
  for (variable in names(scores)) {
    row <- registry[match(variable, registry$variable_name), ]
    if (!row$score_type %in% "continuous" || variable %in% keep_bounds) {
      next
    }
    low <- suppressWarnings(as.numeric(row$min_value))
    high <- suppressWarnings(as.numeric(row$max_value))
    values <- scores[[variable]]
    outside <- (!is.na(low) & !is.na(values) & values < low) |
      (!is.na(high) & !is.na(values) & values > high)
    if (any(outside)) {
      scores[[variable]][outside] <- NA_real_
      out_of_bounds[[variable]] <- outside
    }
  }

  # 5. One score per construct and occasion.
  wave_rows <- function(wave_label) {
    if (is.na(wave_label)) is.na(wave) else wave %in% wave_label
  }
  repeats <- find_repeated_scores(
    scores,
    wave,
    stats::setNames(registry$node, registry$variable_name),
    settings$repeat_min_rows
  )
  for (i in which(repeats$drop)) {
    scores[[repeats$specific[i]]][wave_rows(repeats$wave[i])] <- NA_real_
    audit[[length(audit) + 1]] <- note(
      repeats$specific[i],
      "dropped_repeat",
      paste0("same score as ", repeats$keep[i]),
      repeats$wave[i]
    )
  }
  wave_has <- function(variable, wave_label) {
    hit <- wave_rows(wave_label)
    variable %in% names(scores) && any(!is.na(scores[[variable]][hit]))
  }
  node_of <- function(variable) {
    registry$node[match(variable, registry$variable_name)]
  }
  for (wave_label in unique(wave)) {
    preferred <- bp_preferred_outcome_rows()
    for (i in seq_len(nrow(preferred))) {
      if (
        wave_has(preferred$keep[i], wave_label) &&
          wave_has(preferred$drop[i], wave_label)
      ) {
        scores[[preferred$drop[i]]][wave_rows(wave_label)] <- NA_real_
        audit[[length(audit) + 1]] <- note(
          preferred$drop[i],
          "dropped_preferred",
          paste0(preferred$keep[i], " is used where both exist"),
          wave_label
        )
      }
    }
    for (variable in names(scores)) {
      wholes <- registry$part_of[match(variable, registry$variable_name)]
      if (is.na(wholes) || !wave_has(variable, wave_label)) {
        next
      }
      for (whole in strsplit(wholes, "|", fixed = TRUE)[[1]]) {
        if (
          wave_has(whole, wave_label) &&
            identical(node_of(whole), node_of(variable))
        ) {
          scores[[variable]][wave_rows(wave_label)] <- NA_real_
          audit[[length(audit) + 1]] <- note(
            variable,
            "dropped_part",
            paste0("part of ", whole, ", in the same node"),
            wave_label
          )
          break
        }
      }
    }
  }

  # 6. Standardise continuous rows.
  keys_exception <- exceptions[exceptions$action %in% "standardise_by", ]
  outcomes_z <- tibble::tibble(.rows = n)
  for (variable in names(scores)) {
    row <- registry[match(variable, registry$variable_name), ]
    if (!row$score_type %in% "continuous") {
      next
    }
    keys <- row$standardise_by
    override <- keys_exception$value[keys_exception$variable_name %in% variable]
    if (length(override) > 0) {
      keys <- override[1]
    }
    if (is.na(keys)) {
      keys <- "dataset|wave|country"
    }
    outcomes_z[[variable]] <- standardise_within(
      scores[[variable]],
      standardise_groups(keys, base, rows)
    )
  }

  # Audit: one row per wave and outcome row that had a value.
  counts <- lapply(names(observed), function(variable) {
    used <- if (variable %in% names(scores)) {
      !is.na(scores[[variable]])
    } else {
      rep(FALSE, n)
    }
    bounds <- out_of_bounds[[variable]]
    if (is.null(bounds)) {
      bounds <- rep(FALSE, n)
    }
    out <- dplyr::summarise(
      tibble::tibble(
        wave = wave,
        observed = observed[[variable]],
        used = used,
        bounds = bounds
      ),
      n_values = sum(.data$observed),
      n_used = sum(.data$used),
      n_out_of_bounds = sum(.data$bounds),
      .by = "wave"
    )
    out$variable_name <- variable
    out[out$n_values > 0, ]
  })
  counts <- dplyr::bind_rows(
    tibble::tibble(
      wave = character(),
      n_values = integer(),
      n_used = integer(),
      n_out_of_bounds = integer(),
      variable_name = character()
    ),
    dplyr::bind_rows(counts)
  )
  notes <- dplyr::bind_rows(
    tibble::tibble(
      variable_name = character(),
      wave = character(),
      all_waves = logical(),
      status = character(),
      detail = character()
    ),
    dplyr::bind_rows(audit)
  )
  audit <- finish_outcome_audit(counts, notes, targets)

  used <- vapply(scores, function(x) any(!is.na(x)), logical(1))
  scores <- scores[names(scores)[used]]
  outcomes_z <- outcomes_z[intersect(names(outcomes_z), names(scores))]
  list(
    outcomes = scores,
    outcomes_z = outcomes_z,
    audit = audit,
    repeats = repeats,
    derived = derived$derived[derived$derived$target %in% targets, ]
  )
}

# Rows where one is used in place of another when a wave has both.
bp_preferred_outcome_rows <- function() {
  tibble::tibble(
    keep = "acc_academachment_numgrades",
    drop = "acc_academachment_lettergrades"
  )
}

# The grouping vectors a row's `standardise_by` keys name. `dataset` is
# implicit, because a dataset is prepared on its own.
standardise_groups <- function(keys, base, rows) {
  keys <- setdiff(strsplit(keys, "|", fixed = TRUE)[[1]], "dataset")
  lapply(keys, function(key) {
    if (key == "age_band") {
      return(base$school_stage)
    }
    if (key %in% names(base)) {
      return(base[[key]])
    }
    if (key %in% names(rows)) {
      return(as.character(rows[[key]]))
    }
    stop("Unknown standardise_by key: ", key, call. = FALSE)
  })
}

# Join the counts with the notes made along the way into one status per wave
# and row.
finish_outcome_audit <- function(counts, notes, derived_targets) {
  unscored <- notes[notes$status %in% c("archive", "descriptive_only"), ]
  notes <- notes[!notes$status %in% c("archive", "descriptive_only"), ]
  counts$status <- as.character(ifelse(
    counts$variable_name %in% derived_targets,
    "derived",
    "used"
  ))
  counts$detail <- as.character(ifelse(
    counts$n_out_of_bounds > 0,
    paste0(counts$n_out_of_bounds, " values outside the schema's bounds"),
    NA_character_
  ))
  for (i in seq_len(nrow(notes))) {
    hit <- counts$variable_name == notes$variable_name[i] &
      (notes$all_waves[i] |
        (is.na(counts$wave) & is.na(notes$wave[i])) |
        counts$wave %in% notes$wave[i])
    counts$status[hit] <- notes$status[i]
    counts$detail[hit] <- notes$detail[i]
  }
  counts$n_out_of_bounds <- NULL
  extra <- tibble::tibble(
    variable_name = unscored$variable_name,
    wave = NA_character_,
    n_values = NA_integer_,
    n_used = 0L,
    status = unscored$status,
    detail = unscored$detail
  )
  out <- dplyr::bind_rows(counts, extra)
  out$kind <- rep("outcome", nrow(out))
  out
}
