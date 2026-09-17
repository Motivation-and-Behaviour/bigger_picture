# Allowlist of functions available as bare names inside `variables.csv`
# expressions.
#
# Expressions are evaluated in a sandboxed environment (see
# `harmonisation_eval_env()`): the tidied study tibble is the data mask,
# dataset bindings (`spec`, `tbl`, `analysis_base`, `lookup_*`) sit above it,
# then this allowlist, then base R. Expressions cannot reach the global
# environment or attached packages, which keeps the harmonisation vocabulary
# explicit, auditable, and testable outside of `targets`.
#
# Namespaced calls such as `haven::as_factor(x)` remain available because `::`
# is a base function; prefer adding recurring functions here so the vocabulary
# stays reviewable in one place.
bp_harmonisation_functions <- function() {
  list(
    # dplyr recode/conditional vocabulary
    recode_values = dplyr::recode_values,
    if_else = dplyr::if_else,
    case_when = dplyr::case_when,
    case_match = dplyr::case_match,
    coalesce = dplyr::coalesce,
    na_if = dplyr::na_if,
    # haven labelled-data helpers
    as_factor = haven::as_factor,
    # repo helpers (defined in R/helpers.R)
    lookup_values = lookup_values,
    sum_nonmissing = sum_nonmissing
  )
}

# Parse an expression string and collect every function name used in call
# position. Bare calls are returned as the function name (e.g.
# "recode_values"); namespaced calls are returned as "pkg::fn". Used by the
# test suite to verify that every `variables.csv` expression only calls
# functions that resolve in the harmonisation evaluation environment.
harmonisation_expression_calls <- function(expression) {
  expr <- rlang::parse_expr(expression)
  unique(collect_call_names(expr))
}

collect_call_names <- function(node) {
  if (!is.call(node)) {
    return(character(0))
  }

  head <- node[[1]]
  rest <- as.list(node)[-1]

  if (is.name(head)) {
    head_name <- as.character(head)
    if (head_name %in% c("::", ":::")) {
      return(paste0(
        as.character(node[[2]]),
        head_name,
        as.character(node[[3]])
      ))
    }
    return(c(
      head_name,
      unlist(lapply(rest, collect_call_names), use.names = FALSE)
    ))
  }

  # Head is itself a call, e.g. `pkg::fn(...)` where node[[1]] is `pkg::fn`
  unlist(
    lapply(as.list(node), collect_call_names),
    use.names = FALSE
  )
}
