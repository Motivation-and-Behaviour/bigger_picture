#' Split the imputation numbers into the groups fitted together
#'
#' @param indexes The imputation numbers.
#' @param size How many a group holds.
#' @return An unnamed list of integer vectors, in order.
split_imputations <- function(indexes, size = bp_imputation_group_size()) {
  unname(split(indexes, ceiling(seq_along(indexes) / size)))
}
