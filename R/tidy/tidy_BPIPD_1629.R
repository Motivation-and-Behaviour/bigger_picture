#' Tidier for BPIPD-1629 (EU Kids Online)
#'
#' One row per child (9-16 y.o. internet user); the primary file already
#' carries both the child (`QC*`) and parent (`QP*`) interview, so no join is needed.
tidy_BPIPD_1629 <- function(raw_dataset, spec) {
  tibble::as_tibble(raw_dataset$data$`Primary Dataset`)
}
