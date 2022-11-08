# rpwf_workflow_export ---------------------------------------------------------
#' Constructor for rpwf_workflow_set Class
#'
#' @param obj a [tibble::tibble()].
#'
#' @return a "rpwf_workflow_set" object, which inherits from [tibble::tibble()].
#' @noRd
new_rpwf_workflow_set <- function (obj = tibble::tibble()) {
  stopifnot(tibble::is_tibble(obj))
  structure(obj, class = c("rpwf_workflow_set", class(tibble::tibble())))
}

rpwf_augment <- function(obj, ...) {
  UseMethod("rpwf_augment")
}
