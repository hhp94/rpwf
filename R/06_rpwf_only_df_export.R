# This is for if one wants to just pass the data frame to Python
rpwf_data_set <- function(..., db_con) {
  preprocs <- list(...)

  stopifnot("preproc accept recipes" = "recipe" == unique(sapply(preprocs, class)))
  # Add columns. No need for the `rpwf_augment()` method.
  df <- tibble::tibble(preprocs = unique(preprocs))
  df$recipe_tag <- sapply(df$preprocs, \(x) { x$recipe_tag })
  df$grids <- NA
  df$Rgrid <- lapply(df$grids, \(x) { RGrid$new(x, db_con) })
  df$TrainDf <- lapply(df$preprocs, \(x) {TrainDf$new(x, db_con)})
  return(new_rpwf_data_set(df))
}

#' @rdname rpwf_export_db
#' @export
rpwf_export_db.rpwf_data_set <- function(obj, db_con) {
  rpwf_export_dfs_(obj, db_con)
}
