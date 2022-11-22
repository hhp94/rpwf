# Juice and Export data only to Python
#' Juice data from a Recipe
#'
#' Create an object that holds the juiced prepped data to be exported to Python.
#'
#' @param ... [recipes::recipe()] objects
#' @inheritParams rpwf_add_py_model
#'
#' @return a `rpwf_data_set` object
#' @export
#'
#' @examples
#' board <- pins::board_temp()
#' tmp_dir <- tempdir()
#' db_con <- rpwf_connect_db(paste(tmp_dir, "db.SQLite", sep = "/"), board)
#'
#' r <- recipes::recipe(mpg ~ ., data = mtcars)
#' d <- rpwf_data_set(r, db_con = db_con)
#' d
rpwf_data_set <- function(..., db_con) {
  preprocs <- list(...)

  stopifnot("preproc accept recipes" = "recipe" == unique(sapply(preprocs, class)))
  # Add columns. No need for the `rpwf_augment()` method.
  df <- tibble::tibble(preprocs = unique(preprocs))
  # Add the recipe tag column
  df$recipe_tag <- vapply(df$preprocs, \(x) {
    if (is.null(x$recipe_tag)) {
      return(NA_character_)
    } else {
      return(x$recipe_tag)
    }
  }, "character")

  r_tag <- df$recipe_tag[which(!is.na(df$recipe_tag))]
  stopifnot("duplicated recipe tags error" = length(r_tag) ==
    dplyr::n_distinct(r_tag))

  df$grids <- NA
  df$Rgrid <- lapply(df$grids, \(x) {
    RGrid$new(x, db_con)
  })
  df$TrainDf <- lapply(df$preprocs, \(x) {
    TrainDf$new(x, db_con)
  })
  return(new_rpwf_data_set(df))
}

#' @rdname rpwf_export_db
#' @export
rpwf_export_db.rpwf_data_set <- function(obj, db_con) {
  rpwf_export_dfs_(obj, db_con)
}
