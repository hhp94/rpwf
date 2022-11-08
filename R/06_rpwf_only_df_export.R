# # This is for if one wants to just pass the data frame to Python
#
# rpwf_df_set <- function(..., db_con) {
#   preprocs <- list(...)
#
#   stopifnot("preproc accept recipes" = "recipe" == unique(sapply(preprocs, class)))
#
#   df <- tibble::tibble(preprocs = unique(preprocs))
#   df$recipe_tag <- sapply(df$preprocs, \(x) {
#     x$recipe_tag
#   })
#   df$grids <- NA
#   df$TrainDf <- lapply(df$preprocs, \(x) {TrainDf$new(x, db_con)})
#   return(df)
# }

# #' Export the Locations of Train/Test Parquets into the Database
# #'
# #' @param obj object generated by [rpwf_df_set].
# #' @inheritParams rpwf_write_grid
# #'
# #' @return number of rows exported.
# #' @export
# #' @examples
# #' # Create the database
# #' temp_dir <- withr::local_tempdir()
# #' db_con <- rpwf_connect_db("db.SQLite", temp_dir)
# #'
# #' d <- mtcars
# #' d$target <- stats::rbinom(nrow(d), 1, 0.5)
# #' r1 <- d |>
# #'   recipes::recipe(target ~ .)
# #' df <- rpwf_df_set(r1)
# #'
# #' rpwf_write_df(df, db_con)
# #'
# #' # Before exporting
# #' DBI::dbGetQuery(db_con$con, "SELECT * FROM wflow_tbl;")
# #' # After exporting
# #' rpwf_export_dfs(df, db_con)
# #' DBI::dbGetQuery(db_con$con, "SELECT * FROM wflow_tbl;")
# rpwf_export_dfs <- rpwf_export_fns_(c("df_id", "grid_id", "recipe_tag"))