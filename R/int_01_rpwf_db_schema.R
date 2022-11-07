# Add initial values to new db -------------------------------------------------
#' Add Initial Values to `r_grid_tbl` and `model_type_tbl`
#'
#' Add some initial values such as the cost functions and the XGBoost model as
#' defined in R and Python. Won't update duplicated rows. Expand compatibility
#' by adding values to this function.
#'
#' @inheritParams rpwf_dm
#'
#' @return Used for side effects.
#' @noRd
#'
#' @examples
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db("db.SQLite", tmp_dir)
#' DBI::dbListTables(db_con$con)
rpwf_db_init_values_ <- function(con) {
  # Add a value for NA grid
  grid_tbl_query <-
    'INSERT INTO r_grid_tbl (grid_hash)
    VALUES
    ("5963bac0ddd4b0c3af914e1d4375ed4e");' # rlang::hash for NA
  message("Adding initial values to the database")
  ## Add stuff into the model_type_tbl
  ### 'sup_mod_df__' is generated in data-raw/supported_models.R
  try(DBI::dbAppendTable(con, "model_type_tbl", sup_mod_df__), silent = TRUE)
  ## Add the empty values for the r grid
  try(DBI::dbExecute(con, grid_tbl_query), silent = TRUE)
}

# Wrapper for db creation -------------------------------------------------
#' Create the Database and Add Initial Values
#'
#' A wrapper around [DbCreate] and [rpwf_db_init_values_()]. This function
#' iteratively runs through the table creation queries in the schema
#' object and create the tables after creating a new `rpwfDb` folder and
#' database specified by the `DbCon$new()` object.
#'
#' @inheritParams rpwf_dm
#' @param schema output of [rpwf_schema()].
#' @return Used for side effects.
#' @noRd
#'
#' @examples
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db("db.SQLite", tmp_dir)
#' DBI::dbListTables(db_con$con)
rpwf_db_init_ <- function(con, schema = rpwf_schema()) {
  invisible( ### Create the data base
    DbCreate$new(con = con, query = NULL)$
      run(schema$model_type_tbl)$
      run(schema$r_grid_tbl)$
      run(schema$df_tbl)$
      run(schema$wflow_tbl)$
      run(schema$wflow_result_tbl)
  )
  rpwf_db_init_values_(con = con) ### Add some initial values
}
