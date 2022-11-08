#' Internal Function to check if the Specified Model is Available in the Database
#'
#' @inheritParams rpwf_dm
#' @param py_module string of what the py module is e.g., `xgboost`,
#' `sklearn.ensemble`.
#' @param py_base_learner string of what the base learner is e.g.,
#' `XGBClassifier`.
#' @param r_engine string of the r_engine used.
#'
#' @return either an error or "Model found in db".
#' @noRd
#'
#' @examples
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db("db.SQLite", tmp_dir)
#' rpwf_db_init_(db_con$con, rpwf_schema()) # Create the database
#' rpwf_chk_model_avail_(db_con$con, "xgboost", "XGBClassifier", "xgboost")
rpwf_chk_model_avail_ <- function(con, py_module, py_base_learner, r_engine) {
  stopifnot("only accept vector of length 1 as arguments" = all(
    c(length(py_module), length(py_base_learner), length(r_engine)) == 1
  ))
  query_results <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT * FROM model_type_tbl
      WHERE py_module = ? AND py_base_learner = ? AND r_engine = ?;",
      .con = con
    ),
    list(py_module, py_base_learner, r_engine)
  )
  if (nrow(query_results) != 1) {
    stop(
      paste(
        "Invalid scikit-learn model, select only one model",
        "from `rpwf_avail_models()` or add models with `rpwf_add_py_model()`"
      ),
      sep = " "
    )
  }
}
