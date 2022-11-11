#' Generate a `{dm}` Object of the Database
#'
#' Working with a `{dm}` object can make life easier than writing manual queries
#' Also allows the visualization of the database with `dm::dm_draw()`.
#'
#' @inheritParams rpwf_add_py_model
#'
#' @return `{dm}` object of the created schema.
#' @export
#'
#' @examples
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db("db.SQLite", tmp_dir)
#' dm_obj <- rpwf_dm(db_con)
#' dm_obj
#' # dm_obj |> dm::dm_draw()
rpwf_dm <- function(db_con) {
  model_type_tbl <- model_type_id <- r_grid_tbl <- grid_id <- df_tbl <- NULL
  df_id <- wflow_tbl <- wflow_id <- wflow_result_tbl <- NULL

  if (!rlang::is_installed("dm")) {
    stop("dm package is required for this function")
  }
  dm::dm_from_con(db_con$con, learn_keys = FALSE) |>
    dm::dm_add_pk(model_type_tbl, model_type_id) |>
    dm::dm_add_pk(r_grid_tbl, grid_id) |>
    dm::dm_add_pk(df_tbl, df_id) |>
    dm::dm_add_pk(wflow_tbl, wflow_id) |>
    dm::dm_add_pk(wflow_result_tbl, wflow_id) |>
    dm::dm_add_fk(wflow_tbl, model_type_id, model_type_tbl) |>
    dm::dm_add_fk(wflow_tbl, grid_id, r_grid_tbl) |>
    dm::dm_add_fk(wflow_tbl, df_id, df_tbl, on_delete = "cascade") |>
    dm::dm_add_fk(wflow_result_tbl, wflow_id, wflow_tbl, on_delete = "cascade")
}
