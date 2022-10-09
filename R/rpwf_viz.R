#' Generate a `{dm}` Object of the Database
#'
#' Working with a `{dm}` object can make life easier than writing manual queries
#'
#' @param con (`DBI::dbConnect()`)\cr
#' a [DBI::dbConnect()] object, created by [DbCon]. Access with `DbCon$new()$con`.
#'
#' @return `{dm}` object of the created schema.
#' @export
#'
#' @examples
#' db_con <- DbCon$new("db.SQLite", ".")
#' dm_obj <- rpwf_dm_obj(db_con$con)
#' dm_obj
#' # dm_obj |> dm::dm_draw()
rpwf_dm_obj <- function(con) {
  cost_tbl <- cost_tbl <- model_type_tbl <- model_type_id <- r_grid_tbl <- NULL
  grid_id <- df_tbl <- df_id <- wflow_tbl <- wflow_id <- wflow_result_tbl <- NULL
  cost_id <- NULL
  if (!rlang::is_installed("dm")) {
    stop("dm package is required for this function")
  }
  dm::dm_from_con(con, learn_keys = FALSE) |>
    dm::dm_add_pk(cost_tbl, cost_id) |>
    dm::dm_add_pk(model_type_tbl, model_type_id) |>
    dm::dm_add_pk(r_grid_tbl, grid_id) |>
    dm::dm_add_pk(df_tbl, df_id) |>
    dm::dm_add_pk(wflow_tbl, wflow_id) |>
    dm::dm_add_pk(wflow_result_tbl, wflow_id) |>
    dm::dm_add_fk(wflow_tbl, cost_id, cost_tbl) |>
    dm::dm_add_fk(wflow_tbl, model_type_id, model_type_tbl) |>
    dm::dm_add_fk(wflow_tbl, grid_id, r_grid_tbl) |>
    dm::dm_add_fk(wflow_tbl, df_id, df_tbl, on_delete = "cascade") |>
    dm::dm_add_fk(wflow_result_tbl, wflow_id, wflow_tbl, on_delete = "cascade")
}
