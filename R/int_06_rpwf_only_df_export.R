#' Function for Exporting `rpwf_data_set()`
#'
#' @noRd
rpwf_export_dfs_ <- function(obj, db_con) {
  rpwf_Rgrid_R6_id_(obj = obj, db_con = db_con) |>
    rpwf_TrainDf_R6_id_(db_con = db_con) |>
    rpwf_export_fns_(c("df_id", "grid_id"))(db_con = db_con)
}
