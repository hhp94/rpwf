#' Switch Function to Find What the Corresponding id Column of Tables Are
#'
#' Not meant to be called manually.
#'
#' @param tbl table names, i.e., "df_tbl", etc. Throw error if
#' tries to return id of "wflow_tbl".
#'
#' @return a character string of id column for respective table.
#' @noRd
#'
#' @examples
#' id_col_switch_("df_tbl")
#' # id_col_switch_("wflow_tbl") # error
id_col_switch_ <- function(tbl) {
  switch(tbl,
    "df_tbl" = "df_id",
    "model_type_tbl" = "model_type_id",
    "r_grid_tbl" = "grid_id",
    "wflow_result_tbl" = "result_id",
    "wflow_tbl" = stop("Use rpwf_db_del_wflow() instead"),
    stop("tbl not valid")
  )
}
