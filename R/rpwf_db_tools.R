#' Delete exported workflows
#'
#' This function specifically just deletes the workflows from the `wflow_tbl`
#'
#' @param con a [DBI::dbConnect()] object. Created by [rpwf::rpwf_db_con()]
#' @param id numeric vector of workflow ids to be removed.
#'
#' @export
#'
#' @examples
#' # Delete workflows with id from 1 to 99 of the database defined by `con`
#' con = rpwf_db_con("db.SQLite", here::here())
#' rpwf_db_del_wflow(1:99, con)
rpwf_db_del_wflow = function(id, con) {
  try(dbExecute(
    conn = con,
    glue::glue_sql('DELETE from wflow_tbl WHERE wflow_id IN ({ids*})',
                   ids = id, .con = con)
    )
  )
}

#' Switch function to find what the corresponding id column of tables
#'
#' Not meant to be called manually.
#'
#' @param tbl table names, i.e., "cost_tbl", "df_tbl", etc. Throw error if
#' tries to return id of "wflow_tbl"
#'
#' @return a character string of id column for respective table.
#' @export
#'
#' @examples
#' id_col_switch_("cost_tbl")
#' id_col_switch_("wflow_tbl") # error
id_col_switch_ = function(tbl) {
  switch(tbl,
         "cost_tbl" = "cost_id",
         "df_tbl" = "df_id",
         "model_type_tbl" = "model_type_id",
         "r_grid_tbl" = "grid_id",
         "wflow_result_tbl" = "result_id",
         "wflow_tbl" = stop("Use rpwf_db_del_wflow() instead"),
         stop("tbl not valid"))
  }

#' Deletes rows from tables other than `wflow_tbl`
#'
#' The deletion of workflows from `wflow_tbl` is specified separately to
#' avoid mistakes.
#'
#' @param tbls vector of character of table names, i.e., "cost_tbl", "df_tbl",
#' "model_type_tbl", "r_grid_tbl", "r_grid_tbl", "wflow_result_tbl"
#' @param id vector of ids to be deleted from a particular table
#' @param con a [DBI::dbConnect()] object. Created by [rpwf::rpwf_db_con()]
#'
#' @return NULL
#' @export
#'
#' @examples
#' con = rpwf_db_con("db.SQLite", here::here())
#' rpwf_db_del_single_tbl("cost_tbl", 1, con)
rpwf_db_del_entry = function(tbls, id, con) {
  for(tbl in tbls){
    id_col = id_col_switch_(tbl)
    query = glue::glue_sql('DELETE from {`tbl`} WHERE {`id_col`} IN ({ids*})',
                           ids = id, .con = con)
    message(query)
    try(dbExecute(conn = con, query))
  }
}

