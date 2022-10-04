# This function specifically just deletes the workflows from the `wflow_tbl`
rpwf_db_del_wflow = function(id, con){
  try(dbExecute(
    conn = con,
    glue::glue_sql('DELETE from wflow_tbl WHERE wflow_id IN ({ids*})',
                   ids = id, .con = con))
  )
}

# Given the table, what is the corresponding id column
id_col_switch_ = function(tbl){
  switch(tbl,
         "cost_tbl" = "cost_id",
         "df_tbl" = "df_id",
         "model_type_tbl" = "model_type_id",
         "r_grid_tbl" = "grid_id",
         "wflow_result_tbl" = "result_id",
         "wflow_tbl" = stop("Use rpwf_db_del_wflow() instead"),
         stop("tbl not valid"))
  }

# This function deletes rows from other tables
rpwf_db_del_single_tbl = function(tbl, id, con){
  id_col = id_col_switch_(tbl)
  query = glue::glue_sql('DELETE from {`tbl`} WHERE {`id_col`} IN ({ids*})',
                         ids = id, .con = con)
  print(query)
  try(dbExecute(conn = con, query))
}

# This function delete rows from multiple tables
rpwf_db_del_mult_tbl = function(tbl, id, con){
  for(i in tbl) { rpwf_db_del_single_tbl(i, id = id, con = con) }
}
