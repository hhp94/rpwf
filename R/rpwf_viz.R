require(dm)
require(RSQLite)

feat_eng_dm = 
  dm::dm_from_con(RSQLite.con, learn_keys = FALSE) |>
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