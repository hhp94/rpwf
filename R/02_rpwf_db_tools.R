#' Delete Exported Workflows and Associated Files
#'
#' This function specifically deletes the workflows from the `wflow_tbl`.
#'
#' @inheritParams rpwf_add_py_model
#' @param id numeric vector of workflow ids to be removed.
#'
#' @return Called for the side effect.
#' @export
#'
#' @examples
#' # Delete workflows with id from 1 to 99 of the database defined by `con`
#' board <- pins::board_temp()
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db(paste(tmp_dir, "db.SQLite", sep = "/"), board)
#' rpwf_db_del_wflow(1:99, db_con)
rpwf_db_del_wflow <- function(id, db_con) {
  try(DBI::dbExecute(
    conn = db_con$con,
    glue::glue_sql("DELETE from wflow_tbl WHERE wflow_id IN ({ids*})",
      ids = id, .con = db_con$con
    )
  ))
}

#' Deletes Rows from Tables Other than `wflow_tbl`
#'
#' The deletion of workflows from `wflow_tbl` is specified separately to
#' avoid mistakes.
#'
#' @param tbls vector of character of table names, i.e., "df_tbl",
#' "model_type_tbl", "r_grid_tbl", "r_grid_tbl", "wflow_result_tbl".
#' @param id vector of ids to be deleted from a particular table.
#' @inheritParams rpwf_add_py_model
#'
#' @return Called for the side effect.
#' @export
#'
#' @examples
#' board <- pins::board_temp()
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db(paste(tmp_dir, "db.SQLite", sep = "/"), board)
#' # Before deleting
#' DBI::dbGetQuery(db_con$con, "SELECT * FROM model_type_tbl;")
#' rpwf_db_del_entry("model_type_tbl", 1, db_con)
#' # After deleting
#' DBI::dbGetQuery(db_con$con, "SELECT * FROM model_type_tbl;")
rpwf_db_del_entry <- function(tbls, id, db_con) {
  for (tbl in tbls) {
    id_col <- id_col_switch_(tbl)
    query <- glue::glue_sql("DELETE from {`tbl`} WHERE {`id_col`} IN ({ids*})",
      ids = id, .con = db_con$con
    )
    try(DBI::dbExecute(conn = db_con$con, query))
  }
}

#' Show Available Models
#'
#' Models have to be pre-defined and added to the database. Some models are
#' already added. Other models can be added with [rpwf_add_py_model()]. This
#' functions shows the models currently in the database.
#'
#' @inheritParams rpwf_add_py_model
#'
#' @return a data.frame of models available in the database.
#' @export
#'
#' @examples
#' board <- pins::board_temp()
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db(paste(tmp_dir, "db.SQLite", sep = "/"), board)
#' rpwf_avail_models(db_con)
rpwf_avail_models <- function(db_con) {
  DBI::dbGetQuery(
    db_con$con,
    "SELECT py_module, py_base_learner, r_engine, model_mode FROM model_type_tbl"
  )
}

#' Get Results from Finished workflows
#'
#' Wrapper for an inner join between the `wflow_tbl` and `wflow_result_tbl` and
#' invoke [utils::read.csv()] to read in the results.
#'
#' @inheritParams rpwf_add_py_model
#' @param import_csv whether to read in the results of the workflow.
#'
#' @return A tibble with the results stored in the fit_results column
#' @export
rpwf_results <- function(db_con, import_csv = TRUE) {
  df <- DBI::dbGetQuery(
    db_con$con,
    "SELECT w.wflow_id AS wflow_id, w.model_tag AS model_tag,
            w.recipe_tag AS recipe_tag, w.costs AS costs,
            r.description AS description, r.result_path AS result_path,
            r.model_path AS model_path
    FROM wflow_tbl AS w
      INNER JOIN wflow_result_tbl AS r
        ON w.wflow_id = r.wflow_id;"
  ) |>
    dplyr::as_tibble()

  if (import_csv) {
    df$fit_results <- lapply(
      df$result_path,
      \(x) { pins::pin_read(db_con$board, name = x) }
    )
  }

  return(df)
}
