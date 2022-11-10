#' Delete Exported Workflows and Associated Files
#'
#' This function specifically deletes the workflows from the `wflow_tbl`.
#'
#' @inheritParams rpwf_add_py_model
#' @param id numeric vector of workflow ids to be removed.
#' @param delete_files deletes the associated files or not.
#'
#' @return Called for the side effect.
#' @export
#'
#' @examples
#' # Delete workflows with id from 1 to 99 of the database defined by `con`
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db("db.SQLite", tmp_dir)
#' rpwf_db_del_wflow(1:99, db_con)
rpwf_db_del_wflow <- function(id, db_con, delete_files = FALSE) {
  if (delete_files) {
    query_list <- list(
      grid = "SELECT r.grid_path FROM wflow_tbl AS w
              INNER JOIN r_grid_tbl AS r ON w.grid_id = r.grid_id;",
      df = "SELECT d.df_path FROM wflow_tbl AS w
            INNER JOIN df_tbl AS d ON w.df_id = d.df_id;",
      results = "SELECT wr.result_path, wr.model_path FROM wflow_tbl AS w
                 INNER JOIN wflow_result_tbl AS wr ON w.wflow_id = wr.wflow_id;"
    )

    unlink_fns <- function(query, db_con) {
      query_results <- DBI::dbGetQuery(db_con$con, query) # returns a data.frame
      if (nrow(query_results) == 0) {
        return(NULL)
      }
      paths <- query_results |> # returns a data.frame
        unlist() |> # convert columns to list of vectors
        purrr::reduce(c) |> # bind columns (vectors of paths) to vector
        unique() |> # remove duplicated path
        na.omit() # remove NA paths
      if (length(paths) == 0) {
        return(NULL)
      } else {
        unlink(paste(db_con$proj_root_path, paths, sep = "/"))
      }
    }

    purrr::walk(query_list, unlink_fns, db_con = db_con)
  }

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
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db("db.SQLite", tmp_dir)
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
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db("db.SQLite", tmp_dir)
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
  ) |> dplyr::as_tibble()

  if (import_csv) {
    df$fit_results <- lapply(
      df$result_path,
      \(x) {
        dplyr::as_tibble(
          utils::read.csv(paste(db_con$proj_root_path, x, sep = "/"))
        )
      }
    )
  }

  return(df)
}
