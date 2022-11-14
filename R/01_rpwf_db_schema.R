# Board S3 methods -------------------------------------------------------------
#' @rdname rpwf_write_board_yaml
#' @export
rpwf_write_board_yaml.pins_board_folder <- function(board, file) {
  yaml_board <- yaml::yaml.load(yaml::as.yaml(board)) |>
    purrr::list_modify(
      api = purrr::zap(),
      cache = purrr::zap(),
      versioned = TRUE
    )
  yaml::write_yaml(yaml_board, file)
}

#' @import R6
NULL

# DbCon - Create folder and db -------------------------------------------------
#' @name DbCon
#' @title R6 Object that Stores the Connection and Path to the Db
#'
#' @description
#' Create the "rpwfDb" folder in the provided root path and create a db if
#' needed.
#'
#' @noRd
DbCon <- R6::R6Class(
  "DbCon",
  public = list(
    #' @field dbname name of the database.
    dbname = NULL,
    #' @field board a `{pins}` board object.
    board = NULL,
    #' @field db_path path to the database.
    db_path = NULL,
    #' @field con a [DBI::dbConnect()] object to the database.
    con = NULL,
    #' @description
    #' Create an `DbCon` object. Should be a singleton. `self$con` returns a
    #' [DBI::dbConnect()] object and `self$board` returns
    #' `board`.
    #'
    #' @inheritParams rpwf_connect_db
    #'
    #' @return A new `DbCon` object.
    #' @examples
    #' board <- pins::board_temp()
    #' tmp_dir <- withr::local_tempdir()
    #' db_con <- rpwf_connect_db(paste(tmp_dir, "db.SQLite", sep = "/"), board)
    #' db_con$con
    #' db_con$board
    initialize = function(dbname, board, ...) {
      stopifnot("board needs to be a `pins::board_<>` object" = all("pins_board" %in% class(board)))
      stopifnot(is.character(dbname) & length(dbname) == 1)
      self$dbname <- dbname
      self$board <- board
      # Create the root path if needed
      self$con <- DBI::dbConnect(RSQLite::SQLite(), dbname = self$dbname, ...)
      stopifnot("Created connection is not valid" = DBI::dbIsValid(self$con))
    }
  ),
  private = list(
    finalize = function() {
      DBI::dbDisconnect(self$con)
    }
  )
)

# DbCreate - Create a schema ---------------------------------------------------
#' @title Internal R6 Object that Set and Run a SQL Query
#'
#' @description
#' A R6 object that provides a shortcut to setting and executing a SQL query
#' to create new tables in the database.
#'
#' @noRd
DbCreate <- R6::R6Class("DbCreate",
  public = list(
    #' @field con a [DBI::dbConnect()] object, created by [DbCon].
    con = NULL,
    #' @field query pre-defined SQL query to create a table.
    query = NULL,
    #' @description
    #' Create an `DbCreate` object.
    #' @param con a [DBI::dbConnect()] connection to the database.
    #' @param query a SQL query string.
    #' @return A new `DbCreate` object.
    #' @examples
    #' board <- pins::board_temp()
    #' tmp_dir <- withr::local_tempdir()
    #' db_con <- rpwf_connect_db(paste(tmp_dir, "db.SQLite", sep = "/"), board)
    #' db <- DbCreate$new(db_con$con, "SELECT * FROM wflow_tbl")
    initialize = function(con, query) {
      self$con <- con
      self$query <- query
    },

    #' @description
    #' Change the query
    #' @param query a new SQL query string.
    set_query = function(query) {
      self$query <- query
      invisible(self)
    },

    #' @description
    #' Run the SQL query
    execute = function() {
      DBI::dbExecute(self$con, self$query)
      invisible(self)
    },

    #' @description
    #' Wrapper around `self$set_query()` and `self$execute()` that set query and
    #' run the query.
    #' @param new_table a new SQL query string.
    run = function(new_table) {
      self$set_query(new_table)$execute()
      invisible(self)
    }
  )
)

# Schema description -----------------------------------------------------------
#' The db Schema
#'
#' @description
#' Wrapper around the SQL queries that holds the table definitions.
#'
#' @details
#' Here are the tables available in the database. Print out the raw SQL code
#' with `rpwf_schema()$<table name>` to see constraints and variable names.
#'
#' * __model_type_tbl__: defines comparable models in R and python.
#' * __df_tbl__: holds the transformed data (train and test) to pass to python.
#' * __r_grid_tbl__: holds hyper param grids created in R.
#' * __wflow_tbl__: defines all the params needed to run a ML model in sklearn.
#' * __wflow_result_tbl__: holds results of the workflow ran by python.
#'
#' @return a named list with containing SQL query strings defining a table.
#' @export
#'
#' @examples
#' definitions <- rpwf_schema()
#' names(definitions)
rpwf_schema <- function() {
  tbl <- list()
  # model_type_tbl-----------------------------------------------
  tbl$model_type_tbl <-
    "CREATE TABLE IF NOT EXISTS model_type_tbl(
    model_type_id INTEGER PRIMARY KEY,
    py_module VARCHAR(50) NOT NULL, /* xgboost sklearn.ensemble etc */
    py_base_learner VARCHAR(50) NOT NULL, /* XGB GBM etc */
    r_engine VARCHAR(50) NOT NULL, /* R engine types */
    hyper_par_rename VARCHAR, /* json to rename the columns of the grid */
    model_mode VARCHAR(14) NOT NULL CHECK(model_mode in ('regression', 'classification')),
    UNIQUE(r_engine, model_mode),
    UNIQUE(r_engine, py_base_learner, py_module),
    UNIQUE(py_module, py_base_learner, r_engine, model_mode)
  );"
  # r_grid_tbl-----------------------------------------------
  tbl$r_grid_tbl <-
    "CREATE TABLE IF NOT EXISTS r_grid_tbl(
    grid_id INTEGER PRIMARY KEY,
    grid_pin_name VARCHAR UNIQUE, /* Path to grid parquet, one NULL is accepted*/
    grid_hash VARCHAR UNIQUE NOT NULL /* Hash of the grid for caching */
  );"
  # df_tbl-----------------------------------------------
  tbl$df_tbl <-
    "CREATE TABLE IF NOT EXISTS df_tbl(
    df_id INTEGER PRIMARY KEY,
    idx_col VARCHAR, /* id column for pandas index */
    target VARCHAR, /* target column, NULL if test data.frame */
    predictors VARCHAR NOT NULL, /* predictors columns */
    df_pin_name VARCHAR UNIQUE NOT NULL, /* Path to the parquet file of the experiment */
    df_hash VARCHAR UNIQUE NOT NULL /* Hash of the *recipe* to juice the file */
  );"
  # wflow_tbl-----------------------------------------------
  tbl$wflow_tbl <-
    "CREATE TABLE IF NOT EXISTS wflow_tbl(
    wflow_id INTEGER PRIMARY KEY,
    model_tag VARCHAR, /* tag of the model */
    recipe_tag VARCHAR, /* tag of the recipe */
    costs VARCHAR, /* cost function */
    model_type_id INTEGER, /* model function */
    py_base_learner_args VARCHAR, /* args passed to base learner in python */
    grid_id INTEGER NOT NULL, /* id of the grid for grid search */
    df_id INTEGER NOT NULL, /* id of the train df */
    random_state INTEGER,  /* Experiment seed */
    CONSTRAINT model_type_id_fk
      FOREIGN KEY (model_type_id)
      REFERENCES model_type_tbl (model_type_id),
    CONSTRAINT grid_id_fk
      FOREIGN KEY (grid_id)
      REFERENCES r_grid_tbl (grid_id),
    CONSTRAINT df_id_fk
      FOREIGN KEY (df_id)
      REFERENCES df_tbl (df_id)
      ON DELETE CASCADE,  /* Allows cascade deletion by removing df id */
    UNIQUE(df_id, grid_id, model_type_id, random_state, py_base_learner_args)
  );"
  # wflow_result_tbl-----------------------------------------------
  tbl$wflow_result_tbl <-
    "CREATE TABLE IF NOT EXISTS wflow_result_tbl(
    result_id INTEGER PRIMARY KEY,
    wflow_id INTEGER UNIQUE NOT NULL,
    description VARCHAR,
    result_path VARCHAR UNIQUE NOT NULL, /* Where results are stored (csv) */
    model_path VARCHAR, /* Path where model file is stored in (joblib) */
    CONSTRAINT wflow_id
      FOREIGN KEY (wflow_id)
      REFERENCES wflow_tbl (wflow_id)
      ON DELETE CASCADE,  /* Allows cascade deletion by removing exp id */
    UNIQUE(wflow_id, description, result_path, description)
  );"

  return(tbl)
}

# Add models to the db ---------------------------------------------------------
#' Add scikit-learn Models to Database
#'
#' Use this function to add or update the scikit-learn model using the module (i.e.,
#' "xgboost"), the base learner in scikit-learn (i.e., "XGBClassifier"),
#' the corresponding `{parsnip}` engine (i.e., "xgboost"), the equivalent hyper
#' parameter names (i.e., "mtry" in `{parsnip}` is "colsample_bytree"), and
#' model mode (i.e., "classification")
#'
#' @param db_con (`DBI::dbConnect()`)\cr
#' An [rpwf_connect_db()] object.
#' @param py_module the module in scikit-learn, i.e., "sklearn.ensemble".
#' @param py_base_learner the base learner in scikit-learn, i.e.,
#' "RandomForestClassifier".
#' @param r_engine the engine in parsnip, i.e., "ranger" or "rpart".
#' @param hyper_par_rename a named list of equivalent hyper parameters, i.e.,
#' `list(cost_complexity = "ccp_alpha")`.
#' @param model_mode "classification" or "regression".
#'
#' @return Use for side effect to update DB, not returning any values.
#' @export
#'
#' @examples
#' # Generate dummy database
#' board <- pins::board_temp()
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db(paste(tmp_dir, "db.SQLite", sep = "/"), board)
#' DBI::dbListTables(db_con$con)
#' DBI::dbGetQuery(db_con$con, "SELECT * FROM model_type_tbl") # before adding
#' rpwf_add_py_model(
#'   db_con,
#'   "sklearn.ensemble",
#'   "RandomForestClassifier",
#'   "rpart",
#'   list(
#'     cost_complexity = "ccp_alpha",
#'     tree_depth = "max_depth",
#'     min_n = "min_samples_split"
#'   ),
#'   "classification"
#' )
#' DBI::dbGetQuery(db_con$con, "SELECT * FROM model_type_tbl") # after adding
rpwf_add_py_model <- function(db_con,
                              py_module,
                              py_base_learner,
                              r_engine,
                              hyper_par_rename,
                              model_mode) {
  query_results <- DBI::dbGetQuery(
    db_con$con,
    glue::glue_sql(
      "SELECT model_type_id
      FROM model_type_tbl
      WHERE py_module = ? AND py_base_learner = ? AND r_engine = ? AND model_mode = ?;",
      .con = db_con$con
    ),
    params = list(py_module, py_base_learner, r_engine, model_mode)
  )

  rename_json <- as.character(jsonlite::toJSON(hyper_par_rename))
  if (nrow(query_results) == 0) {
    DBI::dbExecute(
      db_con$con,
      glue::glue_sql(
        "INSERT INTO model_type_tbl (
          py_module, py_base_learner, r_engine, hyper_par_rename, model_mode
        )
        VALUES ({vals*});",
        vals = c(
          py_module,
          py_base_learner,
          r_engine,
          rename_json,
          model_mode
        ),
        .con = db_con$con
      )
    )
  } else if (nrow(query_results) == 1) {
    DBI::dbExecute(
      db_con$con,
      glue::glue_sql(
        "UPDATE model_type_tbl
        SET py_module = ?, py_base_learner = ?, r_engine = ?,
            hyper_par_rename = ?, model_mode = ?
        WHERE model_type_id = ?;",
        .con = db_con$con
      ),
      param = list(
        py_module, py_base_learner, r_engine, rename_json, model_mode,
        query_results$model_type_id
      )
    )
  } else {
    stop("Unexpected error")
  }
}

# Wrapper for the whole process and around the R6 object -----------------------
#' Create a Database and Return an Object that Stores the Connection and Path
#'
#' @description
#' Create the "rpwfDb" folder in the provided root path and create a db if
#' needed. Access the connection with `object$con`
#'
#' @param dbname path to the database, passed to RSQLite::SQLite().
#' @param board a `{pins}` board object.
#' @param ... arguments passed to [DBI::dbConnect()]
#' @return A new `DbCon` object.
#'
#' @export
#'
#' @examples
#' board <- pins::board_temp()
#' tmp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db(paste(tmp_dir, "db.SQLite", sep = "/"), board)
#' db_con$con
#' db_con$board
rpwf_connect_db <- function(dbname, board, ...) {
  db_con <- DbCon$new(dbname = dbname, board = board, ...)
  rpwf_db_init_(db_con, rpwf_schema())
  return(db_con)
}
