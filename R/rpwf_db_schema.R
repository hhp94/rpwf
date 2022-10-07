# Create a folder and connection -----------------------------------------------
#' A wrapper for [DBI::dbConnect()] using [RSQLite::SQLite()]
#'
#' Creates, if needed, the `rpwfDb` folder in the project root path that's
#' needed for python codes and a connection to the database in that folder.
#'
#' @param db_name a string name of the database
#' @param proj_root_path a string that indicate the root of the project.
#' The use of `here::here()` within an Rstudio project is recommended.
#'
#' @return A [DBI::dbConnect()] object
#' @export
#'
#' @examples
#' con = rpwf_db_con("db.SQLite", here::here())
#' con
rpwf_db_con = function(db_name, proj_root_path){
  withr::local_dir(proj_root_path)
  if(!dir.exists("rpwfDb")){dir.create("rpwfDb")}
  db_path = paste("rpwfDb", db_name, sep = "/")
  if(file.exists(db_path)){ message("db found")} else { message("creating new db") }
  return(DBI::dbConnect(RSQLite::SQLite(), dbname = db_path))
}

# R6 obj to create schema -----------------------------------------------
#' @title Internal R6 object that set and run a SQL query
#'
#' @description
#' A R6 object that provides a shortcut to setting and executing a SQL query
#' to create new tables in the database. Not meant to be called manually
DbCreate = R6::R6Class("DbCreate",
  public = list(
    #' @field con a [DBI::dbConnect()] object, created by [rpwf::rpwf_db_con()]
    con = NULL,
    #' @field query pre-defined SQL query to create a table
    query = NULL,
    #' @description
    #' Create an [rpwf::DbCreate] object. Should be a singleton
    #' @param con [DBI::dbConnect()] connection
    #' @param query a SQL query string
    #' @return A new `DbCreate` object.
    initialize = function(con, query){
      self$con = con
      self$query = query},

    #' @description
    #' Change the query
    #' @param query a new SQL query string
    #' @examples
    #' con = rpwf_db_con("db.SQLite", here::here())
    #' db = DbCreate(con, "SELECT * FROM wflow_tbl")
    #' db$query
    set_query = function(query) {self$query = query; invisible(self)},

    #' @description
    #' Run the SQL query
    execute = function() {DBI::dbExecute(self$con, self$query); invisible(self)},

    #' @description
    #' Wrapper around `self$set_query()` and `self$execute()` that set query and
    #' run the query.
    #' @param query a new SQL query string
    #' @examples
    #' ?rpwf::rpwf_db_init()
    run = function(query) {self$set_query(query = query)$execute(); invisible(self)}
  )
)

# Schema description -----------------------------------------------------------
#' Function contains the db schema
#'
#' @description
#' Wrapper around the SQL queries that holds the table definitions
#'
#' Here are the tables available in the database. Print out the raw SQL code
#' with `rpwf_schema()$<table name>` to see constraints and variable names.
#'
#' * __cost_tbl__: contains the cost functions
#' * __model_type_tbl__: defines comparable models in R and python
#' * __df_tbl__: holds the transformed data (train and test) to pass to python
#' * __r_grid_tbl__: holds hyper param grids created in R
#' * __wflow_tbl__: defines all the params needed to run a ML model in SKlearn
#' * __wflow_result_tbl__: holds results of the workflow ran by python
#'
#' @return a named list with containing SQL query strings defining a table
#' @export
#'
#' @examples
#' definitions = rpwf_schema()
#' names(definitions)
rpwf_schema = function() {
  tbl = list()
# cost_tbl-----------------------------------------------
  tbl$cost_tbl =
    "CREATE TABLE IF NOT EXISTS cost_tbl(
    cost_id INTEGER PRIMARY KEY,
    cost_name VARCHAR(50) NOT NULL, /* neg_log_lost or roc_auc and etc. */
    model_mode VARCHAR(14) NOT NULL CHECK(model_mode in ('regression', 'classification')),
    UNIQUE(cost_name, model_mode)
  );"
# model_type_tbl-----------------------------------------------
  tbl$model_type_tbl =
    "CREATE TABLE IF NOT EXISTS model_type_tbl(
    model_type_id INTEGER PRIMARY KEY,
    py_module VARCHAR(50) NOT NULL, /* xgboost sklearn.ensemble etc */
    py_base_learner VARCHAR(50) NOT NULL, /* XGB GBM etc */
    r_engine VARCHAR(50) NOT NULL, /* R engine types */
    model_mode VARCHAR(14) NOT NULL CHECK(model_mode in ('regression', 'classification')),
    UNIQUE(r_engine, model_mode),
    UNIQUE(r_engine, py_base_learner, py_module),
    UNIQUE(py_module, py_base_learner, r_engine, model_mode)
  );"
# r_grid_tbl-----------------------------------------------
  tbl$r_grid_tbl =
    "CREATE TABLE IF NOT EXISTS r_grid_tbl(
    grid_id INTEGER PRIMARY KEY,
    grid_path VARCHAR UNIQUE, /* Path to grid parquet, one NULL is accepted*/
    grid_hash VARCHAR UNIQUE NOT NULL /* Hash of the grid for caching */
  );"
# df_tbl-----------------------------------------------
  tbl$df_tbl =
    "CREATE TABLE IF NOT EXISTS df_tbl(
    df_id INTEGER PRIMARY KEY,
    idx_col VARCHAR NOT NULL, /* id column for pandas index */
    target VARCHAR, /* target column, NULL if test data.frame */
    predictors VARCHAR NOT NULL, /* predictors columns */
    df_path VARCHAR UNIQUE NOT NULL, /* Path to the parquet file of the experiment */
    df_hash VARCHAR UNIQUE NOT NULL /* Hash of the *recipe* to juice the file */
  );"
# wflow_tbl-----------------------------------------------
  tbl$wflow_tbl =
    "CREATE TABLE IF NOT EXISTS wflow_tbl(
    wflow_id INTEGER PRIMARY KEY,
    wflow_desc VARCHAR NOT NULL, /* description of the wflow */
    cost_id INTEGER NOT NULL, /* cost function */
    model_type_id INTEGER NOT NULL, /* model function */
    py_base_learner_args VARCHAR, /* args passed to base learner in python */
    grid_id INTEGER NOT NULL, /* id of the grid for grid search */
    df_id INTEGER NOT NULL, /* id of the train df */
    random_state INTEGER NOT NULL,  /* Experiment seed, not yet implemented */
    CONSTRAINT cost_id_fk
      FOREIGN KEY (cost_id)
      REFERENCES cost_tbl (cost_id),
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
    UNIQUE(df_id, grid_id, cost_id, model_type_id, random_state, py_base_learner_args)
  );"
# wflow_result_tbl-----------------------------------------------
  tbl$wflow_result_tbl =
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

# Add initial values to new db -------------------------------------------------
#' Add initial values to the `cost_tbl` and `model_type_tbl`
#'
#' Add some initial values such as the cost functions and the XGBoost model as
#' defined in R and Python. Won't update duplicated rows. Expand compatibility
#' by adding values to this function.
#'
#' @inheritParams rpwf_dm_obj
#'
#' @return NULL
#' @export
#'
#' @examples
#' ?rpwf_db_init()
rpwf_db_ini_val = function(con) {
  cost_tbl_query =
    'INSERT INTO cost_tbl (cost_name, model_mode)
    VALUES
    ("roc_auc", "classification"),
    ("neg_log_loss", "classification");'
  model_type_tbl_query =
    'INSERT INTO model_type_tbl (py_module, py_base_learner, r_engine, model_mode)
    VALUES
    ("xgboost", "XGBClassifier", "xgboost", "classification"),
    ("lightgbm", "LGBMClassifier", "lightgbm", "classification");'
  grid_tbl_query =
    'INSERT INTO r_grid_tbl (grid_hash)
    VALUES
    ("5963bac0ddd4b0c3af914e1d4375ed4e");' # rlang::hash for NA
  ## Add stuff into cost table
  try(DBI::dbExecute(conn = con, cost_tbl_query), silent = TRUE)
  ## Add stuff into the model_type_tbl
  try(DBI::dbExecute(conn = con, model_type_tbl_query), silent = TRUE)
  ## Add the empty values for the r grid
  try(DBI::dbExecute(conn = con, grid_tbl_query), silent = TRUE)
}

# Wrapper for db creation -------------------------------------------------
#' Create the database and add initial values
#'
#' A wrapper around [rpwf::DbCreate$execute()] and [rpwf::rpwf_db_ini_val()].
#' This function iteratively runs through the table creation queries in the schema
#' object and create the tables after creating a new `rpwfDb` folder and
#' database specified by the `rpwf_db_con()` function.
#'
#' @inheritParams rpwf_dm_obj
#' @param schema a [rpwf::rpwf_schema()] object
#' @return NULL
#' @export
#'
#' @examples
#' con = rpwf_db_con("db.SQLite", here::here())
#' rpwf_db_init(con) # Create the database
#' DBI::dbDisconnect(con)
rpwf_db_init = function(con, schema) {
  invisible( ### Create the data base
    DbCreate$new(con = con, query = NULL)$
      run(schema$cost_tbl)$
      run(schema$model_type_tbl)$
      run(schema$r_grid_tbl)$
      run(schema$df_tbl)$
      run(schema$wflow_tbl)$
      run(schema$wflow_result_tbl))
  rpwf_db_ini_val(con = con) ### Add some initial values
}
