# DbCon - Create folder and db -------------------------------------------------
#' @name DbCon
#' @title R6 Object that Stores the Connection and Path to the Db
#'
#' @description
#' Create the "rpwfDb" folder in the provided root path and create a db if
#' needed. Initialize with `db = DbCon$new(<db_name>, here::here())`.
#'
#' @keywords internal
#' @export DbCon
DbCon <- R6::R6Class(
  "DbCon",
  public = list(
    #' @field db_name name of the database.
    db_name = NULL,
    #' @field proj_root_path root path of the project.
    proj_root_path = NULL,
    #' @field db_path path to the database.
    db_path = NULL,
    #' @field con a [DBI::dbConnect()] object to the database.
    con = NULL,
    #' @description
    #' Create an `DbCon` object. Should be a singleton. `self$con` returns a
    #' [DBI::dbConnect()] object and `self$proj_root_path` returns
    #' `proj_root_path`.
    #' @param db_name name of the database.
    #' @param proj_root_path root path of the project.
    #' @return A new `DbCon` object.
    #' @examples
    #' db_con = DbCon$new("db.SQLite", tempdir())
    #' db_con$con
    #' db_con$proj_root_path
    initialize = function(db_name, proj_root_path) {
      self$db_name <- db_name
      self$proj_root_path <- proj_root_path
      self$db_path <- paste("rpwfDb", self$db_name, sep = "/")
      # Create the root path if needed
      withr::with_dir(self$proj_root_path, {
        if (!dir.exists("rpwfDb")) {
          dir.create("rpwfDb")
        }
        if (file.exists(self$db_path)) {
          message("db found")
        } else {
          message("creating new db")
        }
        self$con <- DBI::dbConnect(RSQLite::SQLite(), dbname = self$db_path)
      })
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
#' to create new tables in the database. Not meant to be called manually.
#'
#' @keywords internal
#'
#' @export DbCreate
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
    #' db_con = DbCon$new("db.SQLite", tempdir())
    #' db = DbCreate$new(db_con$con, "SELECT * FROM wflow_tbl")
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
    grid_path VARCHAR UNIQUE, /* Path to grid parquet, one NULL is accepted*/
    grid_hash VARCHAR UNIQUE NOT NULL /* Hash of the grid for caching */
  );"
  # df_tbl-----------------------------------------------
  tbl$df_tbl <-
    "CREATE TABLE IF NOT EXISTS df_tbl(
    df_id INTEGER PRIMARY KEY,
    idx_col VARCHAR, /* id column for pandas index */
    target VARCHAR, /* target column, NULL if test data.frame */
    predictors VARCHAR NOT NULL, /* predictors columns */
    df_path VARCHAR UNIQUE NOT NULL, /* Path to the parquet file of the experiment */
    df_hash VARCHAR UNIQUE NOT NULL /* Hash of the *recipe* to juice the file */
  );"
  # wflow_tbl-----------------------------------------------
  tbl$wflow_tbl <-
    "CREATE TABLE IF NOT EXISTS wflow_tbl(
    wflow_id INTEGER PRIMARY KEY,
    wflow_desc VARCHAR NOT NULL, /* description of the wflow */
    costs VARCHAR NOT NULL, /* cost function */
    model_type_id INTEGER NOT NULL, /* model function */
    py_base_learner_args VARCHAR, /* args passed to base learner in python */
    grid_id INTEGER NOT NULL, /* id of the grid for grid search */
    df_id INTEGER NOT NULL, /* id of the train df */
    random_state INTEGER NOT NULL,  /* Experiment seed, not yet implemented */
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

# Add initial values to new db -------------------------------------------------
#' Add Initial Values to `r_grid_tbl` and `model_type_tbl`
#'
#' Add some initial values such as the cost functions and the XGBoost model as
#' defined in R and Python. Won't update duplicated rows. Expand compatibility
#' by adding values to this function.
#'
#' @inheritParams rpwf_dm
#'
#' @return Used for side effects.
#' @keywords internal
#' @export
#'
#' @examples
#' db_con <- DbCon$new("db.SQLite", tempdir())
#' rpwf_db_init_(db_con$con, rpwf_schema()) # Create the db
#' DBI::dbListTables(db_con$con)
rpwf_db_init_values_ <- function(con) {
  # Add a value for NA grid
  grid_tbl_query <-
    'INSERT INTO r_grid_tbl (grid_hash)
    VALUES
    ("5963bac0ddd4b0c3af914e1d4375ed4e");' # rlang::hash for NA
  message("Adding initial values to the database")
  ## Add stuff into the model_type_tbl
  ### 'sup_mod_df' is generated in data-raw/supported_models.R
  try(DBI::dbAppendTable(con, "model_type_tbl", sup_mod_df), silent = TRUE)
  ## Add the empty values for the r grid
  try(DBI::dbExecute(con, grid_tbl_query), silent = TRUE)
}

# Wrapper for db creation -------------------------------------------------
#' Create the Database and Add Initial Values
#'
#' A wrapper around [DbCreate] and [rpwf_db_init_values_()]. This function
#' iteratively runs through the table creation queries in the schema
#' object and create the tables after creating a new `rpwfDb` folder and
#' database specified by the `DbCon$new()` object.
#'
#' @inheritParams rpwf_dm
#' @param schema output of [rpwf_schema()].
#' @return Used for side effects.
#' @export
#'
#' @examples
#' db_con <- DbCon$new("db.SQLite", tempdir())
#' rpwf_db_init_(db_con$con, rpwf_schema()) # Create the database
#' DBI::dbListTables(db_con$con)
rpwf_db_init_ <- function(con, schema = rpwf_schema()) {
  invisible( ### Create the data base
    DbCreate$new(con = con, query = NULL)$
      run(schema$model_type_tbl)$
      run(schema$r_grid_tbl)$
      run(schema$df_tbl)$
      run(schema$wflow_tbl)$
      run(schema$wflow_result_tbl)
  )
  rpwf_db_init_values_(con = con) ### Add some initial values
}

# Add models to the db ---------------------------------------------------------
#' Add scikit-learn Models to database
#'
#' Use this function to add or update the scikit-learn model using the module (i.e.,
#' "xgboost"), the base learner in scikit-learn (i.e., "XGBClassifier"),
#' the corresponding `{parsnip}` engine (i.e., "xgboost"), the equivalent hyper
#' parameter names (i.e., "mtry" in `{parsnip}` is "colsample_bytree"), and
#' model mode (i.e., "classification")
#'
#' @inheritParams rpwf_dm
#' @param py_module the module in scikit-learn, i.e., "sklearn.ensemble".
#' @param py_base_learner the base learner in scikit-learn, i.e.,
#' "RandomForestClassifier".
#' @param r_engine the engine in parsnip, i.e., "ranger" or "rpart".
#' @param hyper_par_rename a named list of equivalent hyper parameters, i.e.,
#' `list(cost_complexity = "ccp_alpha")`
#' @param model_mode classification or regression
#'
#' @return Use for side effect to update DB, not returning any values
#' @export
#'
#' @examples
#' # Generate dummy database
#' db_con <- rpwf_connect_db("db.SQLite", tempdir())
#' DBI::dbListTables(db_con$con)
#' DBI::dbGetQuery(db_con$con, "SELECT * FROM model_type_tbl") # before adding
#' rpwf_add_py_model(
#'   db_con$con,
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
rpwf_add_py_model <- function(con,
                              py_module,
                              py_base_learner,
                              r_engine,
                              hyper_par_rename,
                              model_mode) {
  query_results <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT model_type_id
      FROM model_type_tbl
      WHERE py_module = ? AND py_base_learner = ? AND r_engine = ? AND model_mode = ?;",
      .con = con
    ),
    params = list(py_module, py_base_learner, r_engine, model_mode)
  )

  rename_json <- as.character(jsonlite::toJSON(hyper_par_rename))
  if (nrow(query_results) == 0) {
    DBI::dbExecute(
      con,
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
        .con = con
      )
    )
  } else if (nrow(query_results) == 1) {
    DBI::dbExecute(
      con,
      glue::glue_sql(
        "UPDATE model_type_tbl
        SET py_module = ?, py_base_learner = ?, r_engine = ?,
            hyper_par_rename = ?, model_mode = ?
        WHERE model_type_id = ?;",
        .con = con
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
#' @param db_name name of the database.
#' @param proj_root_path root path of the project.
#' @return A new `DbCon` object.
#'
#' @export
#'
#' @examples
#' db_con <- rpwf_connect_db("db.SQLite", tempdir())
#' db_con$con
#' db_con$proj_root_path
rpwf_connect_db <- function(db_name, proj_root_path) {
  db_con <- DbCon$new(db_name = db_name, proj_root_path = proj_root_path)
  rpwf_db_init_(db_con$con, rpwf_schema())
  return(db_con)
}
