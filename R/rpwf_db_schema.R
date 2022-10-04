## Create a new database connection
rpwf_db_con = function(db_name, proj_root_path){
  withr::local_dir(proj_root_path)
  if(!dir.exists("rpwfDb")){dir.create("rpwfDb")}
  db_path = paste("rpwfDb", db_name, sep = "/")
  if(file.exists(db_path)){ message("db found")} else { message("creating new db") }
  return(DBI::dbConnect(RSQLite::SQLite(), dbname = db_path))
}

## Object that run queries
rpwfDbCreate = R6::R6Class("rpwfDbCreate",
  public = list(
    con = NULL, # Connection
    query = NULL, # Query
    initialize = function(con, query){
      self$con = con
      self$query = query},
    set_query = function(query) {self$query = query; invisible(self)},
    execute = function() {DBI::dbExecute(self$con, self$query); invisible(self)},
    run = function(val) {self$set_query(query = val)$execute(); invisible(self)}
  )
)

## Contains schema information stored as list with each element a table definition
rpwf_schema = function() {
  tbl = list()
  tbl$cost_tbl_create =
    "CREATE TABLE IF NOT EXISTS cost_tbl(
    cost_id INTEGER PRIMARY KEY,
    cost_name VARCHAR(50) NOT NULL, /* neg_log_lost or roc_auc and etc. */
    model_mode VARCHAR(14) NOT NULL CHECK(model_mode in ('regression', 'classification')),
    UNIQUE(cost_name, model_mode)
  );"

  tbl$model_type_tbl_create =
    "CREATE TABLE IF NOT EXISTS model_type_tbl(
    model_type_id INTEGER PRIMARY KEY,
    py_module VARCHAR(50) NOT NULL, /* xgboost sklearn.ensemble etc */
    py_base_learner VARCHAR(50) NOT NULL, /* XGB GBM etc */
    r_engine VARCHAR(50) NOT NULL, /* R engine types */
    model_mode VARCHAR(14) NOT NULL CHECK(model_mode in ('regression', 'classification')),
    UNIQUE(r_engine, model_mode),
    UNIQUE(r_engine, py_base_learner),
    UNIQUE(py_module, py_base_learner, r_engine, model_mode)
  );"

  tbl$r_grid_tbl_create =
    "CREATE TABLE IF NOT EXISTS r_grid_tbl(
    grid_id INTEGER PRIMARY KEY,
    grid_path VARCHAR UNIQUE NOT NULL, /* Path to grid parquet*/
    grid_hash VARCHAR UNIQUE NOT NULL /* Hash of the grid for caching */
  );"

  tbl$df_tbl_create =
    "CREATE TABLE IF NOT EXISTS df_tbl(
    df_id INTEGER PRIMARY KEY,
    idx_col VARCHAR NOT NULL, /* id column for pandas index */
    target VARCHAR NOT NULL, /* target column */
    predictors VARCHAR NOT NULL, /* predictors columns */
    df_path VARCHAR UNIQUE NOT NULL, /* Path to the parquet file of the experiment */
    df_hash VARCHAR UNIQUE NOT NULL /* Hash of the *recipe* to juice the file */
  );"

  tbl$wflow_tbl_create =
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

  tbl$wflow_result_tbl_create =
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

## Add initial values to the database
rpwf_db_ini_val = function(con) {
  ## Add stuff into cost table
  try({
    DBI::dbExecute(
      conn = con,
      'INSERT INTO cost_tbl (cost_name, model_mode)
    VALUES
    ("roc_auc", "classification"),
    ("neg_log_loss", "classification");'
    )
  }, silent = TRUE)
  ## Add stuff into the model_type_tbl
  try({
    DBI::dbExecute(
      conn = con,
      'INSERT INTO model_type_tbl (py_module, py_base_learner, r_engine, model_mode)
    VALUES
    ("xgboost", "XGBClassifier", "xgboost", "classification"),
    ("lightgbm", "LGBMClassifier", "lightgbm", "classification");'
    )
  }, silent = TRUE)
}

## Execute the query, add tables and the initial values
rpwf_db_init = function(con, schema) {
  invisible( ### Create the data base
    rpwfDbCreate$new(con = con, query = NULL)$
      run(schema$cost_tbl_create)$
      run(schema$model_type_tbl_create)$
      run(schema$r_grid_tbl_create)$
      run(schema$df_tbl_create)$
      run(schema$wflow_tbl_create)$
      run(schema$wflow_result_tbl_create))
  rpwf_db_ini_val(con = con) ### Add some initial values
}
