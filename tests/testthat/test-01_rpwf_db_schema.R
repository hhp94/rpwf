test_that("rpwf_connect_db()", {
  board <- pins::board_temp()
  tmp_dir <- withr::local_tempdir()
  db_name <- paste(tmp_dir, "db.SQLite", sep = "/")
  db_con <- rpwf_connect_db(db_name, board)

  created_tables <- sort(DBI::dbListTables(db_con$con))
  required_tables <- sort(names(rpwf_schema()))

  # Folder is created
  expect_true(fs::file_exists(db_name))
  # Connection is valid
  expect_true(DBI::dbIsValid(db_con$con))
  # All tables are created
  expect_true(all(created_tables == required_tables))
})

test_that("Duplicated values of model_type_tbl are ignored", {
  board <- pins::board_temp()
  tmp_dir <- withr::local_tempdir()
  db_name <- paste(tmp_dir, "db.SQLite", sep = "/")
  db_con <- rpwf_connect_db(db_name, board)

  before <- sapply(
    c("model_type_tbl", "r_grid_tbl"),
    \(x){
      nrow(DBI::dbGetQuery(db_con$con, glue::glue("SELECT * FROM {x}")))
    }
  )
  # Try to add repeated values
  rpwf_db_init_values_(db_con)
  after <- sapply(
    c("model_type_tbl", "r_grid_tbl"),
    \(x){
      nrow(DBI::dbGetQuery(db_con$con, glue::glue("SELECT * FROM {x}")))
    }
  )
  expect_true(all(before == after))
})

test_that("Add additional models with rpwf_add_py_model()", {
  board <- pins::board_temp()
  tmp_dir <- withr::local_tempdir()
  db_name <- paste(tmp_dir, "db.SQLite", sep = "/")
  db_con <- rpwf_connect_db(db_name, board)

  before <- DBI::dbGetQuery(db_con$con, "SELECT * FROM model_type_tbl")
  rpwf_add_py_model(
    db_con,
    "sklearn.ensemble",
    "RandomForestClassifier",
    "rpart",
    list(
      cost_complexity = "ccp_alpha",
      tree_depth = "max_depth",
      min_n = "min_samples_split"
    ),
    "classification"
  )
  after <- DBI::dbGetQuery(db_con$con, "SELECT * FROM model_type_tbl")

  # sup_mod_df__ add the required models. So the after would be the models plus
  # a new model
  expect_true(nrow(after) == 1 + nrow(sup_mod_df__))

  # Try updating
  rpwf_add_py_model(
    db_con,
    "sklearn.ensemble",
    "RandomForestClassifier",
    "rpart",
    list(
      cost_complexity = "ccp_alpha",
      tree_depth = "max_depth"
    ),
    "classification"
  )
  updated <- DBI::dbGetQuery(db_con$con, "SELECT * FROM model_type_tbl")

  expect_true(after$hyper_par_rename[1 + nrow(sup_mod_df__)] == jsonlite::toJSON(
    list(
      cost_complexity = "ccp_alpha",
      tree_depth = "max_depth",
      min_n = "min_samples_split"
    )
  ))

  expect_true(updated$hyper_par_rename[1 + nrow(sup_mod_df__)] == jsonlite::toJSON(
    list(
      cost_complexity = "ccp_alpha",
      tree_depth = "max_depth"
    )
  ))
})
