test_that("Can create a connection and create SQL tables", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")

  db_con <- DbCon$new("db.SQLite", tmp_dir)
  rpwf_db_init_(db_con$con, rpwf_schema())

  created_tables <- sort(DBI::dbListTables(db_con$con))
  required_tables <- sort(names(rpwf_schema()))

  # Folder is created
  expect_true(dir.exists(paste(tmp_dir, "rpwfDb", sep = "/")))
  # Connection is valid
  expect_true(DBI::dbIsValid(db_con$con))
  # All tables are created
  expect_true(all(created_tables == required_tables))
})

test_that("Can create a connection and create SQL tables", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- DbCon$new("db.SQLite", tmp_dir)
  rpwf_db_init_(db_con$con, rpwf_schema())

  created_tables <- sort(DBI::dbListTables(db_con$con))
  required_tables <- sort(names(rpwf_schema()))

  # Folder is created
  expect_true(dir.exists(paste(tmp_dir, "rpwfDb", sep = "/")))
  # Connection is valid
  expect_true(DBI::dbIsValid(db_con$con))
  # All tables are created
  expect_true(all(created_tables == required_tables))
})

test_that("Duplicated values of model_type_tbl are ignored", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)

  before <- sapply(
    c("model_type_tbl", "r_grid_tbl"),
    \(x){
      nrow(DBI::dbGetQuery(db_con$con, glue::glue("SELECT * FROM {x}")))
    }
  )
  # Try to add repeated values
  rpwf_db_init_values_(db_con$con)
  after <- sapply(
    c("model_type_tbl", "r_grid_tbl"),
    \(x){
      nrow(DBI::dbGetQuery(db_con$con, glue::glue("SELECT * FROM {x}")))
    }
  )
  expect_true(all(before == after))
})

test_that("Can create a connection and create SQL tables", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")

  db_con <- rpwf_create_db("db.SQLite", tmp_dir)

  created_tables <- sort(DBI::dbListTables(db_con$con))
  required_tables <- sort(names(rpwf_schema()))

  # Folder is created
  expect_true(dir.exists(paste(tmp_dir, "rpwfDb", sep = "/")))
  # Connection is valid
  expect_true(DBI::dbIsValid(db_con$con))
  # All tables are created
  expect_true(all(created_tables == required_tables))
})

test_that("Add additional models with rpwf_add_py_model()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")

  db_con <- rpwf_create_db("db.SQLite", tmp_dir)

  DBI::dbListTables(db_con$con)
  before <- DBI::dbGetQuery(db_con$con, "SELECT * FROM model_type_tbl")
  rpwf_add_py_model(
    db_con$con,
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
  expect_true(nrow(after) == 2)

  # Try updating
  rpwf_add_py_model(
    db_con$con,
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

  expect_true(after$hyper_par_rename[2] == jsonlite::toJSON(
    list(
      cost_complexity = "ccp_alpha",
      tree_depth = "max_depth",
      min_n = "min_samples_split"
    )
  ))

  expect_true(updated$hyper_par_rename[2] == jsonlite::toJSON(
    list(
      cost_complexity = "ccp_alpha",
      tree_depth = "max_depth"
    )
  ))
})
