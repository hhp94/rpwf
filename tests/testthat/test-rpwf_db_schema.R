test_that("Can create a connection and create SQL tables", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")

  db_con <- DbCon$new("db.SQLite", tmp_dir)
  rpwf_db_init(db_con$con, rpwf_schema())

  created_tables <- sort(DBI::dbListTables(db_con$con))
  required_tables <- sort(names(rpwf_schema))

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
  rpwf_db_init(db_con$con, rpwf_schema())

  created_tables <- sort(DBI::dbListTables(db_con$con))
  required_tables <- sort(names(rpwf_schema))

  # Folder is created
  expect_true(dir.exists(paste(tmp_dir, "rpwfDb", sep = "/")))
  # Connection is valid
  expect_true(DBI::dbIsValid(db_con$con))
  # All tables are created
  expect_true(all(created_tables == required_tables))
})

test_that("Duplicated values of model_type_tbl and cost_tbl are ignored", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)

  before <- sapply(
    c("cost_tbl", "model_type_tbl", "r_grid_tbl"),
    \(x){
      nrow(DBI::dbGetQuery(db_con$con, glue::glue("SELECT * FROM {x}")))
    }
  )
  # Try to add repeated values
  print(before)
  rpwf_db_ini_val(db_con$con)
  after <- sapply(
    c("cost_tbl", "model_type_tbl", "r_grid_tbl"),
    \(x){
      nrow(DBI::dbGetQuery(db_con$con, glue::glue("SELECT * FROM {x}")))
    }
  )
  print(after)
  expect_true(all(before == after))
})
