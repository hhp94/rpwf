test_that("Can create a connection and create SQL tables", {
  withr::local_package("DBI")
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  db_name = "base_db.SQLite"
  con = rpwf_db_con(db_name, tmp_dir)
  rpwf_db_init(con, rpwf_schema())

  created_tables = sort(dbListTables(con))
  required_tables = sort(names(rpwf_schema))

  # Folder is created
  expect_true(dir.exists(paste(tmp_dir, "rpwfDb", sep = "/")))
  # Connection is valid
  expect_true(dbIsValid(con))
  # All tables are created
  expect_true(all(created_tables == required_tables))
})

test_that("Duplicated values of model_type_tbl and cost_tbl are ignored", {
  withr::local_package("DBI")
  withr::local_package("glue")

  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  db_name = "base_db.SQLite"
  con = rpwf_db_con(db_name, tmp_dir)
  rpwf_db_init(con, rpwf_schema())

  before = sapply(c("cost_tbl", "model_type_tbl"),
                  \(x){nrow(DBI::dbGetQuery(con, glue("SELECT * FROM {x}")))})

  # Try to add repeated values
  rpwf_db_ini_val(con = con)

  after = sapply(c("cost_tbl", "model_type_tbl"),
                 \(x){nrow(DBI::dbGetQuery(con, glue("SELECT * FROM {x}")))})

  expect_true(all(before == after))
})

