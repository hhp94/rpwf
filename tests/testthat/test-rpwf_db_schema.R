test_that("Can create a connection and create SQL tables", {
  withr::local_package("DBI")
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  created_tables = sort(dbListTables(con))
  required_tables = sort(names(rpwf_schema))

  # Folder is created
  expect_true(dir.exists(paste(tmp_dir, "rpwfDb", sep = "/")))
  # Connection is valid
  expect_true(dbIsValid(con))
  # All tables are created
  expect_true(all(created_tables == required_tables))
  DBI::dbDisconnect(con)
})

test_that("Duplicated values of model_type_tbl and cost_tbl are ignored", {
  withr::local_package("DBI")
  withr::local_package("glue")
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  before = sapply(c("cost_tbl", "model_type_tbl"),
                  \(x){nrow(DBI::dbGetQuery(con, glue("SELECT * FROM {x}")))})
  # Try to add repeated values
  print(before)
  rpwf_db_ini_val(con = con)
  after = sapply(c("cost_tbl", "model_type_tbl"),
                 \(x){nrow(DBI::dbGetQuery(con, glue("SELECT * FROM {x}")))})
  print(after)
  expect_true(all(before == after))
  DBI::dbDisconnect(con)
})

