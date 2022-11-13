test_that("rpwf_data_set()", {
  board <- pins::board_temp()
  tmp_dir <- withr::local_tempdir()
  db_name <- paste(tmp_dir, "db.SQLite", sep = "/")
  db_con <- rpwf_connect_db(db_name, board)

  df <- rpwf_sim_()$train

  r <- recipes::recipe(target ~ ., data = df) |>
    recipes::update_role(id, new_role = "pd.index") |>
    rpwf_tag_recipe("base")
  r2 <- r |>
    rpwf_tag_recipe("base")
  r3 <- r |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    rpwf_tag_recipe("normalize")

  df <- rpwf_data_set(r, r2, r3, db_con = db_con)
  # print(df)
  expect_equal(nrow(df), 2L)
})

test_that("rpwf_export_db.rpwf_data_set()", {
  board <- pins::board_temp()
  tmp_dir <- withr::local_tempdir()
  db_name <- paste(tmp_dir, "db.SQLite", sep = "/")
  db_con <- rpwf_connect_db(db_name, board)

  query_wflow_tbl <- function() {
    DBI::dbGetQuery(db_con$con, "SELECT * FROM wflow_tbl")
  }
  # Add a train df
  df <- rpwf_sim_()$train

  r <- recipes::recipe(target ~ ., data = df) |>
    recipes::update_role(id, new_role = "pd.index") |>
    rpwf_tag_recipe("base")
  r2 <- r |>
    rpwf_tag_recipe("base")
  r3 <- r |>
    recipes::step_normalize(recipes::all_numeric_predictors()) |>
    rpwf_tag_recipe("normalize")

  df <- rpwf_data_set(r, r2, r3, db_con = db_con)
  expect_equal(length(list.files(board$path)), 0L)

  rpwf_write_df(df)
  expect_equal(length(list.files(board$path)), 2L)

  before <- query_wflow_tbl()

  expect_equal(nrow(before), 0) # before export, there would be no wflow
  rpwf_export_db(df, db_con)
  after <- query_wflow_tbl() # after export, there would be 1 wflow
  expect_equal(nrow(after), 2)
  rpwf_export_db(df, db_con)
  after1 <- query_wflow_tbl() # exporting the same wflow would not work
  expect_equal(nrow(after1), 2)
})
