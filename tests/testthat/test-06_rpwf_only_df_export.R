test_that("rpwf_data_set()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)
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

