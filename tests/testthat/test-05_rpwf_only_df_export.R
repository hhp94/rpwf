# test_that("rpwf_df_set()", {
#   tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
#   db_con <- dummy_con_(tmp_dir = tmp_dir)
#   df <- rpwf_sim_()$train
#
#   r <- recipes::recipe(target ~ ., data = df) |>
#     recipes::update_role(id, new_role = "pd.index") |>
#     rpwf_tag_recipe("base")
#   r2 <- r |>
#     rpwf_tag_recipe("base")
#   r3 <- r |>
#     recipes::step_normalize(recipes::all_numeric_predictors()) |>
#     rpwf_tag_recipe("normalize")
#
#   df <- rpwf_df_set(r, r2, r3, db_con = db_con)
#   print(df)
#   expect_equal(nrow(df), 2L)
# })
#
# test_that("rpwf_write_df()", {
#   tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
#   db_con <- dummy_con_(tmp_dir = tmp_dir)
#   folder <- paste(tmp_dir, "rpwfDb", sep = "/")
#   df <- rpwf_sim_()$train
#
#   r <- recipes::recipe(target ~ ., data = df) |>
#     recipes::update_role(id, new_role = "pd.index") |>
#     rpwf_tag_recipe("base")
#   r3 <- r |>
#     recipes::step_normalize(recipes::all_numeric_predictors()) |>
#     rpwf_tag_recipe("normalize")
#
#   df <- rpwf_df_set(r, r3)
#   expect_equal(length(list.files(paste(folder, sep = "/"))), 1L)
#   rpwf_write_df(df)
#   expect_true("db.SQLite_df" %in% list.files(paste(folder, sep = "/")))
#   expect_equal(length(list.files(paste(folder, "db.SQLite_df", sep = "/"))), 2)
#   expect_equal(nrow(rpwf_parquet_id_(df, db_con)), 2L)
# })
#
# test_that("rpwf_write_df()", {
#   tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
#   db_con <- dummy_con_(tmp_dir = tmp_dir)
#   folder <- paste(tmp_dir, "rpwfDb", sep = "/")
#   df <- rpwf_sim_()$train
#
#   query_wflow_tbl <- function() {
#     DBI::dbGetQuery(db_con$con, "SELECT * FROM wflow_tbl")
#   }
#
#   r <- recipes::recipe(target ~ ., data = df) |>
#     recipes::update_role(id, new_role = "pd.index") |>
#     rpwf_tag_recipe("base")
#   r3 <- r |>
#     recipes::step_normalize(recipes::all_numeric_predictors()) |>
#     rpwf_tag_recipe("normalize")
#
#   df <- rpwf_df_set(r, r3)
#   expect_equal(length(list.files(paste(folder, sep = "/"))), 1L)
#   expect_error(rpwf_export_dfs(df, db_con), regexp = "rpwf_write_df")
#   print(list.files(paste(folder, "db.SQLite_df", sep = "/")))
#
#   before <- query_wflow_tbl()
#   expect_equal(nrow(before), 0) # before export, there would be no wflow
#   rpwf_write_df(df, db_con)
#   # print(list.files(paste(folder, "db.SQLite_df", sep = "/")))
#   a <- rlang::hash((r3))
#   # rpwf_parquet_id_(df, db_con)
#   after <- query_wflow_tbl()
#   # gc()
#   # expect_equal(nrow(after), 2)
#   b <- rlang::hash((r3))
#   print(waldo::compare(a, b))
#   # rpwf_parquet_id_(df, db_con)
#   # after1 <- query_wflow_tbl() # exporting the same wflow would not work
#   # expect_equal(nrow(after1), 2)
# })
