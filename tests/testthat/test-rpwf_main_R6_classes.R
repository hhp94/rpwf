# Test TrainDf R6 class --------------------------------------------------------
test_that("test the initialization of the TrainDf class", {
  withr::local_package("DBI")
  withr::local_package("R6")
  withr::local_package("jsonlite")

  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "train")
  # initialization
  train_df_obj = TrainDf$new(dummy_test_rec, con, tmp_dir)
  expect_equal(class(train_df_obj$prepped), "recipe")
  # these three values are set in the recipe data
  expect_equal(train_df_obj$db_folder, "TrainDf")
  expect_equal(train_df_obj$idx_col, "id")
  expect_equal(train_df_obj$target, "target")
  expect_equal(parse_json(train_df_obj$predictors),
               list("X1", "X2", "X3_X1", "X3_X2", "X3_X3", "X3_X4"))
  # This recipe is newly added, so the SQL query would return a 0 row data.frame
  expect_true(is.data.frame(train_df_obj$query_results))
  expect_equal(nrow(train_df_obj$query_results), 0)
  # The SQL query is 0 row, make a new export query to insert to database
  expect_true(!is.null(train_df_obj$export_query))
})

test_that("test the export() method of the TrainDf class", {
  withr::local_package("DBI")
  withr::local_package("R6")
  withr::local_package("jsonlite")
  withr::local_package("glue")

  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "train")
  # initialization
  train_df_obj = TrainDf$new(dummy_test_rec, con, tmp_dir)
  # write the parquet and export the database
  train_df_obj$export()

  # Check if the data is exported into the database
  expect_equal(
    DBI::dbGetQuery(con, glue("SELECT df_hash FROM df_tbl WHERE df_id = 1"))$df_hash,
    train_df_obj$hash
  )
  expect_true(file.exists(paste(tmp_dir, train_df_obj$path, sep = "/")))
  # Removing the file and re-exporting would create a new file work
  unlink(paste(tmp_dir, train_df_obj$path, sep = "/"))
  # File doesn't exist after unlinking
  expect_true(!file.exists(paste(tmp_dir, train_df_obj$path, sep = "/")))
  train_df_obj$export() # Re-export
  expect_true(file.exists(paste(tmp_dir, train_df_obj$path, sep = "/")))
})

test_that("test that export() method won't add repeated rows class", {
  withr::local_package("DBI")
  withr::local_package("R6")
  withr::local_package("jsonlite")
  withr::local_package("glue")

  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "train")
  # initialization of a new TrainDf object
  train_df_obj = TrainDf$new(dummy_test_rec, con, tmp_dir)
  # write the parquet and export the database
  train_df_obj$export()
  # initialize a new TrainDf object using the same recipe
  train_df_obj_repeated = TrainDf$new(dummy_test_rec, con, tmp_dir)
  # If we try the same recipe, hash check would find one row
  expect_equal(nrow(train_df_obj_repeated$query_results), 1)
  # if hash check find one row, then export query would return NULL
  expect_true(is.null(train_df_obj_repeated$export_query))
  # `recipes::juice()` won't run, so self$df is NULL
  expect_true(is.null(train_df_obj_repeated$df))
  # and the path to the file would be the same
  expect_equal(train_df_obj_repeated$path, as.character(train_df_obj$path))
})

# Test Rgrid generation --------------------------------------------------------
test_that("Test the renaming function", {
  # Check if individual value works
  expect_equal(rpwf_grid_rename("mtry"), "colsample_bytree")
  expect_equal(rpwf_grid_rename("trees"), "n_estimators")
  # Check if invalid values are returned as is
  expect_equal(rpwf_grid_rename("dummy"), "dummy")
  # Check if a vector value are accepted
  expect_equal(rpwf_grid_rename(c("tree_depth", "dummy")),
               c("max_depth", "dummy"))
})

test_that("Test rpwf_grid_gen() with tune()", {
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  withr::local_package("jsonlite")
  withr::local_package("dials")

  con = dummy_con_(tmp_dir = tmp_dir)

  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec = xgb_model_spec_() |>
    set_py_engine("XGBClassifier",
                  args = list(eval_metric = "logloss", silent = TRUE))
  grid_size = 10
  # Test the generation of the grids
  partial_fns = purrr::partial(rpwf_grid_gen, dummy_mod_spec, dummy_test_rec,
                               size = grid_size)

  c_grid_lhcube = partial_fns(.grid_fun = dials::grid_latin_hypercube)
  c_grid_rand = partial_fns(.grid_fun = dials::grid_random)

  expect_equal(c(nrow(c_grid_lhcube), ncol(c_grid_lhcube)), c(grid_size, 6))
  expect_equal(c(nrow(c_grid_rand), ncol(c_grid_rand)), c(grid_size, 6))
  expect_true("rpwf_grid" %in% class(c_grid_rand))
})

test_that("Test rpwf_grid_gen() no tuning param", {
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  withr::local_package("jsonlite")
  withr::local_package("dials")

  con = dummy_con_(tmp_dir = tmp_dir)

  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec = xgb_model_spec_no_tune_() |>
    set_py_engine("XGBClassifier",
                  args = list(eval_metric = "logloss", silent = TRUE))
  grid_size = 10
  # Test the generation of the grids
  partial_fns = purrr::partial(rpwf_grid_gen, dummy_mod_spec, dummy_test_rec,
                               size = grid_size)

  c_grid_lhcube = partial_fns(.grid_fun = dials::grid_latin_hypercube)
  c_grid_rand = partial_fns(.grid_fun = dials::grid_random)

  expect_equal(c(nrow(c_grid_lhcube), ncol(c_grid_lhcube)), c(grid_size, 6))
  expect_equal(c(nrow(c_grid_rand), ncol(c_grid_rand)), c(grid_size, 6))
  expect_true("rpwf_grid" %in% class(c_grid_rand))
})

test_that("Test rpwf_finalize_params()", {
  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec = xgb_model_spec_() |>
    set_py_engine("XGBClassifier",
                  args = list(eval_metric = "logloss", silent = TRUE))
  dummy_mod_spec_no_tune_ = xgb_model_spec_no_tune_() |>
    set_py_engine("XGBClassifier",
                  args = list(eval_metric = "logloss", silent = TRUE))
  expect_equal(nrow(rpwf_finalize_params(dummy_mod_spec, dummy_test_rec)$par), 6)
  expect_equal(nrow(rpwf_finalize_params(dummy_mod_spec_no_tune_,
                                         dummy_test_rec)$par), 0)
})


# Test RGrid R6 class --------------------------------------------------------
test_that("test the initialization of the RGrid class", {
  withr::local_package("DBI")
  withr::local_package("R6")
  withr::local_package("jsonlite")
  withr::local_package("dials")

  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  # Generate a grid
  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec = xgb_model_spec_() |>
    set_py_engine("XGBClassifier",
                  args = list(eval_metric = "logloss", silent = TRUE))
  grid_size = 10
  # Test the generation of the grids
  partial_fns = purrr::partial(rpwf_grid_gen, dummy_mod_spec, dummy_test_rec,
                               size = grid_size)

  c_grid_lhcube = partial_fns(.grid_fun = dials::grid_latin_hypercube)

  # Generate an object
  r_grid_obj = RGrid$new(c_grid_lhcube, con, tmp_dir)
  # This recipe is newly added, so the SQL query would return a 0 row data.frame
  expect_true(is.data.frame(r_grid_obj$query_results))
  expect_equal(nrow(r_grid_obj$query_results), 0)
  # The SQL query is 0 row, make a new export query to insert to database
  expect_true(!is.null(r_grid_obj$export_query))
})

test_that("test the export() method of the RGrid class", {
  withr::local_package("DBI")
  withr::local_package("R6")
  withr::local_package("jsonlite")
  withr::local_package("dials")
  withr::local_package("glue")

  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  # Generate a grid
  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec = xgb_model_spec_() |>
    set_py_engine("XGBClassifier",
                  args = list(eval_metric = "logloss", silent = TRUE))
  grid_size = 10
  # Test the generation of the grids
  partial_fns = purrr::partial(rpwf_grid_gen, dummy_mod_spec, dummy_test_rec,
                               size = grid_size)

  c_grid_lhcube = partial_fns(.grid_fun = dials::grid_latin_hypercube)

  # Generate an object
  r_grid_obj = RGrid$new(c_grid_lhcube, con, tmp_dir)
  # write the parquet and export the database
  r_grid_obj$export()

  # Check if the data is exported into the database
  expect_equal(
    DBI::dbGetQuery(
      con,
      glue("SELECT grid_hash FROM r_grid_tbl WHERE grid_id = 1"))$grid_hash,
    r_grid_obj$hash
  )
  expect_true(file.exists(paste(tmp_dir, r_grid_obj$path, sep = "/")))
  # Removing the file and re-exporting would create a new file work
  unlink(paste(tmp_dir, r_grid_obj$path, sep = "/"))
  # File doesn't exist after unlinking
  expect_true(!file.exists(paste(tmp_dir, r_grid_obj$path, sep = "/")))
  r_grid_obj$export() # Re-export
  expect_true(file.exists(paste(tmp_dir, r_grid_obj$path, sep = "/")))
})

test_that("test that export() method won't add repeated rows class", {
  withr::local_package("DBI")
  withr::local_package("R6")
  withr::local_package("jsonlite")
  withr::local_package("dials")
  withr::local_package("glue")

  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  # Generate a grid
  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec = xgb_model_spec_() |>
    set_py_engine("XGBClassifier",
                  args = list(eval_metric = "logloss", silent = TRUE))
  grid_size = 10
  # Test the generation of the grids
  partial_fns = purrr::partial(rpwf_grid_gen, dummy_mod_spec, dummy_test_rec,
                               size = grid_size)

  c_grid_lhcube = partial_fns(.grid_fun = dials::grid_latin_hypercube)

  # Generate an object
  r_grid_obj = RGrid$new(c_grid_lhcube, con, tmp_dir)
  r_grid_obj$export()
  # initialize a new RGrid object using the same recipe
  rgrid_obj_repeated = RGrid$new(c_grid_lhcube, con, tmp_dir)
  # If we try the same recipe, hash check would find one row
  expect_equal(nrow(rgrid_obj_repeated$query_results), 1)
  # if hash check find one row, then export query would return NULL
  expect_true(is.null(rgrid_obj_repeated$export_query))
  # `recipes::juice()` won't run, so self$df is NULL
  expect_true(is.null(rgrid_obj_repeated$df))
  # and the path to the file would be the same
  expect_equal(rgrid_obj_repeated$path, as.character(r_grid_obj$path))
})
