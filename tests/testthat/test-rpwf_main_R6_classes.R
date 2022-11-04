# TrainDf R6 class --------------------------------------------------------
test_that("initialization of the TrainDf class", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")

  # initialization
  train_df_obj <- TrainDf$new(dummy_test_rec, db_con)
  expect_equal(class(train_df_obj$prepped), "recipe")
  # these three values are set in the recipe data
  expect_equal(train_df_obj$idx_col, "id")
  expect_equal(train_df_obj$target, "target")
  expect_equal(
    jsonlite::parse_json(train_df_obj$predictors),
    list("X1", "X2", "X3_X1", "X3_X2", "X3_X3", "X3_X4")
  )
  # This recipe is newly added, so the SQL query would return a 0 row data.frame
  expect_true(is.data.frame(train_df_obj$queried_path))
  expect_equal(nrow(train_df_obj$queried_path), 0)
  # The SQL query is 0 row, make a new export query to insert to database
  expect_true(!is.null(train_df_obj$export_query))
})

test_that("export() method of the TrainDf class", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")

  # initialization
  expect_error(TrainDf$new(dummy_test_rec, "INVALID"),
    regex = "input should be a R6"
  )
  train_df_obj <- TrainDf$new(dummy_test_rec, db_con)
  # write the parquet and export the database
  train_df_obj$export()
  # Check if the data is exported into the database
  expect_equal(
    DBI::dbGetQuery(
      db_con$con,
      glue::glue("SELECT df_hash FROM df_tbl WHERE df_id = 1")
    )$df_hash,
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

test_that("export() method won't add repeated rows class", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")

  # initialization of a new TrainDf object
  train_df_obj <- TrainDf$new(dummy_test_rec, db_con)
  # write the parquet and export the database
  train_df_obj$export()
  # initialize a new TrainDf object using the same recipe
  train_df_obj_repeated <- TrainDf$new(dummy_test_rec, db_con)
  # If we try the same recipe, hash check would find one row
  expect_equal(nrow(train_df_obj_repeated$queried_path), 1)
  # if hash check find one row, then export query would return NULL
  expect_true(is.null(train_df_obj_repeated$export_query))
  # `recipes::juice()` won't run, so self$df is NULL
  expect_true(is.null(train_df_obj_repeated$df))
  # and the path to the file would be the same
  expect_equal(train_df_obj_repeated$path, as.character(train_df_obj$path))
})

test_that("Check if data (no outcome) can be exported", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "test")

  # initialization of a new TrainDf object
  expect_message(TrainDf$new(dummy_test_rec, db_con),
    regexp = "No outcome added"
  )

  test_df_obj <- TrainDf$new(dummy_test_rec, db_con)
  # write the parquet and export the database
  test_df_obj$export()
  # initialize a new TrainDf object using the same recipe
  test_df_obj_repeated <- TrainDf$new(dummy_test_rec, db_con)
  # If we try the same recipe, hash check would find one row
  expect_equal(nrow(test_df_obj_repeated$queried_path), 1)
  # if hash check find one row, then export query would return NULL
  expect_true(is.null(test_df_obj_repeated$export_query))
  # `recipes::juice()` won't run, so self$df is NULL
  expect_true(is.null(test_df_obj_repeated$df))
  # and the path to the file would be the same
  expect_equal(test_df_obj_repeated$path, as.character(test_df_obj$path))
})

test_that("pandas index adding", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)
  dummy_rec <- dummy_recipe_(rpwf_sim(), type = "train")

  # initialization of a new TrainDf object
  expect_message(TrainDf$new(dummy_rec, db_con), regexp = "as pandas idx")
  expect_message(TrainDf$new(
    recipes::update_role(dummy_rec, id, new_role = "INVALID ROLE"),
    db_con
  ), regexp = "No pandas idx added")
  expect_true(TrainDf$new(
    recipes::update_role(dummy_rec, id, new_role = "INVALID ROLE"),
    db_con
  )$idx_col |> is.na())
})

# Rgrid generation --------------------------------------------------------
test_that("renaming function", {
  # Check if individual value works
  hyper_par_rename <-
    jsonlite::toJSON(list(
      "mtry" = "colsample_bytree",
      "trees" = "n_estimators",
      "min_n" = "min_child_weight",
      "tree_depth" = "max_depth",
      "learn_rate" = "learning_rate",
      "loss_reduction" = "gamma",
      "sample_size" = "subsample"
    ), auto_unbox = TRUE)

  rename_fns <- rpwf_grid_rename_(hyper_par_rename)

  expect_equal(rename_fns("mtry"), "colsample_bytree")
  expect_equal(rename_fns("trees"), "n_estimators")
  # Check if invalid values are returned as is
  expect_equal(rename_fns("dummy"), "dummy")
  # Check if a vector value are accepted
  expect_equal(
    rename_fns(c("tree_depth", "dummy")),
    c("max_depth", "dummy")
  )
})

test_that("rpwf_grid_gen_() with tune()", {
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  #### Linear model
  glm_spec <- parsnip::logistic_reg(penalty = hardhat::tune()) |>
    parsnip::set_engine("glmnet") |>
    parsnip::set_mode("classification") |>
    set_py_engine("sklearn.linear_model", "LogisticRegression",
      args = list(penalty = "elasticnet")
    )

  glm_rename <-
    jsonlite::toJSON(list(
      "penalty" = "C"
    ), auto_unbox = TRUE) |>
    rpwf_grid_rename_()

  glm_grid_1 <- rpwf_grid_gen_(
    glm_spec, dummy_test_rec, glm_rename, dials::grid_regular,
    levels = 3
  )

  glm_grid_2 <- rpwf_grid_gen_(
    glm_spec, dummy_test_rec, glm_rename, dials::grid_regular,
    update_params = list(penalty = dials::penalty(range = c(-5, 0.5))),
    levels = 3
  )

  expect_true(all(range(glm_grid_1$C) != range(glm_grid_2$C)))
})

test_that("rpwf_grid_gen_() with tune()", {
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec <- xgb_model_spec_() |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )

  hyper_par_rename <-
    jsonlite::toJSON(list(
      "mtry" = "colsample_bytree",
      "trees" = "n_estimators",
      "min_n" = "min_child_weight",
      "tree_depth" = "max_depth",
      "learn_rate" = "learning_rate",
      "loss_reduction" = "gamma",
      "sample_size" = "subsample"
    ), auto_unbox = TRUE)

  rename_fns <- rpwf_grid_rename_(hyper_par_rename)

  grid_size <- 10
  # generation of the grids
  partial_fns <- purrr::partial(rpwf_grid_gen_, dummy_mod_spec, dummy_test_rec,
    rename_fns,
    size = grid_size
  )

  c_grid_lhcube <- partial_fns(.grid_fun = dials::grid_latin_hypercube)
  c_grid_rand <- partial_fns(.grid_fun = dials::grid_random)

  expect_equal(c(nrow(c_grid_lhcube), ncol(c_grid_lhcube)), c(grid_size, 6))
  expect_equal(c(nrow(c_grid_rand), ncol(c_grid_rand)), c(grid_size, 6))
  expect_error(partial_fns(.grid_fun = NA)) # NA is not a function
  expect_message(partial_fns(.grid_fun = NULL),
    regex = "No hyper param tuning"
  )
  expect_true(is.na(partial_fns(.grid_fun = NULL)))
})

test_that("rpwf_grid_gen_() with fun from set_r_grid", {
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec <- xgb_model_spec_() |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    ) |>
    set_r_grid(dials::grid_latin_hypercube, size = 10)

  hyper_par_rename <-
    jsonlite::toJSON(list("mtry" = "colsample_bytree"), auto_unbox = TRUE)

  rename_fns <- rpwf_grid_rename_(hyper_par_rename)

  # generation of the grids
  c_grid_lhcube <- rpwf_grid_gen_(
    dummy_mod_spec, dummy_test_rec, rename_fns
  )

  expect_equal(nrow(c_grid_lhcube), 10L)
  expect_true("colsample_bytree" %in% names(c_grid_lhcube))
})


test_that("rpwf_grid_gen_() no tuning param", {
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  no_tune_spec <- xgb_model_spec_no_tune_() |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )
  grid_size <- 10
  # generation of the grids
  partial_fns <- purrr::partial(rpwf_grid_gen_, no_tune_spec, dummy_test_rec,
    size = grid_size
  )

  c_grid_lhcube <- partial_fns(.grid_fun = dials::grid_latin_hypercube)
  c_grid_rand <- partial_fns(.grid_fun = dials::grid_random)

  expect_true(is.na(c_grid_lhcube))
  expect_true(is.na(c_grid_rand))
})

test_that("rpwf_finalize_params_()", {
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec <- xgb_model_spec_() |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )
  dummy_mod_spec_no_tune_ <- xgb_model_spec_no_tune_() |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )
  expect_equal(nrow(rpwf_finalize_params_(dummy_mod_spec, dummy_test_rec)$par), 6)
  # returning no params so r_grid_gen() would return an NA
  expect_equal(nrow(rpwf_finalize_params_(
    dummy_mod_spec_no_tune_,
    dummy_test_rec
  )$par), 0)
})

# RGrid R6 class --------------------------------------------------------
test_that("set_r_grid()", {
  m <- parsnip::boost_tree() |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("classification")
  expect_true(is.null(m$.model_grid_fun))

  expect_error(set_r_grid(m, mtcars), regexp = "to be function")
  m <- m |>
    set_r_grid(dials::grid_random, size = 5, original = TRUE)

  expect_false(is.null(m$.model_grid_fun))
  expect_equal(m$.model_grid_fun, dials::grid_random)
  expect_equal(m$.model_grid_fun_args, list(size = 5, original = TRUE))
})

test_that("initialization of the RGrid class", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)

  # Generate a grid
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec <- xgb_model_spec_() |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )
  grid_size <- 10

  hyper_par_rename <-
    jsonlite::toJSON(list(
      "mtry" = "colsample_bytree",
      "trees" = "n_estimators",
      "min_n" = "min_child_weight",
      "tree_depth" = "max_depth",
      "learn_rate" = "learning_rate",
      "loss_reduction" = "gamma",
      "sample_size" = "subsample"
    ), auto_unbox = TRUE)

  rename_fns <- rpwf_grid_rename_(hyper_par_rename)

  # generation of the grids
  partial_fns <- purrr::partial(rpwf_grid_gen_, dummy_mod_spec, dummy_test_rec,
    rename_fns,
    size = grid_size
  )

  c_grid_lhcube <- partial_fns(.grid_fun = dials::grid_latin_hypercube)

  # Generate an object
  r_grid_obj <- RGrid$new(c_grid_lhcube, db_con)
  # This recipe is newly added, so the SQL query would return a 0 row data.frame
  expect_true(is.data.frame(r_grid_obj$queried_path))
  expect_equal(nrow(r_grid_obj$queried_path), 0)
  # The SQL query is 0 row, make a new export query to insert to database
  expect_true(!is.null(r_grid_obj$export_query))
})

test_that("passing NA to RGrid class", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)

  # Generate an object
  r_grid_obj <- RGrid$new(NA, db_con)
  # write the parquet and export the database

  r_grid_obj$export()

  # Check if the data is exported into the database
  expect_equal(
    DBI::dbGetQuery(
      db_con$con,
      glue::glue("SELECT grid_hash FROM r_grid_tbl WHERE grid_id = 1")
    )$grid_hash,
    r_grid_obj$hash
  )

  expect_true(is.na(r_grid_obj$path)) # path is NA because its NULL in the db
  expect_null(r_grid_obj$export_query) # since a query is found, export_q is NULL
  expect_null(r_grid_obj$df) # since a query is found, df is NULL
})

test_that("transformation of hyper param", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)

  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  grid_size <- 10

  #### XGB model
  xgb_spec <- parsnip::boost_tree(mtry = hardhat::tune()) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("classification") |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )

  xgb_rename <-
    jsonlite::toJSON(list(
      "mtry" = "colsample_bytree"
    ), auto_unbox = TRUE) |>
    rpwf_grid_rename_()

  expect_message(
    rpwf_grid_gen_(
      xgb_spec, dummy_test_rec, xgb_rename, dials::grid_random,
      size = grid_size
    ),
    regexp = "colsample"
  )

  xgb_grid <- rpwf_grid_gen_(
    xgb_spec, dummy_test_rec, xgb_rename, dials::grid_random,
    size = grid_size
  )

  expect_true(all(dplyr::between(range(xgb_grid$colsample_bytree), 0, 1)))

  #### Linear model
  glm_spec <- parsnip::logistic_reg(penalty = hardhat::tune()) |>
    parsnip::set_engine("glmnet") |>
    parsnip::set_mode("classification") |>
    set_py_engine("sklearn.linear_model", "LogisticRegression",
      args = list(penalty = "elasticnet")
    )

  glm_rename <-
    jsonlite::toJSON(list(
      "penalty" = "C"
    ), auto_unbox = TRUE) |>
    rpwf_grid_rename_()

  glm_param <- rpwf_finalize_params_(glm_spec, dummy_test_rec)
  glm_grid_1 <- dials::grid_regular(glm_param$pars, levels = 10)
  glm_grid_2 <- rpwf_transform_grid_(glm_grid_1, glm_rename, glm_param$n_predictors)
  expect_equal(glm_grid_1$penalty, 1 / (glm_grid_2$C))
})

test_that("export() method of the RGrid class", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)

  # Generate a grid
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec <- xgb_model_spec_() |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )
  grid_size <- 10

  hyper_par_rename <-
    jsonlite::toJSON(list(
      "mtry" = "colsample_bytree",
      "trees" = "n_estimators",
      "min_n" = "min_child_weight",
      "tree_depth" = "max_depth",
      "learn_rate" = "learning_rate",
      "loss_reduction" = "gamma",
      "sample_size" = "subsample"
    ), auto_unbox = TRUE)

  rename_fns <- rpwf_grid_rename_(hyper_par_rename)

  # generation of the grids
  partial_fns <- purrr::partial(rpwf_grid_gen_, dummy_mod_spec, dummy_test_rec,
    rename_fns,
    size = grid_size
  )

  c_grid_lhcube <- partial_fns(.grid_fun = dials::grid_latin_hypercube)

  # Generate an object
  r_grid_obj <- RGrid$new(c_grid_lhcube, db_con)
  # write the parquet and export the database
  r_grid_obj$export()

  # Check if the data is exported into the database
  expect_equal(
    DBI::dbGetQuery(
      db_con$con,
      glue::glue("SELECT grid_hash FROM r_grid_tbl WHERE grid_id = 2")
    )$grid_hash,
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

test_that("export() method won't add repeated rows class", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)

  # Generate a grid
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec <- xgb_model_spec_() |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )
  grid_size <- 10

  hyper_par_rename <-
    jsonlite::toJSON(list(
      "mtry" = "colsample_bytree",
      "trees" = "n_estimators",
      "min_n" = "min_child_weight",
      "tree_depth" = "max_depth",
      "learn_rate" = "learning_rate",
      "loss_reduction" = "gamma",
      "sample_size" = "subsample"
    ), auto_unbox = TRUE)

  rename_fns <- rpwf_grid_rename_(hyper_par_rename)

  # generation of the grids
  partial_fns <- purrr::partial(rpwf_grid_gen_, dummy_mod_spec, dummy_test_rec,
    rename_fns,
    size = grid_size
  )
  c_grid_lhcube <- partial_fns(.grid_fun = dials::grid_latin_hypercube)

  # Generate an object
  r_grid_obj <- RGrid$new(c_grid_lhcube, db_con)
  r_grid_obj$export()
  # initialize a new RGrid object using the same recipe
  rgrid_obj_repeated <- RGrid$new(c_grid_lhcube, db_con)
  # If we try the same recipe, hash check would find one row
  expect_equal(nrow(rgrid_obj_repeated$queried_path), 1)
  # if hash check find one row, then export query would return NULL
  expect_true(is.null(rgrid_obj_repeated$export_query))
  # `recipes::juice()` won't run, so self$df is NULL
  expect_true(is.null(rgrid_obj_repeated$df))
  # and the path to the file would be the same
  expect_equal(rgrid_obj_repeated$path, as.character(r_grid_obj$path))
})
