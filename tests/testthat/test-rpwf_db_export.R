test_that("rpwf_workflow_set()", {
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec <- xgb_model_spec_() |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )
  expect_equal(
    nrow(rpwf_workflow_set(
      list(dummy_test_rec, dummy_test_rec),
      list(dummy_mod_spec, dummy_mod_spec),
      list("neg_log_loss")
    )),
    1L
  )
})

test_that("rpwf_add_model_info()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  t <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_info(t, db_con$con)
  # print(t1)
  expect_true(!all(c("py_base_learner_args", "model_mode") %in% names(t)))
  expect_true(all(c("py_base_learner_args", "model_mode") %in% names(t1)))
})

test_that("rpwf_add_desc()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  t <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_info(t, db_con$con)
  t2 <- rpwf_add_desc(t1)
  expect_true(!"wflow_desc" %in% names(t1))
  expect_true("wflow_desc" %in% names(t2))
})

test_that("rpwf_add_grid_param()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  custom_grid <- function(x) {
    invisible(list(x))
    return(mtcars)
  }
  t <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_info(t, db_con$con)
  t2 <- rpwf_add_desc(t1)
  t3a <- rpwf_add_grid_param(t2, custom_grid, seed = 1234)
  expect_equal(t3a$grids[[1]], mtcars)

  t3b <- rpwf_add_grid_param(t2, NULL, seed = 1234)
  expect_true(is.na(t3b$grids[[1]]))
})

test_that("rpwf_add_grids()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  t <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_info(t, db_con$con)
  t2 <- rpwf_add_desc(t1)
  t3 <- rpwf_add_grid_param(t2, dials::grid_random, seed = 1234, size = 5)
  t4a <- rpwf_add_grids(t3, db_con)
  t4b <- rpwf_add_grids(
    rpwf_add_grid_param(t2, NULL, seed = 1234), # Not use grid
    db_con
  )
  expect_equal(t4a$grid_id, 2)
  expect_equal(t4b$grid_id, 1)
  expect_message(rpwf_add_grids(
    rpwf_add_grid_param(t2, NULL, seed = 1234), db_con
  ), # Not use grid
  regex = "No hyper param tuning"
  )
})

test_that("rpwf_add_dfs()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  tmp_func <- function(obj) {
    obj |>
      rpwf_add_model_info(db_con$con) |>
      rpwf_add_desc() |>
      rpwf_add_grid_param(dials::grid_random, seed = 1234, size = 5) |>
      rpwf_add_grids(db_con) |>
      rpwf_add_dfs(db_con, 1234)
  }
  # Add a train df
  t <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    tmp_func()
  # Add a test df
  t1 <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "test")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    tmp_func()

  expect_equal(t$df_id, 1)
  expect_equal(t1$df_id, 2)
})

test_that("rpwf_add_cost()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)
  # Add a train df
  t <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    rpwf_add_model_info(db_con$con) |>
    rpwf_add_desc() |>
    rpwf_add_grid_param(dials::grid_random, seed = 1234, size = 5) |>
    rpwf_add_grids(db_con) |>
    rpwf_add_dfs(db_con, 1234)

  t1a <- rpwf_add_cost(t, db_con$con)
  expect_equal(t1a$cost_id, 2)
})

test_that("rpwf_add_model_type()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)
  # Add a train df
  t <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    rpwf_add_model_info(db_con$con) |>
    rpwf_add_desc() |>
    rpwf_add_grid_param(dials::grid_random, seed = 1234, size = 5) |>
    rpwf_add_grids(db_con) |>
    rpwf_add_dfs(db_con, 1234) |>
    rpwf_add_cost(db_con$con)

  # print(t)
  t1a <- rpwf_add_model_type(t, db_con$con)
  expect_equal(t1a$model_type_id, 1)
})

test_that("rpwf_add_random_state()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)
  # Add a train df
  t <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    rpwf_add_model_info(db_con$con) |>
    rpwf_add_desc() |>
    rpwf_add_grid_param(dials::grid_random, seed = 1234, size = 5) |>
    rpwf_add_grids(db_con) |>
    rpwf_add_dfs(db_con, 1234) |>
    rpwf_add_cost(db_con$con) |>
    rpwf_add_model_type(db_con$con)

  # print(t)
  t1a <- rpwf_add_random_state(t, range = c(1, 5000), seed = 1234)
  expect_true(is.numeric(t1a$random_state))
  # Check if seeding works
  t1b <- rpwf_add_random_state(t, range = c(1, 5000), seed = 1234)
  expect_equal(t1a$random_state, t1b$random_state)
})

test_that("rpwf_add_all()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)
  # Add a train df
  t <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )
  expect_error(rpwf_add_all(t, db_con, .grid_fun = "INVALID"))
  t1 <- rpwf_add_all(t, db_con, dials::grid_latin_hypercube)
  expect_equal(nrow(t1), 1)
})

test_that("rpwf_export_db()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  query_wflow_tbl <- function() {
    DBI::dbGetQuery(db_con$con, "SELECT * FROM wflow_tbl")
  }
  # Add a train df
  t <- rpwf_workflow_set(
    list(dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )
  t1 <- rpwf_add_all(t, db_con, dials::grid_latin_hypercube)

  before <- query_wflow_tbl()
  expect_equal(nrow(before), 0) # before export, there would be no wflow
  rpwf_export_db(t1, db_con$con)
  after <- query_wflow_tbl() # after export, there would be 1 wflow
  expect_equal(nrow(after), 1)
  rpwf_export_db(t1, db_con$con)
  after1 <- query_wflow_tbl() # exporting the same wflow would not work
  expect_equal(nrow(after1), 1)
})
