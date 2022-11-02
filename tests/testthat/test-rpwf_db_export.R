test_that("rpwf_workflow_set()", {
  dummy_test_rec <- dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec <- xgb_model_spec_() |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )
  expect_equal(
    suppressWarnings(nrow(rpwf_workflow_set(
      list(dummy_test_rec, dummy_test_rec),
      list(dummy_mod_spec, dummy_mod_spec),
      list("neg_log_loss")
    ))),
    1L
  )

  # miss specifying preproc
  expect_error(rpwf_workflow_set(
    list(dummy_mod_spec),
    list(dummy_test_rec),
    "neg_log_loss"
  ), regexp = "preproc")

  # miss specifying models
  expect_error(rpwf_workflow_set(
    list(dummy_test_rec),
    list(dummy_test_rec),
    "neg_log_loss"
  ), regexp = "models")

  # mixing up preproc
  expect_error(rpwf_workflow_set(
    list(dummy_test_rec, dummy_mod_spec),
    list(dummy_mod_spec),
    "neg_log_loss"
  ), regexp = "preproc")

  # mixing up models
  expect_error(rpwf_workflow_set(
    list(dummy_test_rec),
    list(dummy_mod_spec, dummy_test_rec),
    "neg_log_loss"
  ), regexp = "models")

  # warning for duplicated preproc
  expect_warning(rpwf_workflow_set(
    list(dummy_test_rec, dummy_test_rec),
    list(dummy_mod_spec),
    list("neg_log_loss")
  ), regexp = "Duplicated preprocs")

  names(dummy_test_rec) <- NULL
  # warning for name less preproc
  expect_warning(rpwf_workflow_set(
    list(dummy_test_rec),
    list(dummy_mod_spec),
    list("neg_log_loss")
  ), regexp = "named list")
})

test_that("rpwf_add_model_info_()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_info_(t, db_con$con)
  # print(t1)
  expect_true(!all(c("py_base_learner_args", "model_mode") %in% names(t)))
  expect_true(all(c("py_base_learner_args", "model_mode") %in% names(t1)))
})

test_that("rpwf_add_desc_()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier", "my_xgboost_tag"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_info_(t, db_con$con)
  expect_true("tag" %in% names(t1))
  expect_true(!"wflow_desc" %in% names(t1))
  t2 <- rpwf_add_desc_(t1)

  expect_true("wflow_desc" %in% names(t2))
})

test_that("rpwf_add_grid_param_()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  custom_grid <- function(x) {
    invisible(list(x))
    return(mtcars)
  }
  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_info_(t, db_con$con)
  t2 <- rpwf_add_desc_(t1)

  # Test if a custom grid generation function works
  t3a <- rpwf_add_grid_param_(t2, custom_grid, seed = 1234)
  expect_equal(t3a$grids[[1]], mtcars)

  # Test if using NULL as the grid function would generate no grids
  t3b <- rpwf_add_grid_param_(t2, NULL, seed = 1234)
  expect_true(is.na(t3b$grids[[1]]))

  # Test if using an appropriate grid function would generate a grid with
  # correct names renamed
  t3c <- rpwf_add_grid_param_(t2, dials::grid_random, seed = 1234, size = 5)
  renamed_cols <- c(
    "colsample_bytree",
    "min_child_weight",
    "max_depth",
    "learning_rate",
    "gamma",
    "subsample"
  )
  expect_true(all(sort(names(t3c$grids[[1]])) == sort(renamed_cols)))
})

test_that("rpwf_export_grid()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_info_(t, db_con$con)
  t2 <- rpwf_add_desc_(t1)
  t3 <- rpwf_add_grid_param_(t2, dials::grid_random, seed = 1234, size = 5)
  t4a <- rpwf_export_grid(t3, db_con)
  t4b <- rpwf_export_grid(
    rpwf_add_grid_param_(t2, NULL, seed = 1234), # Not use grid
    db_con
  )
  expect_equal(t4a$grid_id, 2)
  expect_equal(t4b$grid_id, 1)
  expect_message(
    rpwf_export_grid(
      rpwf_add_grid_param_(t2, NULL, seed = 1234), db_con
    ), # Not use grid
    regex = "No hyper param tuning"
  )
})

test_that("rpwf_export_df()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  tmp_func <- function(obj) {
    obj |>
      rpwf_add_model_info_(db_con$con) |>
      rpwf_add_desc_() |>
      rpwf_add_grid_param_(dials::grid_random, seed = 1234, size = 5) |>
      rpwf_export_grid(db_con) |>
      rpwf_export_df(db_con, 1234)
  }
  # Add a train df
  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    tmp_func()
  # Add a test df
  t1 <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim(), type = "test")),
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

test_that("rpwf_add_model_type_()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)
  # Add a train df
  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    rpwf_add_model_info_(db_con$con) |>
    rpwf_add_desc_() |>
    rpwf_add_grid_param_(dials::grid_random, seed = 1234, size = 5) |>
    rpwf_export_grid(db_con) |>
    rpwf_export_df(db_con, 1234)

  # print(t)
  t1a <- rpwf_add_model_type_(t, db_con$con)
  expect_equal(t1a$model_type_id, 1)
})

test_that("rpwf_add_random_state_()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)
  # Add a train df
  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    rpwf_add_model_info_(db_con$con) |>
    rpwf_add_desc_() |>
    rpwf_add_grid_param_(dials::grid_random, seed = 1234, size = 5) |>
    rpwf_export_grid(db_con) |>
    rpwf_export_df(db_con, 1234) |>
    rpwf_add_model_type_(db_con$con)

  # print(t)
  t1a <- rpwf_add_random_state_(t, range = c(1, 5000), seed = 1234)
  expect_true(is.numeric(t1a$random_state))
  # Check if seeding works
  t1b <- rpwf_add_random_state_(t, range = c(1, 5000), seed = 1234)
  expect_equal(t1a$random_state, t1b$random_state)
})

test_that("rpwf_augment()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)
  # Add a train df
  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )
  expect_error(rpwf_augment(t, db_con, .grid_fun = "INVALID"))
  t1 <- rpwf_augment(t, db_con, dials::grid_latin_hypercube)
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
    list(xgb = dummy_recipe_(rpwf_sim(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    rpwf_augment(db_con, dials::grid_latin_hypercube)

  t1 <- t |>
    rpwf_export_grid(db_con) |>
    rpwf_export_df(db_con, 1234)

  before <- query_wflow_tbl()

  expect_equal(nrow(before), 0) # before export, there would be no wflow
  rpwf_export_db(t1, db_con$con)
  # after <- query_wflow_tbl() # after export, there would be 1 wflow
  # expect_equal(nrow(after), 1)
  # rpwf_export_db(t1, db_con$con)
  # after1 <- query_wflow_tbl() # exporting the same wflow would not work
  # expect_equal(nrow(after1), 1)
  #
  # # Check if rwpf_export_grid or export_df hasn't been run
  # expect_error(rpwf_export_db(rpwf_export_grid(t, db_con), db_con$con),
  #   regexp = "to write parquet files first"
  # )
  # expect_error(rpwf_export_db(rpwf_export_df(t, db_con, 1234), db_con$con),
  #   regexp = "to write parquet files first"
  # )
})
