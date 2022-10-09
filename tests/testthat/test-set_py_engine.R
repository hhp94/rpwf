test_that("test if the python codes folder can be moved to the root path", {
  tmp_dir <- withr::local_tempdir()
  db_con <- dummy_con_(tmp_dir)

  rpwf_cp_py_codes(db_con$proj_root_path)
  to_folder <- paste(db_con$proj_root_path, "rpwf", sep = "/")
  expect_true(dir.exists(to_folder))
  copied_files <- sort(list.files(to_folder))
  expect_equal(copied_files, c("rpwf", "setup.cfg", "setup.py"))
})

test_that("rpwf_chk_model_avail()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)
  expect_message(rpwf_chk_model_avail(
    db_con$con,
    "xgboost", "XGBClassifier", "xgboost"
  ),
  regexp = "Model found in db"
  )
  expect_error(rpwf_chk_model_avail(
    db_con$con,
    "INVALID", "XGBClassifier", "xgboost"
  ),
  regexp = "Invalid py model selection"
  )
  expect_error(rpwf_chk_model_avail(
    db_con$con,
    "xgboost", "INVALID", "xgboost"
  ),
  regexp = "Invalid py model selection"
  )
  expect_error(rpwf_chk_model_avail(
    db_con$con,
    "xgboost", "XGBClassifier", "INVALID"
  ),
  regexp = "Invalid py model selection"
  )
})

test_that("set_py_engine() added py_base_learner attributes", {
  mod_spec <- xgb_model_spec_()
  modified_spec <- mod_spec |>
    set_py_engine("xgboost", "XGBClassifier")
  expect_equal(modified_spec$py_module, "xgboost")
  expect_equal(modified_spec$py_base_learner, "XGBClassifier")
})

test_that("set_py_engine() added py_base_learner_args attributes", {
  mod_spec <- xgb_model_spec_()
  modified_spec <- mod_spec |>
    set_py_engine("xgboost", "XGBClassifier",
      args = list(eval_metric = "logloss", silent = TRUE)
    )
  expect_type(modified_spec$py_base_learner_args, "character")
  expect_equal(
    jsonlite::parse_json(modified_spec$py_base_learner_args),
    list(eval_metric = "logloss", silent = TRUE)
  )
})

test_that("set_py_engine() check is working", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  mod_spec <- xgb_model_spec_()
  expect_error(mod_spec |>
    set_py_engine("lightgbm", "INVALID", db_con$con))
  expect_error(mod_spec |>
    set_py_engine("INVALID", "LGBMClassifier", db_con$con))
  expect_error(mod_spec |>
    set_py_engine("lightgbm", "LGBMClassifier", db_con$con))
  expect_message(mod_spec |>
    set_py_engine("xgboost", "XGBClassifier", db_con$con),
  regex = "Model found in db"
  )
})
