test_that("test if the python codes folder can be moved to the root path", {
  tmp_dir <- withr::local_tempdir()
  db_con <- dummy_con_(tmp_dir)

  rpwf_cp_py_codes(db_con$proj_root_path)
  to_folder <- paste(db_con$proj_root_path, "rpwf", sep = "/")
  expect_true(dir.exists(to_folder))
  copied_files <- sort(list.files(to_folder))
  expect_equal(copied_files, c("rpwf", "setup.cfg", "setup.py"))
})

test_that("test the overwrite function of rpwf_cp_py_codes()", {
  tmp_dir <- withr::local_tempdir()
  db_con <- dummy_con_(tmp_dir)

  rpwf_cp_py_codes(db_con$proj_root_path)
  to_folder <- paste(db_con$proj_root_path, "rpwf", sep = "/")
  test_delete <- glue::glue("{to_folder}/setup.py")
  expect_true(file.exists(test_delete))
  unlink(test_delete) # Delete a file
  expect_true(!file.exists(test_delete)) # Make sure file doesnt exists
  rpwf_cp_py_codes(db_con$proj_root_path, FALSE) # Overwrite is false
  expect_true(!file.exists(test_delete)) # File would not exists
  rpwf_cp_py_codes(db_con$proj_root_path, TRUE) # Overwrite is true
  expect_true(file.exists(test_delete)) # New file copied over
})

test_that("rpwf_chk_model_avail()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir)
  expect_message(rpwf_chk_model_avail(
    db_con$con,
    "xgboost", "XGBClassifier", "xgboost"
  ),
  regexp = "Valid scikit-learn model"
  )
  expect_error(rpwf_chk_model_avail(
    db_con$con,
    "INVALID", "XGBClassifier", "xgboost"
  ),
  regexp = "Invalid scikit-learn model"
  )
  expect_error(rpwf_chk_model_avail(
    db_con$con,
    "xgboost", "INVALID", "xgboost"
  ),
  regexp = "Invalid scikit-learn model"
  )
  expect_error(rpwf_chk_model_avail(
    db_con$con,
    "xgboost", "XGBClassifier", "INVALID"
  ),
  regexp = "Invalid scikit-learn model"
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
    set_py_engine("lightgbm", db_con$con),
  regexp = "need to be of type character"
  )
  expect_error(mod_spec |>
    set_py_engine("lightgbm", "INVALID", con = db_con$con))
  expect_error(mod_spec |>
    set_py_engine("INVALID", "LGBMClassifier", con = db_con$con))
  expect_error(mod_spec |>
    set_py_engine("lightgbm", "LGBMClassifier", con = db_con$con))
  expect_message(mod_spec |>
    set_py_engine("xgboost", "XGBClassifier", con = db_con$con),
  regexp = "Valid scikit-learn model"
  )
})
