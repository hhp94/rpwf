test_that("test if the python codes folder can be moved to the root path", {
  tmp_dir <- withr::local_tempdir()

  rpwf_cp_py_codes(tmp_dir)
  to_folder <- paste(tmp_dir, "rpwf", sep = "/")
  expect_true(dir.exists(to_folder))
  copied_files <- sort(list.files(to_folder))
  expect_equal(copied_files, c("rpwf", "setup.cfg", "setup.py"))
})

test_that("test the overwrite function of rpwf_cp_py_codes()", {
  tmp_dir <- withr::local_tempdir()

  rpwf_cp_py_codes(tmp_dir)
  to_folder <- paste(tmp_dir, "rpwf", sep = "/")
  test_delete <- glue::glue("{to_folder}/setup.py")
  expect_true(file.exists(test_delete))
  unlink(test_delete) # Delete a file
  expect_true(!file.exists(test_delete)) # Make sure file doesnt exists
  rpwf_cp_py_codes(tmp_dir, FALSE) # Overwrite is false
  expect_true(!file.exists(test_delete)) # File would not exists
  rpwf_cp_py_codes(tmp_dir, TRUE) # Overwrite is true
  expect_true(file.exists(test_delete)) # New file copied over
})

test_that("rpwf_chk_model_avail_()", {
  board <- pins::board_temp()
  tmp_dir <- withr::local_tempdir()
  db_name <- paste(tmp_dir, "db.SQLite", sep = "/")
  db_con <- rpwf_connect_db(db_name, board)

  expect_invisible(
    rpwf_chk_model_avail_(
      db_con,
      "xgboost", "XGBClassifier", "xgboost"
    )
  )
  expect_error(
    rpwf_chk_model_avail_(
      db_con,
      "INVALID", "XGBClassifier", "xgboost"
    ),
    regexp = "Invalid scikit-learn model"
  )
  expect_error(
    rpwf_chk_model_avail_(
      db_con,
      "xgboost", "INVALID", "xgboost"
    ),
    regexp = "Invalid scikit-learn model"
  )
  expect_error(
    rpwf_chk_model_avail_(
      db_con,
      "xgboost", "XGBClassifier", "INVALID"
    ),
    regexp = "Invalid scikit-learn model"
  )
})

test_that("set_py_engine() added py_base_learner attributes", {
  mod_spec <- xgb_model_spec_()
  expect_error(
    set_py_engine(mod_spec, "xgboost", "XGBClassifier",
      eval_metric = "a", eval_metric = "b"
    ),
    regexp = "unique"
  )
  expect_error(
    set_py_engine(mod_spec, "xgboost", "XGBClassifier",
      tag = "a", "b", "c"
    ),
    regexp = "must be named"
  )
})

test_that("set_py_engine() named list check", {
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
      eval_metric = "logloss", silent = TRUE
    )
  expect_type(modified_spec$py_base_learner_args, "character")
  expect_equal(
    jsonlite::parse_json(modified_spec$py_base_learner_args),
    list(eval_metric = "logloss", silent = TRUE)
  )
})

test_that("set_py_engine() tag works", {
  mod_spec <- xgb_model_spec_()
  py_mod_spec <- xgb_model_spec_() |>
    set_py_engine("xgboost", "XGBClassifier", "my_xgboost_model",
      eval_metric = "logloss", silent = TRUE
    )
  expect_equal(py_mod_spec$model_tag, "my_xgboost_model")
})
