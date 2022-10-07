test_that("test if the python codes folder can be moved to the root path", {
  tmp_dir = withr::local_tempdir()
  rpwf_cp_py_codes(tmp_dir)
  python_codes = paste(tmp_dir, "rpwf", sep = "/")
  expect_true(dir.exists(python_codes))
  copied_files = sort(list.files(python_codes))
  expect_equal(copied_files, c("rpwf", "setup.cfg", "setup.py"))
})

test_that("set_py_engine added py_base_learner attributes", {
  mod_spec = xgb_model_spec_()
  modified_spec = mod_spec |>
    set_py_engine("XGBClassifier")
  expect_equal(modified_spec$py_base_learner, "XGBClassifier")
})

test_that("set_py_engine added py_base_learner_args attributes", {
  mod_spec = xgb_model_spec_()
  modified_spec = mod_spec |>
    set_py_engine("XGBClassifier",
                  args = list(eval_metric = "logloss", silent = TRUE))
  expect_type(modified_spec$py_base_learner_args, "character")
  expect_equal(jsonlite::parse_json(modified_spec$py_base_learner_args),
               list(eval_metric = "logloss", silent = TRUE))
})

test_that("set_py_engine check is working", {
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  mod_spec = xgb_model_spec_()
  expect_error(mod_spec |>
                 set_py_engine("INVALID", con = con))
  DBI::dbDisconnect(con)
})
