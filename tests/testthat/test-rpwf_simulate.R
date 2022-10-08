test_that("dummy train data generation and dummy recipe codes works", {
  dummy_train_rec = dummy_recipe_(rpwf_sim())
  # print(dummy_train_rec)
  expect_true("recipe" %in% class(dummy_train_rec))
})

test_that("dummy test data generation works", {
  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "test")
  # print(dummy_test_rec)
  expect_true("recipe" %in% class(dummy_test_rec))
})

test_that("xgb_model_spec_ works", {
  mod_spec = xgb_model_spec_()
  # print(mod_spec)
  # print(names(mod_spec))
  expect_true("model_spec" %in% class(mod_spec))
})

test_that("dummy_con_()", {
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  db_obj = dummy_con_(tmp_dir)
  con = db_obj$con
  expect_equal(sort(DBI::dbListTables(db_obj$con)), sort(names(rpwf_schema())))
  expect_true({rm(db_obj); gc(); !DBI::dbIsValid(con)})
})

