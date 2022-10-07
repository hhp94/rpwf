test_that("rpwf_workflow_set()", {
  dummy_test_rec = dummy_recipe_(rpwf_sim(), type = "train")
  dummy_mod_spec = xgb_model_spec_() |>
    set_py_engine("XGBClassifier",
                  args = list(eval_metric = "logloss", silent = TRUE))
  expect_equal(
    nrow(rpwf_workflow_set(
        list(dummy_test_rec, dummy_test_rec),
        list(dummy_mod_spec, dummy_mod_spec),
        list("neg_log_loss"))),
  1L)
})

test_that("rpwf_add_model_info()", {
  t = rpwf_workflow_set(list(dummy_recipe_(rpwf_sim(), type = "train")),
                        list(set_py_engine(xgb_model_spec_(), "XGBClassifier")),
                        list("neg_log_loss"))

  t1 = rpwf_add_model_info(t)
  expect_true(!all(c("py_base_learner_args", "model_mode") %in% names(t)))
  expect_true(all(c("py_base_learner_args", "model_mode") %in% names(t1)))
})

test_that("rpwf_add_desc()", {
  t = rpwf_workflow_set(list(dummy_recipe_(rpwf_sim(), type = "train")),
                        list(set_py_engine(xgb_model_spec_(), "XGBClassifier")),
                        list("neg_log_loss"))

  t1 = rpwf_add_model_info(t)
  t2 = rpwf_add_desc(t1)
  expect_true(!"wflow_desc" %in% names(t1))
  expect_true("wflow_desc" %in% names(t2))
})

test_that("rpwf_add_grid_param()", {
  t = rpwf_workflow_set(list(dummy_recipe_(rpwf_sim(), type = "train")),
                        list(set_py_engine(xgb_model_spec_(), "XGBClassifier")),
                        list("neg_log_loss"))

  custom_grid = function(x){invisible(list(x)); return(mtcars)}

  t1 = rpwf_add_model_info(t)
  t2 = rpwf_add_desc(t1)
  t3a = rpwf_add_grid_param(t2, custom_grid, seed = 1234)
  expect_equal(t3a$grids[[1]], mtcars)

  t3b = rpwf_add_grid_param(t2, NULL, seed = 1234)
  expect_true(is.na(t3b$grids[[1]]))
})

test_that("rpwf_add_grids()", {
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  t = rpwf_workflow_set(list(dummy_recipe_(rpwf_sim(), type = "train")),
                        list(set_py_engine(xgb_model_spec_(), "XGBClassifier")),
                        list("neg_log_loss"))

  t1 = rpwf_add_model_info(t)
  t2 = rpwf_add_desc(t1)
  t3 = rpwf_add_grid_param(t2, dials::grid_random, seed = 1234, size = 5)
  t4a = rpwf_add_grids(t3, con, tmp_dir)
  t4b = rpwf_add_grids(rpwf_add_grid_param(t2, NULL, seed = 1234), #Not use grid
                       con, tmp_dir)
  expect_equal(t4a$grid_id, 2)
  expect_equal(t4b$grid_id, 1)
  expect_message(rpwf_add_grids(
    rpwf_add_grid_param(t2, NULL, seed = 1234), #Not use grid
    con, tmp_dir), regex = "No tuning is assumed")
})

test_that("rpwf_add_dfs()", {
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)

  tmp_func = function(obj){
    obj |>
      rpwf_add_model_info() |>
      rpwf_add_desc() |>
      rpwf_add_grid_param(dials::grid_random, seed = 1234, size = 5) |>
      rpwf_add_grids(con, tmp_dir) |>
      rpwf_add_dfs(con, tmp_dir, 1234)
  }
  # Add a train df
  t = rpwf_workflow_set(list(dummy_recipe_(rpwf_sim(), type = "train")),
                        list(set_py_engine(xgb_model_spec_(), "XGBClassifier")),
                        list("neg_log_loss")) |>
    tmp_func()
  # Add a test df
  t1 = rpwf_workflow_set(list(dummy_recipe_(rpwf_sim(), type = "test")),
                        list(set_py_engine(xgb_model_spec_(), "XGBClassifier")),
                        list("neg_log_loss")) |>
    tmp_func()

  expect_equal(t$df_id, 1)
  expect_equal(t1$df_id, 2)
})

test_that("rpwf_add_cost()", {
  tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
  con = dummy_con_(tmp_dir = tmp_dir)
  # Add a train df
  t = rpwf_workflow_set(list(dummy_recipe_(rpwf_sim(), type = "train")),
                        list(set_py_engine(xgb_model_spec_(), "XGBClassifier")),
                        list("neg_log_loss")) |>
    rpwf_add_model_info() |>
    rpwf_add_desc() |>
    rpwf_add_grid_param(dials::grid_random, seed = 1234, size = 5) |>
    rpwf_add_grids(con, tmp_dir) |>
    rpwf_add_dfs(con, tmp_dir, 1234)

  t1a = rpwf_add_cost(t, con)
  expect_equal(t1a$cost_id, 2)
})
