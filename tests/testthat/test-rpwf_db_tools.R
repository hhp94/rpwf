test_that("id_col_switch_()", {
  expect_equal(id_col_switch_("cost_tbl"), "cost_id")
  expect_error(id_col_switch_("wflow_tbl"))
})

test_that("rpwf_db_del_wflow()", {
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
  rpwf_export_db(t1, db_con$con)
  expect_equal(nrow(query_wflow_tbl()), 1)

  # delete from wflow
  rpwf_db_del_wflow(1, db_con$con)
  expect_equal(nrow(query_wflow_tbl()), 0)
})

test_that("rpwf_db_del_entry()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- dummy_con_(tmp_dir = tmp_dir)

  query_wflow_tbl <- function(tbl) {
    DBI::dbGetQuery(db_con$con, glue::glue("SELECT * FROM {tbl}"))
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
  rpwf_export_db(t1, db_con$con)
  expect_equal(nrow(query_wflow_tbl("wflow_tbl")), 1)

  # delete from wflow would not work
  expect_error(rpwf_db_del_entry("wflow_tbl", 1, db_con$con),
    regex = "instead"
  )

  # delete from random tables
  expect_true(all(c(
    nrow(query_wflow_tbl("cost_tbl")) > 0,
    nrow(query_wflow_tbl("model_type_tbl")) > 0
  )))
  rpwf_db_del_entry(c("cost_tbl", "model_type_tbl"), 1:99, db_con$con)
})
