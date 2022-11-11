test_that("id_col_switch_()", {
  expect_equal(id_col_switch_("df_tbl"), "df_id")
  expect_error(id_col_switch_("wflow_tbl"))
})

test_that("rpwf_db_del_wflow()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)

  query_wflow_tbl <- function() {
    DBI::dbGetQuery(db_con$con, "SELECT * FROM wflow_tbl")
  }
  # Add a train df
  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )
  t1 <- rpwf_augment(t, db_con, dials::grid_latin_hypercube)

  rpwf_write_grid(t1)
  rpwf_write_df(t1)
  expect_equal(length(list.files(paste(tmp_dir, "rpwfDb", sep = "/"), recursive = TRUE)), 3)
  rpwf_export_db(t1, db_con)
  expect_equal(nrow(query_wflow_tbl()), 1)

  # delete from wflow
  rpwf_db_del_wflow(1, db_con, TRUE)
  expect_equal(nrow(query_wflow_tbl()), 0)
  expect_equal(length(list.files(paste(tmp_dir, "rpwfDb", sep = "/"), recursive = TRUE)), 1)
})

test_that("rpwf_db_del_entry()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)

  query_wflow_tbl <- function(tbl) {
    DBI::dbGetQuery(db_con$con, glue::glue("SELECT * FROM {tbl}"))
  }
  # Add a train df
  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )
  t1 <- rpwf_augment(t, db_con, dials::grid_latin_hypercube)
  rpwf_write_grid(t1)
  rpwf_write_df(t1)
  rpwf_export_db(t1, db_con)
  expect_equal(nrow(query_wflow_tbl("wflow_tbl")), 1)

  # delete from wflow would not work
  expect_error(rpwf_db_del_entry("wflow_tbl", 1, db_con),
    regex = "instead"
  )

  # delete from random tables
  expect_true(nrow(query_wflow_tbl("model_type_tbl")) > 0)
  rpwf_db_del_entry("model_type_tbl", 1:99, db_con)
})

test_that("rpwf_avail_models()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)

  models <- rpwf_avail_models(db_con)

  expect_equal(nrow(models), nrow(sup_mod_df__))
})

test_that("rpwf_results()", {
  expect_message(rpwf_connect_db("db.SQLite", test_path("fixtures")),
    regexp = "found"
  )
  db_con <- rpwf_connect_db("db.SQLite", test_path("fixtures"))
  results_df <- rpwf_results(db_con)
  expect_true(nrow(results_df) > 0L)
})
