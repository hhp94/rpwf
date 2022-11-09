test_that("rpwf_workflow_set()", {
  dummy_test_rec <- dummy_recipe_(rpwf_sim_(), type = "train")
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
})

test_that("rpwf_tag_recipe()", {
  r <- dummy_recipe_(rpwf_sim_(), type = "train")
  expect_true(is.null(r$recipe_tag))
  r <- r |>
    rpwf_tag_recipe("test tag")
  expect_equal(r$recipe_tag, "test tag")
})

test_that("rpwf_add_model_param_()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)

  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_param_(t, db_con)
  # print(t1)
  expect_true(!all(c("py_base_learner_args", "model_mode") %in% names(t)))
  expect_true(all(c("py_base_learner_args", "model_mode") %in% names(t1)))
})

test_that("rpwf_add_desc_()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)

  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier", "my_xgboost_tag"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_param_(t, db_con)
  expect_true(all(!c("model_tag", "recipe_tag") %in% names(t1)))

  t2 <- rpwf_add_desc_(t1)
  expect_true(all(c("model_tag", "recipe_tag") %in% names(t2)))
})

test_that("rpwf_add_grid_()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)

  custom_grid <- function(x) {
    invisible(list(x))
    return(mtcars)
  }

  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_param_(t, db_con)
  t2 <- rpwf_add_desc_(t1)

  # Test if a custom grid generation function works
  t3a <- rpwf_add_grid_(t2, custom_grid, seed = 1234)
  expect_equal(t3a$grids[[1]], mtcars)

  # Test if using NULL as the grid function would generate no grids
  t3b <- rpwf_add_grid_(t2, NULL, seed = 1234)
  expect_true(is.na(t3b$grids[[1]]))

  # Test if using an appropriate grid function would generate a grid with
  # correct names renamed
  t3c <- rpwf_add_grid_(t2, dials::grid_random, seed = 1234, size = 5)
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

test_that("rpwf_augment()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)
  # Add a train df
  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )
  expect_error(rpwf_augment(t, db_con, .grid_fun = "INVALID"))
  t1 <- rpwf_augment(t, db_con, dials::grid_latin_hypercube)
  # print(t1)
  expect_equal(nrow(t1), 1)
})

test_that("rpwf_Rgrid_R6_(), rpwf_TrainDf_R6_", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)

  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_param_(t, db_con)
  t2 <- rpwf_add_desc_(t1)
  t3 <- rpwf_add_grid_(t2, dials::grid_random, seed = 1234, size = 5)
  t4 <- rpwf_Rgrid_R6_(t3, db_con) |>
    rpwf_TrainDf_R6_(db_con)
  expect_true("Rgrid" %in% names(t4))
  expect_true("TrainDf" %in% names(t4))
})

test_that("rpwf_write_grid()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)

  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  )

  t1 <- rpwf_add_model_param_(t, db_con)
  t2 <- rpwf_add_desc_(t1)
  t3 <- rpwf_add_grid_(t2, dials::grid_random, seed = 1234, size = 5)
  t4 <- rpwf_Rgrid_R6_(t3, db_con) |>
    rpwf_TrainDf_R6_(db_con)

  # Test if writing NA grid doesn't export anything
  rpwf_write_grid(rpwf_Rgrid_R6_(rpwf_add_grid_(t2, NULL, seed = 1234), db_con))
  expect_true("db.SQLite_grid" %in% (list.files(paste(tmp_dir, "rpwfDb", sep = "/"))))

  # Write an actual grid
  rpwf_write_grid(t4)
  expect_equal(length(list.files(
    paste(tmp_dir, "rpwfDb", "db.SQLite_grid", sep = "/")
  )), 1)
})

test_that("rpwf_write_df()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)

  tmp_func <- function(obj) {
    obj |>
      rpwf_add_model_param_(db_con) |>
      rpwf_add_desc_() |>
      rpwf_add_grid_(dials::grid_random, seed = 1234, size = 5)
  }
  # Add a train df
  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    tmp_func()
  # Add a test df
  t1 <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "test")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    tmp_func() |>
    rpwf_TrainDf_R6_(db_con)

  rpwf_write_df(t1, 1234)
  expect_true("db.SQLite_df" %in% list.files(paste(tmp_dir, "rpwfDb", sep = "/")))
  expect_equal(length(list.files(paste(tmp_dir, "rpwfDb", "db.SQLite_df", sep = "/"))), 1)
})

test_that("rpwf_add_random_state_()", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)
  # Add a train df
  t <- rpwf_workflow_set(
    list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
    list(set_py_engine(
      xgb_model_spec_(),
      "xgboost", "XGBClassifier"
    )),
    list("neg_log_loss")
  ) |>
    rpwf_add_model_param_(db_con) |>
    rpwf_add_desc_() |>
    rpwf_add_grid_(dials::grid_random, seed = 1234, size = 5) |>
    rpwf_add_py_model_(db_con)

  # print(t)
  t1a <- rpwf_add_random_state_(t, range = c(1, 5000), seed = 1234)
  expect_true(is.numeric(t1a$random_state))
  # Check if seeding works
  t1b <- rpwf_add_random_state_(t, range = c(1, 5000), seed = 1234)
  expect_equal(t1a$random_state, t1b$random_state)
})

test_that("rpwf_Rgrid_R6/TrainDf_R6_id_() part 1", {
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
  ) |>
    rpwf_augment(db_con, dials::grid_latin_hypercube)

  rpwf_write_grid(t)
  expect_error(rpwf_TrainDf_R6_id_(t, db_con),
    regexp = "df"
  )
})

test_that("rpwf_Rgrid_R6/TrainDf_R6_id_() part 2", {
  tmp_dir <- withr::local_tempdir(pattern = "rpwfDb")
  db_con <- rpwf_connect_db("db.SQLite", tmp_dir)

  query_wflow_tbl <- function() {
    DBI::dbGetQuery(db_con$con, "SELECT * FROM wflow_tbl")
  }
  # Add a train df
  t <- rpwf_workflow_set(list(xgb = dummy_recipe_(rpwf_sim_(), type = "train")),
                         list(
                           set_py_engine(xgb_model_spec_(),
                                         "xgboost", "XGBClassifier")
                         ),
                         list("neg_log_loss")) |>
    rpwf_augment(db_con, dials::grid_latin_hypercube)

  rpwf_write_df(t)
  expect_error(rpwf_Rgrid_R6_id_(t, db_con),
               regexp = "grid")

  rpwf_write_grid(t)
  expect_true(all(c("grid_id", "df_id") %in% names(
    rpwf_TrainDf_R6_id_(rpwf_Rgrid_R6_id_(t, db_con), db_con)
  )))
})

test_that("rpwf_export_db()", {
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
  ) |>
    rpwf_augment(db_con, dials::grid_latin_hypercube)

  rpwf_write_grid(t)
  rpwf_write_df(t)

  before <- query_wflow_tbl()

  expect_equal(nrow(before), 0) # before export, there would be no wflow
  rpwf_export_db(t, db_con)
  after <- query_wflow_tbl() # after export, there would be 1 wflow
  expect_equal(nrow(after), 1)
  rpwf_export_db(t, db_con)
  after1 <- query_wflow_tbl() # exporting the same wflow would not work
  expect_equal(nrow(after1), 1)
})
