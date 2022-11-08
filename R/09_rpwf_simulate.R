#' Generate the Data Needed for Testing
#'
#' Generate dummy data for testing of the `rpwf` package.
#'
#' @param n_train number of train rows.
#' @param n_test number of test rows.
#' @param seed random seed.
#'
#' @return a list of train and test df and the id and target column names.
#'
#' @noRd
#'
#' @examples
#' sim_dat <- rpwf_sim_()
#' sim_dat$train
#' sim_dat$test
rpwf_sim_ <- function(n_train = 100, n_test = 10, seed = 1234) {
  set.seed(seed)
  n <- n_train + n_test
  df <- data.frame(
    X1 = stats::rnorm(n),
    X2 = stats::rnorm(n),
    X3 = factor(rep(1:4, length.out = n)),
    target = stats::rbinom(n, size = 1, prob = 0.5),
    id = seq_len(n)
  )
  return(list(
    train = df[1:n_train, ],
    test = df[(n_train + 1):n, ]
  ))
}

#' Create a Dummy XGB Model Spec
#'
#' For testing, not meant to be called, add [set_py_engine()] to this object.
#'
#' @return a model spec objection.
#' @noRd
xgb_model_spec_ <- function() {
  parsnip::boost_tree(
    tree_depth = hardhat::tune(),
    min_n = hardhat::tune(),
    loss_reduction = hardhat::tune(),
    sample_size = hardhat::tune(),
    mtry = hardhat::tune(),
    learn_rate = hardhat::tune()
  ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("classification")
}

#' Create a Dummy XGB Model Spec with no Tune Params
#'
#' For testing, not meant to be called, have no specified tuning parameters to
#' test edge case of grid generation. Add [set_py_engine()] to this object.
#'
#' @return a model spec objection.
#' @noRd
xgb_model_spec_no_tune_ <- function() {
  parsnip::boost_tree() |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("classification")
}

#' Create a Dummy Recipe Object for Testing
#'
#' For testing, not meant to be called.
#'
#' @param sim_dat a [rpwf_sim_()] object.
#' @param type either `"train"` or `"test"`.
#'
#' @return a [recipes::recipe()] object.
#' @importFrom rlang .data
#' @noRd
dummy_recipe_ <- function(sim_dat, type = "train") {
  id <- X3 <- NULL
  if (type == "train") {
    return(
      recipes::recipe(target ~ ., data = sim_dat$train) |>
        recipes::step_dummy(X3, one_hot = TRUE) |>
        recipes::update_role(id, new_role = "pd.index")
    )
  }
  if (type == "test") {
    return(
      recipes::recipe(
        ~.,
        data = sim_dat$test[, which(!names(sim_dat$test) %in% sim_dat$target)]
      ) |>
        recipes::step_dummy(X3, one_hot = TRUE) |>
        recipes::update_role(id, new_role = "pd.index")
    )
  }
}

#' Generate Temporary Database and Connection
#'
#' Meant to be called in a `withr::local_tempdir()` environment of a test.
#'
#' @param tmp_dir tmp_dir path.
#'
#' @return a [DBI::dbConnect()] object.
#' @noRd
dummy_con_ <- function(tmp_dir) {
  DbCon_obj <- DbCon$new("db.SQLite", tmp_dir)
  rpwf_db_init_(DbCon_obj$con, rpwf_schema())
  return(DbCon_obj)
}
