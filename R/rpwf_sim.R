#' Generate the data needed for testing
#'
#' Generate dummy data for testing of the rpwf package
#'
#' @param n_train number of train rows
#' @param n_test number of test rows
#' @param seed random seed
#'
#' @return a list of train and test df and the id and target column names
#' @export
#'
#' @examples
#' dim_dat = rpwf_sim()
rpwf_sim = function(n_train = 100, n_test = 10, seed = 1234) {
  set.seed(seed)
  n = n_train + n_test
  df = data.frame(
    X1 = rnorm(n),
    X2 = rnorm(n),
    X3 = factor(c(rep(1:4, times = n %/% 4), 1:(n %% 4))),
    target = rbinom(n, size = 1, prob = 0.5),
    id = seq_len(n)
  )
  return(list(
    train = df[1:n_train,],
    test = df[(n_train + 1):n,],
    id = "id",
    target = "target"
  ))
}

#' Create a dummy xgb model spec
#'
#' For testing, not meant to be called, add set_py_engine() to this object
#'
#' @return a model spec objection
#' @export
xgb_model_spec_ = function() {
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

#' Create a dummy recipe object for testing
#'
#' For testing, not meant to be called
#'
#' @param sim_dat a [rpwf_sim()] object
#' @param type either `"train"` or `"test"`
#'
#' @return a [recipes::recipe()] object
#' @export
#'
#' @examples
#' test_recipe_train = dummy_recipe_(rpwf_sim)
#' test_recipe_test = dummy_recipe_(rpwf_sim, type = "test")
dummy_recipe_ = function(sim_dat, type = "train") {
  if (type == "train") {
    return(
      recipes::recipe(target ~ ., data = sim_dat$train) |>
      recipes::step_dummy(X3, one_hot = TRUE) |>
      recipes::update_role(id, new_role = "index")
    )
  }
  if (type == "test") {
    return(
      recipes::recipe(
        ~ .,
        data = sim_dat$test[, which(!names(sim_dat$test) %in% sim_dat$target)]) |>
      recipes::step_dummy(X3, one_hot = TRUE) |>
      recipes::update_role(id, new_role = "index")
    )
  }
}

#' Generate temporary database and connection
#'
#' Meant to be called in a `withr::local_tempdir()` environment of a test
#'
#' @param db_name Name of the temp database
#' @param tmp_dir tmp_dir path
#'
#' @return a [DBI::dbConnect()] object
#' @export
#'
#' @examples
#' withr::local_package("DBI")
#' tmp_dir = withr::local_tempdir(pattern = "rpwfDb")
#' con = dummy_con_(tmp_dir = tmp_dir)
dummy_con_ = function(db_name = "db.SQLite", tmp_dir){
  con = rpwf_db_con(db_name, tmp_dir)
  rpwf_db_init(con, rpwf_schema())
  return(con)
}
