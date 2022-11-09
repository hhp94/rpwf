# rpwf_workflow_export ---------------------------------------------------------

#' Constructor for `rpwf_workflow_set` Class
#'
#' @param obj a [tibble::tibble()].
#'
#' @return a `rpwf_workflow_set` object, which inherits from [tibble::tibble()].
#' @noRd
new_rpwf_workflow_set <- function (obj = tibble::tibble()) {
  stopifnot(tibble::is_tibble(obj))
  structure(obj, class = c("rpwf_workflow_set", class(tibble::tibble())))
}

#' Generic S3 Function for `rpwf_workflow_set` and `rpwf_data_set`
#'
#' @param obj a `rpwf_workflow_set` or `rpwf_data_set` object.
#' @inheritParams rpwf_add_py_model
#' @inheritParams rpwf_grid_gen_
#' @inheritParams rpwf_add_random_state_
#' @param seed random seed.
#'
#' @return A data frame with necessary columns for export into the database.
#' @export
#' @examples
#' # Create the database
#' temp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db("db.SQLite", temp_dir)
#'
#' # Create a `workflow_set`
#' d <- mtcars
#' d$id <- seq_len(nrow(d))
#' m1 <- parsnip::boost_tree() |>
#'   parsnip::set_engine("xgboost") |>
#'   parsnip::set_mode("classification") |>
#'   set_py_engine(py_module = "xgboost", py_base_learner = "XGBClassifier")
#' r1 <- d |>
#'   recipes::recipe(vs ~ .) |>
#'   # "pd.index" is the special column that used for indexing in pandas
#'   recipes::update_role(id, new_role = "pd.index")
#' wf <- rpwf_workflow_set(list(r1), list(m1), "neg_log_loss")
#'
#' to_export <- wf |>
#'   rpwf_augment(db_con, dials::grid_latin_hypercube, size = 10)
#' list.files(paste0(temp_dir, "/rpwfDb"), recursive = TRUE) # Files are created
rpwf_augment <- function(obj, ...) {
  UseMethod("rpwf_augment")
}

rpwf_augment.default <- function(obj, ...) {
  stop("Only accept a `rpwf_workflow_set` or `rpwf_data_set` object")
}

#' Generic S3 Function for [rpwf_augment()] Object into the Database
#'
#' @param obj an augmented `rpwf_workflow_set()` or `rpwf_data_set()` object.
#' @inheritParams rpwf_add_py_model
#'
#' @return number of rows exported.
#' @export
#' @examples
#' # Create the database
#' temp_dir <- withr::local_tempdir()
#' db_con <- rpwf_connect_db("db.SQLite", temp_dir)
#'
#' # Create a `workflow_set`
#' d <- mtcars
#' d$target <- stats::rbinom(nrow(d), 1, 0.5)
#' m1 <- parsnip::boost_tree() |>
#'   parsnip::set_engine("xgboost") |>
#'   parsnip::set_mode("classification") |>
#'   set_py_engine("xgboost", "XGBClassifier", "my_xgboost_model")
#' r1 <- d |>
#'   recipes::recipe(target ~ .)
#' wf <- rpwf_workflow_set(list(r1), list(m1), "neg_log_loss")
#'
#' to_export <- wf |>
#'   rpwf_augment(db_con, dials::grid_latin_hypercube, size = 10)
#' rpwf_write_grid(to_export, db_con)
#' rpwf_write_df(to_export, db_con)
#'
#' # Before exporting
#' DBI::dbGetQuery(db_con$con, "SELECT * FROM wflow_tbl;")
#' # After exporting
#' rpwf_export_db(to_export, db_con)
#' DBI::dbGetQuery(db_con$con, "SELECT * FROM wflow_tbl;")
rpwf_export_db <- function(obj, db_con) {
  UseMethod("rpwf_export_db")
}

rpwf_export_db.default <- function(obj, db_con) {
  stop("Only accept a `rpwf_workflow_set` or `rpwf_data_set` object")
}

# rpwf_only_df_export ----------------------------------------------------------
#' Constructor for `rpwf_data_set` Class
#'
#' @param obj a [tibble::tibble()].
#'
#' @return a `rpwf_data_set` object, which inherits from [tibble::tibble()].
#' @noRd
new_rpwf_data_set <- function (obj = tibble::tibble()) {
  stopifnot(tibble::is_tibble(obj))
  structure(obj, class = c("rpwf_data_set", class(tibble::tibble())))
}

