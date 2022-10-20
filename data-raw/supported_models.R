# template --------------------------------------------------------------------
rpwf_m.gen <- function(py_module,
                       py_base_learner,
                       r_engine,
                       hyper_par_rename,
                       model_mode) {
  df <- data.frame(
    py_module = py_module,
    py_base_learner = py_base_learner,
    r_engine = r_engine,
    hyper_par_rename = as.character(jsonlite::toJSON(hyper_par_rename)),
    model_mode = model_mode
  )
  return(df)
}

# rpwf_m.gen(
#   py_module = <input>,
#   py_base_learner = <input>,
#   r_engine = <input>,
#   hyper_par_rename =
#     list(
#       # "mtry" = "colsample_bytree"
#     ),
#   model_mode = <input>
# )

mod_list <- list()

# xgboost.c --------------------------------------------------------------------
mod_list$rpwf_m.xgboost.c <- rpwf_m.gen(
  py_module = "xgboost",
  py_base_learner = "XGBClassifier",
  r_engine = "xgboost",
  hyper_par_rename =
    list(
      "mtry" = "colsample_bytree",
      "trees" = "n_estimators",
      "min_n" = "min_child_weight",
      "tree_depth" = "max_depth",
      "learn_rate" = "learning_rate",
      "loss_reduction" = "gamma",
      "sample_size" = "subsample"
    ),
  model_mode = "classification"
)

# add and export object --------------------------------------------------------
sup_mod_df <- purrr::reduce(
  mod_list,
  rbind
)

sup_mod_df <- unique(sup_mod_df)
usethis::use_data(sup_mod_df, overwrite = TRUE, internal = TRUE, version = 3)
