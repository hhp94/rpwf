# xgboost.c --------------------------------------------------------------------
mod_list$xgboost.c <- rpwf_m.gen(
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
