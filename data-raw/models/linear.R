# linear.c ---------------------------------------------------------------------

# python: https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LogisticRegression.html
# glmnet: https://parsnip.tidymodels.org/reference/details_linear_reg_glmnet.html

## C is reciprocal, meaning smaller number has higher regularization. This needs
## to be transformed since "penalty" is in the original scale

mod_list$linear.c <- rpwf_m.gen(
  py_module = "sklearn.linear_model",
  py_base_learner = "LogisticRegression",
  r_engine = "glmnet",
  hyper_par_rename =
    list(
      "penalty" = "C",
      "mixture" = "l1_ratio"
    ),
  model_mode = "classification"
)

# linear.r ---------------------------------------------------------------------
# python: https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LinearRegression.html
# glmnet: https://parsnip.tidymodels.org/reference/details_linear_reg_glmnet.html

## C is reciprocal, meaning smaller number has higher regularization. This needs
## to be transformed since "penalty" is in the original scale

mod_list$linear.r.enet <- rpwf_m.gen(
  py_module = "sklearn.linear_model",
  py_base_learner = "ElasticNet",
  r_engine = "glmnet",
  hyper_par_rename =
    list(
      "penalty" = "alpha",
      "mixture" = "l1_ratio"
    ),
  model_mode = "regression"
)
