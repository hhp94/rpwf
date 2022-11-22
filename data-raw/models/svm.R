# svm_poly_rbf.c ---------------------------------------------------------------

# python: https://scikit-learn.org/stable/modules/generated/sklearn.svm.SVC.html
# kernlab: https://cran.r-project.org/web/packages/kernlab/kernlab.pdf

## rbf and poly have separate hyper parameters but they are named the same in
## sci-kit learn. I don't for see potential for bugs but further testings are
## needed.

## C is reciprocal, meaning smaller number has higher regularization. This needs
## to be transformed since "penalty" is in the original scale

mod_list$svm_poly_rbf.c <- rpwf_m.gen(
  py_module = "sklearn.svm",
  py_base_learner = "SVC",
  r_engine = "kernlab",
  hyper_par_rename =
    list(
      "cost" = "C",
      "degree" = "degree",
      "scale_factor" = "gamma",
      "rbf_sigma" = "gamma",
      "kernel_offset" = "coef0"
    ),
  model_mode = "classification"
)

# svm_poly_rbf.r ---------------------------------------------------------------

# python: https://scikit-learn.org/stable/modules/generated/sklearn.svm.SVR.html
# kernlab: https://cran.r-project.org/web/packages/kernlab/kernlab.pdf

mod_list$svm_poly_rbf.r <- rpwf_m.gen(
  py_module = "sklearn.svm",
  py_base_learner = "SVR",
  r_engine = "kernlab",
  hyper_par_rename =
    list(
      "cost" = "C",
      "degree" = "degree",
      "scale_factor" = "gamma",
      "rbf_sigma" = "gamma",
      "kernel_offset" = "coef0",
      "margin" = "epsilon"
    ),
  model_mode = "regression"
)
