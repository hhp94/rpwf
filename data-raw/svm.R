# svm_poly_rbf.c -------------------------------------------------------------------
## rbf and poly have separate hyper parameters but they are named the same in
## sci-kit learn. I don't for see potential for bugs but further testings are
## needed.
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
