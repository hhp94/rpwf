# svm_poly.c -------------------------------------------------------------------
mod_list$svm_poly.c = rpwf_m.gen(
  py_module = "sklearn.svm",
  py_base_learner = "SVC",
  r_engine = "kernlab",
  hyper_par_rename =
    list(
      "cost" = "C",
      "degree" = "degree",
      "scale_factor" = "gamma"
    ),
  model_mode = "classification"
)
