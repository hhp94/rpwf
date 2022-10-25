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

# add the models ---------------------------------------------------------------
mod_list <- list()
source("./data-raw/xgboost.R")
source("./data-raw/svm.R")

# add and export object --------------------------------------------------------
sup_mod_df <- purrr::reduce(
  mod_list,
  rbind
)

sup_mod_df <- unique(sup_mod_df)
usethis::use_data(sup_mod_df, overwrite = TRUE, internal = TRUE, version = 3)
