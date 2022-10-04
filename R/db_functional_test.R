# xgb_model_1 = parsnip::boost_tree(
#   tree_depth = hardhat::tune(),
#   min_n = hardhat::tune(),
#   loss_reduction = hardhat::tune(),
#   sample_size = hardhat::tune(),
#   mtry = hardhat::tune(),
#   learn_rate = hardhat::tune()
# ) |>
#   parsnip::set_engine("xgboost") |>
#   parsnip::set_mode("classification")
#
# xgb_model_2 = parsnip::boost_tree(
#   tree_depth = hardhat::tune(),
#   min_n = hardhat::tune(),
#   loss_reduction = hardhat::tune(),
#   sample_size = hardhat::tune(),
#   mtry = hardhat::tune(),
#   learn_rate = hardhat::tune()
# ) |>
#   parsnip::set_engine("xgboost") |>
#   parsnip::set_mode("classification")
#
# dummy_df_nrow = 50
# set.seed(1234)
# dummy_recipe =
#   recipes::recipe(target ~ ., data = dplyr::slice_sample(train, n = dummy_df_nrow)) |> # boiler-plate recipe
#   recipes::step_mutate(target = rbinom(dummy_df_nrow, 1, 0.5),
#                        age = factor(age)) |> # dummy target values
#   recipes::step_dummy(age, one_hot = TRUE) |>
#   recipes::update_role(patient_id, new_role = "index") # Change role of `patient_id`
#
# RSQLite.con = rpwf_db_con("base_db.SQLite", here::here())
# rpwf_db_init(RSQLite.con) ## Create the schema and tables and add some data
