# rpwf_add_all -----------------------------------------------------------------
rpwf_workflow_set = function(preprocs, models, cost) {
  hashes = vapply(preprocs, rlang::hash, "character")
  if (length(hashes) > dplyr::n_distinct(hashes)) {
    warning("Found duplicated input recipes")
  } # Check for duplicated recipes

  rpwf = tidyr::crossing(preprocs, models, cost) # crossing of the parameters
  return(rpwf)
}

rpwf_add_model_info = function(obj) {
  stopifnot("Run rpwf_workflow_set() first!" =
              all(c("preprocs", "models", "cost") %in% names(obj)))
  rpwf = obj
  # Add the base learner
  rpwf$py_base_learner = vapply(rpwf$models, \(x) {
    stopifnot("add set_py_engine() to your {parsnip} model_spec object" =
                !is.null(x$py_base_learner))
    x$py_base_learner
  }, "character") # get the engine

  # Add the base learner related args if presented
  rpwf$py_base_learner_args = vapply(rpwf$models, \(x) {
    if (!is.null(x$py_base_learner_args)) {
      return(x$py_base_learner_args)
    }  else {
      return(NA_character_)
    }
  }, "character")

  # get the model mode (regression/classification)
  rpwf$model_mode = vapply(rpwf$models, \(x) {x$mode}, "character")
  return(rpwf)
}

# This function is a wrapper for `DBI::dbGetQuery` that also perform some cleaning.
# Should only return one column and <= 1 row for every query. Used in every
# function afterwards.
rpwf_query = function(val, query, con) {
  query_res_list = lapply(val, \(x) {
    DBI::dbGetQuery(con, glue::glue_sql(query, .con = con), list(x))
  })
  stopifnot("Query should only return 1 column and <= 1 row" =
    all(sapply(query_res_list, \(query) {ncol(query) == 1 & nrow(query) <= 1})))
  # Number of rows for each query
  empty_res = sapply(query_res_list, \(query) {nrow(query)})
  # If any query finds nothing (nrow(query) == 0) then replace with NA
  query_res_list[which(empty_res == 0L)] = NA
  return(c(unlist(query_res_list))) # return an unlisted vector for mutate
}

# Adds description of each workflow to the rpwf object with mutate
rpwf_add_desc = function(obj) {
  stopifnot("Run rpwf_add_model_info first!" = "py_base_learner" %in% names(obj))
  # by pasting together preprocs, models and cost cols
  return(dplyr::mutate(obj, wflow_desc = paste(
    names(preprocs), py_base_learner, cost, sep = "-"
  )))
}

# Add relevant parameters to the grid. Can also provide fixed params here that's
# different from default param in xgboost
rpwf_add_grid_param = function(obj, .grid_fun, fixed_params, seed, ...) {
  stopifnot("Run rpwf_workflow_set() first!" =
              all(c("preprocs", "models", "cost") %in% names(obj)))
  dplyr::mutate(obj, grids = purrr::map2(
    models, preprocs, \(x, y) {
      set.seed(seed)
      rpwf_grid_gen(
        model = x, preproc = y, .grid_fun = .grid_fun,
        fixed_params = fixed_params, ...
      )
    }))
}

# Add the RGrid R6 objects using the provided grids. This object does the heavy
# lifting of generating the path, parquet and updating the database.
rpwf_add_grids = function(obj, con, proj_root_path) {
  stopifnot("run rpwf_add_grid_param() first" = "grids" %in% names(obj))

  # For each grid, initialize a new RGrid(), call self$export(), then return
  # the object. This make sure the same object called twice will just fetch
  # the result from the previous call.
  RGrid_obj = lapply(obj$grids, \(x) {
    return(RGrid$new(x, con = con, proj_root_path = proj_root_path)$export())
  })

  # Grab the hashes from the returned RGrid() objects. at this point, we made
  # sure that 1) the db is updated, 2) file is exported, 3) file exists.
  query_res = rpwf_query(# Use the hash to find the grid_id
    lapply(RGrid_obj, \(x) {x$hash}),
    query = 'SELECT grid_id FROM r_grid_tbl WHERE grid_hash = ?',
    con = con)
  # Add `grid_id` to the accumulating object
  return(dplyr::mutate(obj, grid_id = query_res))
}

# Add the TrainDf() R6 objects using provided recipes. Similar to add_grids, this
# object does the heavy lifting of generating the path, parquet and updating
# the database.
rpwf_add_dfs = function(obj, con, proj_root_path, seed) {
  # For each recipe, initialize a new TrainDf(), call self$export(), then return
  # the object. This make sure the same object called twice will just fetch
  # the result from the previous call.
  TrainDf_obj = lapply(obj$preprocs, \(x) {
    set.seed(seed)
    return(TrainDf$new(x, con = con, proj_root_path = proj_root_path)$export())
  })

  query_res = rpwf_query(
    lapply(TrainDf_obj, \(x) {x$hash}),
    query = 'SELECT df_id FROM df_tbl WHERE df_hash = ?',
    con = con)
  # Add `df_id` to the accumulating object
  return(dplyr::mutate(obj, df_id = query_res))
}

# Query the cost_id associated with the requested cost function
rpwf_add_cost = function(obj, con) {
  query_res = rpwf_query(# Query the db for the cost_id
    obj$cost,
    query = 'SELECT cost_id FROM cost_tbl WHERE cost_name = ?',
    con = con)
  # Add `cost_id` to the accumulating object
  return(dplyr::mutate(obj, cost_id = query_res))
}

# Query the model_type id for the requested model
rpwf_add_model_type = function(obj, con) {
  stopifnot("add set_py_engine() to your {parsnip} model_spec object" =
              "py_base_learner" %in% names(obj))

  query_res = rpwf_query(
    obj$py_base_learner,
    query = 'SELECT model_type_id FROM model_type_tbl WHERE py_base_learner = ?',
    con = con)
  # Add `model_type_id` to the accumulating object
  return(dplyr::mutate(obj, model_type_id = query_res))
}

# Add the random state seeds
rpwf_add_random_state = function(obj, range = c(1L, 5000L), seed) {
  set.seed(seed)
  stopifnot("range of random_state should be of length 2" = length(range) == 2L)
  sorted_range = as.integer(sort(range))
  stopifnot("range should be an int vector" = !anyNA(sorted_range))
  random_state = sample(sorted_range[1]:sorted_range[2], size = nrow(obj))
  # Add `random_state` to the accumulating object
  return(dplyr::mutate(obj, random_state = random_state))
}

#### Wrapper to generate the object to be exported to the database
rpwf_add_all = function(wflow_obj,
                        dbCon,
                        proj_path,
                        df_train,
                        grid_fun = dials::grid_latin_hypercube,
                        fixed_params = NULL,
                        ...,
                        randstate_range = c(1L, 5000L),
                        seed = 1234L) {
  set.seed(seed)
  wflow_obj |>
  rpwf_add_model_info() |>
  rpwf_add_desc() |>
  rpwf_add_grid_param(.grid_fun = grid_fun,
                      fixed_params = fixed_params,
                      seed = seed,
                      ...) |>
  rpwf_add_grids(con = dbCon, proj_root_path = proj_path) |>
  rpwf_add_dfs(con = dbCon,
               proj_root_path = proj_path,
               seed = seed) |>
  rpwf_add_cost(con = dbCon) |>
  rpwf_add_model_type(con = dbCon) |>
  rpwf_add_random_state(range = randstate_range, seed = seed)
}

# Export the object that has been processed by the other functions into the
# database so that python can query the information.

rpwf_wflow_hash = function(df){
  apply(as.data.frame(df), 1, rlang::hash)
}

rpwf_export_db = function(obj, con) {
  # These columns must be present in the data
  required = c(
    "df_id", "grid_id", "wflow_desc" , "cost_id", "model_type_id",
    "random_state", "py_base_learner_args"
  )
  hash_chk = setdiff(required, "wflow_desc") # columns for hash checking
  # which mandatory column is not in the processed data?
  missing_cols = required[which(!required %in% names(obj))]
  if (length(missing_cols) != 0L) {
    # Which columns are not found in the object
    stop(glue::glue('{paste(missing_cols, collapse = ", ")} not found'))
  }
  # Query the wflow that's already in the database
  db_wflow_hash = rpwf_wflow_hash(
    dplyr::select(DBI::dbGetQuery(con, glue::glue('SELECT * FROM wflow_tbl;')),
                  dplyr::all_of(hash_chk))
  )

  to_export_hash = rpwf_wflow_hash(dplyr::select(obj, dplyr::all_of(hash_chk)))
  matched_wflow = to_export_hash %in% db_wflow_hash
  if (any(matched_wflow)) {
    message("the following workflows are already in the database\n")
    print(obj[matched_wflow, ])
  }
  # Only add the workflow that's not in the database
  to_export = as.data.frame(obj[!matched_wflow, required])

  if (nrow(to_export) == 0) {
    message("All workflows found in db, exiting...")
    return(0)
  } else {
    message("Exporting workflows to db...")
    DBI::dbAppendTable(conn = con, name = "wflow_tbl", value = to_export)
  }
}
