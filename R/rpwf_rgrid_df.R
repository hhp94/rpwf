# Base -------------------------------------------------------------------------
Base = R6::R6Class(
  "Base",
  # This class is the parent of RGrid and TrainDf, not meant to be called
  public = list(
    hash = NULL,
    path = NULL,
    df = NULL,
    con = NULL,
    export_query = NULL,
    query_results = NULL,
    proj_root_path = NULL,
    db_folder = NULL,

    initialize = function(df, con, proj_root_path) {
      stopifnot("Invalid connection" = DBI::dbIsValid(con)) # Connection is valid
      self$df = df
      self$con = con
      self$hash = NULL
      self$export_query = NULL
      self$query_results = NULL
      self$db_folder = NULL
      self$proj_root_path = proj_root_path
    },

    # Either find one unique row or no row using where == hash format
    exec_query = function(query) {DBI::dbGetQuery(self$con, query, list(self$hash))},

    # Calculate the hash of the object
    set_hash = function(val) {self$hash = as.character(val)},

    # Path to store the object on disk
    set_path = function(val) {self$path = val},

    # Root folder of the project
    set_db_folder = function(val) {self$db_folder = val},

    # query have to return a df (so nrow() works) with 0 <= nrow() < 2
    set_query_results = function(val) {
      stopifnot(is.data.frame(val) & nrow(val) < 2)
      self$query_results = val
    },

    # Query to export to database
    set_export_query = function(val) {self$export_query = val},

    export_db = function() {
      if (!is.null(self$export_query)) {
        message("Exporting to db")
        DBI::dbExecute(self$con, self$export_query) # if an export query is created
        self$set_export_query(NULL) # then run the query and reset to NULL
      } else {
        message("Found entry in db")
      }
      invisible(self)
    },

    export_parquet = function() {
      withr::local_dir(self$proj_root_path)
      if (!file.exists(self$path)) {
        message("Writing parquet file\n")
        arrow::write_parquet(self$df, self$path)
      } else {
        message("Parquet file found\n")
      }
      invisible(self)
    },

    export = function() {
      self$export_db()$export_parquet()
    },

    query_path = function(path_query, new_export_query) {
      # If the query generate no entry, then make a path
      if (nrow(self$query_results) == 0L) {
        self$set_path(path_query)
        self$set_export_query(new_export_query)
      } else {
        # Else get the path from the query results and assign to self$path
        self$set_path(self$query_results$path)
      }
    },

    create_folder = function() {
      ## Create folder if not exists
      withr::local_dir(self$proj_root_path)
      folder = paste("pexpDb", self$db_folder, sep = "/")
      if (!dir.exists(folder)) {
        message(glue::glue("Creating {folder} folder..."))
        dir.create(folder)
      } else {
        message("Folder already created")
      }
    }
  )
)

# RGrid ------------------------------------------------------------------------
RGrid = R6::R6Class(
  "RGrid",
  inherit = Base,
  public = list(
    initialize = function(grid_obj, con, proj_root_path) {
      stopifnot("Run pexp_grid_gen() first" = "pexp_grid" %in% class(grid_obj))

      super$initialize(grid_obj, con, proj_root_path) # Init from the super class
      self$set_hash(rlang::hash(self$grid_obj)) # hash the grid
      self$set_query_results(self$exec_query(
        glue::glue_sql( # hash is passed into ?
          'SELECT grid_path AS path FROM r_grid_tbl WHERE grid_hash = ?',
          .con = self$con
        )
      ))
      self$set_db_folder("pexp_grids") # Set the root folder to "pexp_grids"
      self$create_folder() # Create the folder if needed
      self$query_path(
        path_query = glue::glue(
          'pexpDb', '{self$db_folder}', '{self$hash}.grid.parquet', .sep = "/"
        ),
        new_export_query = glue::glue_sql(
          'INSERT INTO r_grid_tbl (grid_path, grid_hash) VALUES ({vals*})',
          vals = c(self$path, self$hash),
          .con = self$con
        ) # Get the path for parquet file
      )
    }
))

# TrainDf ----------------------------------------------------------------------
TrainDf = R6::R6Class(
  "TrainDf",
  inherit = Base,
  public = list(
    prepped = NULL,
    term_info = NULL,
    idx_col = NULL,
    target = NULL,
    predictors = NULL,
    # Holds the recipe to transform the data
    initialize = function(recipe, con, proj_root_path) {
      stopifnot("a recipe object required" = "recipe" %in% class(recipe))

      super$initialize(NULL, con, proj_root_path) # Init from the super class
      self$prepped = recipes::prep(recipe) # Init the prepped object
      self$term_info = self$prepped$term_info # post-transform train metadata
      self$set_hash(rlang::hash(self$prepped)) # Set the hash of the prepped obj
      # use the hash of the prepped to find the path
      self$set_query_results(self$exec_query(
        glue::glue_sql('SELECT df_path AS path FROM df_tbl WHERE df_hash = ?',
                       .con = self$con)
      ))
      self$set_idx_col() # set index column for pandas
      self$set_target_col() # set target column for pandas
      self$set_predictors() # get the predictors
      self$set_df() # From the query results, generate df if needed
      self$set_db_folder("TrainDf") # Set the root folder to "TrainDf"
      self$create_folder() # Create the folder if needed
      self$query_path(
        path_query = glue::glue(
          'pexpDb', '{self$db_folder}', '{self$hash}.df.parquet', .sep = "/"
        ),
        new_export_query = glue::glue_sql(
          'INSERT INTO df_tbl (idx_col, target, predictors, df_path, df_hash)
            VALUES ({vals*})',
          vals = c(
            self$idx_col, self$target, self$predictors, self$path, self$hash
          ), .con = self$con
        )
      ) # Get the path for parquet file
    },

    set_idx_col = function() {
      idx = which(self$term_info$role == "index") # which var is "index"
      stopifnot("Run `recipes::update_role(<one index column>, new_role = 'index')" = length(idx) == 1)
      self$idx_col = self$term_info[idx, "variable", drop = TRUE]
      invisible(self)
    },

    set_target_col = function() {
      targ = which(self$term_info$role == "outcome") # which var is "outcome"
      if(length(targ) == 1){
        self$target = self$term_info[targ, "variable", drop = TRUE]
      } else {
        message("No outcome added. Add in recipe, or assuming this is test data")
      }
      invisible(self)
    },

    set_predictors = function() {
      pred = which(self$term_info$role == "predictor") # which are predictors
      stopifnot("Must have more than 0 predictors" = length(pred) > 0)
      self$predictors = as.character(
        jsonlite::toJSON(self$term_info[pred, "variable", drop = TRUE])
      )
    },

    set_df = function() {
      withr::local_dir(new = self$proj_root_path)
      if (nrow(self$query_results) == 0L) {
        # If query yields 0 rows, then create df
        message("Transforming data...")
        self$df = recipes::juice(self$prepped)
      } else if (!file.exists(self$query_results$path)) {
        # If parquet file not found but is in found in database
        message("Transforming data...") # Then transform the data
        self$df = recipes::juice(self$prepped)
      } # Otherwise no transformation needed, leave `self$df` as NULL
      else {
        message("File found, no transformation needed")
      }
    }
  )
)

# pexp_grid --------------------------------------------------------------------
pexp_grid_rename_fns = function(x) {
  # This function renames the param to SKL API
  dplyr::case_when(
    x == "mtry" ~ "colsample_bytree",
    x == "trees" ~ "n_estimators",
    x == "min_n" ~ "min_child_weight",
    x == "tree_depth" ~ "max_depth",
    x == "learn_rate" ~ "learning_rate",
    x == "loss_reduction" ~ "gamma",
    x == "sample_size" ~ "subsample",
    TRUE ~ x
  )
}

## If we specify fixed parameters, this function handles that. not meant to be
## manually called
pexp_add_fixed_params = function(model, fixed_params, r_grid) {
  grid = r_grid # generate temporary object to bypass the scoping complications
  if (!model$engine %in% names(fixed_params)) {
    # `fixed_params` list doesn't have model
    message("No fixed params requested for this model")
    return(grid)
  }
  params = fixed_params[[model$engine]] # Get the fixed_param for this model
  param_names = intersect(names(params), names(model$args))
  if (length(param_names) == 0) {
    message("Valid fixed params not found")
    return(grid)
  }
  for (i in param_names) {
    stopifnot("Fixed param length not 1" = length(params[[i]]) == 1)
    message("Assigning fixed params")
    grid[[i]] = params[[i]]
  }
  return(grid)
}

pexp_finalize_params = function(model, preproc) {
  # some parameters (mtry) requires the data to be finalized
  stopifnot("model_spec" %in% class(model) &
              "recipe" %in% class(preproc))

  # Get just the predictors of pre-transformed data
  preds = preproc$var_info[
    which(preproc$var_info$role == "predictor"), "variable", drop = TRUE
    ]

  # mtry conversion to python requires ncol() and nrow() of pre-transform data
  finalized_params = dials::finalize(
    hardhat::extract_parameter_set_dials(model),
    dplyr::select(preproc$template , dplyr::all_of(preds))
  )

  # prevent specifying of params not belonging to the model in R. Works by
  # raising an subscript out of range error if the param not found.
  tryCatch({
    labels = sapply(finalized_params$object, \(x) {
      x[["label"]]
    })
  },
  error = function(c) {
    message("Tuning params not found")
    stop(c)
  })

  return(list(pars = finalized_params, n_predictors = length(preds)))
}

pexp_grid_gen = function(model,
                         preproc,
                         .grid_fun,
                         fixed_params = NULL,
                         ...) {
  params = pexp_finalize_params(model = model, preproc = preproc)
  r_grid = .grid_fun(x = params$pars, ...)

  if ("mtry" %in% params$pars$name) {
    # `colby_sample` is mtry converted into proportion so we need a denominator.
    #  Denominator is number is number of predictors
    r_grid$mtry = round(r_grid$mtry / params$n_predictors, 2)
  }

  if (!is.null(fixed_params)) {
    ## Add fixed params if provided
    r_grid = pexp_add_fixed_params(model = model,
                                   fixed_params = fixed_params,
                                   r_grid = r_grid)
  }

  python_grid = dplyr::rename_with(r_grid, pexp_grid_rename_fns)
  class(python_grid) = c("pexp_grid", class(tibble::tibble()))
  return(python_grid)
}
