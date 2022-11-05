# This is for if one wants to just pass the data frame to Python

rpwf_df_set <- function(preprocs) {
  stopifnot("preproc accept list of recipes" = is.vector(preprocs))
  stopifnot("preproc accept list of recipes" = "recipe" == unique(sapply(preprocs, class)))

  df <- dplyr::tibble(preprocs = unique(preprocs))
  return(df)
}

