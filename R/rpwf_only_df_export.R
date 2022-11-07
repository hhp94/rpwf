# This is for if one wants to just pass the data frame to Python

rpwf_df_set <- function(...) {
  preprocs <- list(...)

  stopifnot("preproc accept recipes" = "recipe" == unique(sapply(preprocs, class)))

  df <- dplyr::tibble(preprocs = unique(preprocs))
  df$recipe_tag <- sapply(df$preprocs, \(x) {
    x$recipe_tag
  })
  df$grids <- NA
  return(df)
}
