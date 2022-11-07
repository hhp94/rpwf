# This is for if one wants to just pass the data frame to Python

rpwf_df_set <- function(..., db_con) {
  preprocs <- list(...)

  stopifnot("preproc accept recipes" = "recipe" == unique(sapply(preprocs, class)))

  df <- dplyr::tibble(preprocs = unique(preprocs))
  df$recipe_tag <- sapply(df$preprocs, \(x) {
    x$recipe_tag
  })
  df$grids <- NA
  df$TrainDf <- lapply(df$preprocs, \(x) {TrainDf$new(x, db_con)})
  return(df)
}
