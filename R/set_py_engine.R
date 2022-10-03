set_py_engine = function(obj, py_base_learner, con, args = NULL) {
  avail = DBI::dbGetQuery(
    con, 'SELECT py_base_learner FROM model_type_tbl'
    )$py_base_learner
  if (!py_base_learner %in% avail) {
    cat(paste(avail, collapse = ", "))
    cat("\n")
    stop("Select from the above models")
  }
  obj$py_base_learner = py_base_learner

  if (!is.null(args)) {
    stopifnot("Use named list for py learner args" =
                all("list" %in% class(args)) & !is.null(names(args)))
    obj$py_base_learner_args = jsonlite::toJSON(args, auto_unbox = TRUE)
  }
  return(obj)
}
