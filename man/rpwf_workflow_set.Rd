% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_rpwf_workflow_export.R
\name{rpwf_workflow_set}
\alias{rpwf_workflow_set}
\title{Customized Version of \code{{workflowsets}}}
\usage{
rpwf_workflow_set(preprocs, models, costs)
}
\arguments{
\item{preprocs}{list or vector of \code{\link[recipes:recipe]{recipes::recipe()}}.}

\item{models}{list or vector of model spec. Generated by adding
\code{\link[=set_py_engine]{set_py_engine()}} to a model, e.g. \code{\link[parsnip:boost_tree]{parsnip::boost_tree()}} and
\code{\link[parsnip:set_engine]{parsnip::set_engine()}}.}

\item{costs}{list or vector of sklearn cost optimization metrics such as
"neg_log_loss" and "roc_auc". Check the \href{https://scikit-learn.org/stable/modules/model_evaluation.html}{docs}
for available values.}
}
\value{
tibble that contains a combination of list of recipes, models,
and costs.
}
\description{
Wrapper around \code{\link[tidyr:expand]{tidyr::crossing()}} that creates all combinations of recipes
and models.
}
\examples{
d <- mtcars
d$id <- seq_len(nrow(d))
m1 <- parsnip::boost_tree() |>
  parsnip::set_engine("xgboost") |>
  parsnip::set_mode("classification") |>
  set_py_engine(py_module = "xgboost", py_base_learner = "XGBClassifier")
r1 <- d |>
  recipes::recipe(vs ~ .) |>
  # "pd.index" is the special column that used for indexing in pandas
  recipes::update_role(id, new_role = "pd.index")
wf <- rpwf_workflow_set(list(r1), list(m1), "neg_log_loss")
wf
}
