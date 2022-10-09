
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rpwf

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/hhp94/rpwf/branch/master/graph/badge.svg)](https://app.codecov.io/gh/hhp94/rpwf?branch=master)
<!-- badges: end -->

`{rpwf}` allows data engineering in R with
[{tidymodels}](https://www.tidymodels.org/) and running the generated
workflow sets using python’s very mature ML framework
[scikit-learn](https://scikit-learn.org/stable/index.html). To
facilitate this, a SQLite database is created to handles all the paths
and information needed to fit models between R and Python. On top of the
wonderful experience manipulating data in R, `{rpwf}` also enables the
ability to use the generation of hyper parameter grids using the
[{dials}](https://dials.tidymodels.org/) functions such as
[`grid_max_entropy`](https://dials.tidymodels.org/reference/grid_max_entropy.html)
and etc. for grid search in
[scikit-learn](https://scikit-learn.org/stable/index.html).

The combination of automated handling of paths and files with SQLite and
provided python CLI templates aims to make feature engineering on HPCs a
smoother experience.

## Installation

You can install the development version of `{rpwf}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hhp94/rpwf")
```

After installing the package, make sure to either download the python
codes at [this
link](https://github.com/hhp94/rpwf/tree/master/inst/python/rpwf) or run
the function:

``` r
rpwf::rpwf_cp_py_codes(<project root path here>)
```

Then, install the package as a local package with pip with
`python -m pip install -e <path to the downloaded folder>` so that the
imports works.
