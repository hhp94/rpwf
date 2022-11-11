
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rpwf

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/hhp94/rpwf/branch/master/graph/badge.svg)](https://app.codecov.io/gh/hhp94/rpwf?branch=master)
<!-- badges: end -->

{rpwf} is a lightweight R and Python package that enables:

- data engineering in
  ![R](https://img.shields.io/static/v1?label=%20&message=%20&color=blue&logo=R)
  with [{tidymodels}](https://www.tidymodels.org/).
  - hyper parameter grids generation using the
    [{dials}](https://dials.tidymodels.org/) functions such as
    [`grid_max_entropy`](https://dials.tidymodels.org/reference/grid_max_entropy.html).  
  - vast libraries of cool data transformation methods such as splines
    basis expansion, data reduction, and multiple imputation.  
- testing the generated data transformation pipeline in
  ![Python](https://img.shields.io/static/v1?label=%20&message=%20&color=yellow&logo=Python)
  with [scikit-learn](https://scikit-learn.org/stable/index.html).  
- streamlining the use of HPC and cloud computing to massively speed up
  feature engineering experimentation.

# Installation

## R

- Install the package from github

``` r
# install.packages("devtools")
devtools::install_github("hhp94/rpwf", branch = "master")
```

## Python

- First, setup a python environment with conda.

``` bash
conda create -n py39 python=3.9.13 anaconda
```

- You’ll need the the following python packages installed in your python
  environment. `pandas` and `scikit-learn` should already be installed
  with the above command.
  - sqlalchemy  
  - pandas  
  - pandas-downcast  
  - pyarrow  
  - scikit-learn  
  - xgboost  
  - any other machine learning library such as `lightgbm` and etc.

``` bash
conda activate py39
conda install pyarrow sqlalchemy xgboost
pip install pandas-downcast
```

- Next, copy the python codes into any folder with `rpwf_cp_py_codes()`.
  Here I’m just copying the codes to my `home/opt` folder.

``` r
list.files("~/opt") # Coping python codes to this folder
rpwf_cp_py_codes("~/opt")
list.files("~/opt") # A folder called "rpwf" is created
```

- Install the copied python codes as a local package for maximum
  flexibility. **Remember the -e flag**, this allows you to modify the
  python codes without re-installation.

``` bash
python -m pip install -e ~/opt/rpwf
#   Preparing metadata (setup.py) ... done
# Installing collected packages: local-rpwf
#   Running setup.py develop for local-rpwf
# Successfully installed local-rpwf-0.1.0
```

- Remove the package if needed with

``` bash
pip uninstall local-rpwf
# Found existing installation: local-rpwf 0.1.0
# Uninstalling local-rpwf-0.1.0:
#   Would remove:
#     .../local-rpwf.egg-link
# Proceed (Y/n)? Y
#   Successfully uninstalled local-rpwf-0.1.0
```

## Linux installation

The R [`{arrow}`](https://arrow.apache.org/) installation on linux might
fail. In which case, use this
[instruction](https://cran.r-project.org/web/packages/arrow/vignettes/install.html).
The following codes worked for me

``` r
Sys.setenv(NOT_CRAN = TRUE)
install.packages("arrow", repos = c(arrow = "https://nightlies.apache.org/arrow/r", getOption("repos")))
```

Or update your current build of arrow with

``` r
source("https://raw.githubusercontent.com/apache/arrow/master/r/R/install-arrow.R")
install_arrow(verbose = TRUE)
```
