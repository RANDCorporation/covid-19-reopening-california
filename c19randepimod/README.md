
<!-- README.md is generated from README.Rmd. Please edit that file -->

# c19randepimod

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

This is the R package behind [RAND’s COVID-19 State policy Evaluation
Tool](https://www.rand.org/pubs/tools/TLA173-1/tool.html). This package
contains code that reads data from external APIs, calibrates the
epidemiological model, runs the model for pre-specified scenarios and
saves these results. This R package does not contain code for the
webtool Frontend and the econ model.

## Installation

First, download the R package from the git repository. You can install
the package from source using:

``` r

# If you don't have devtools installed, install it first (make sure to )
# install.packages("devtoools")

devtools::install_local(path = "c19randepimod_1.0.0.tar.gz",upgrade = "never")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(c19randepimod)
## basic example code
```
