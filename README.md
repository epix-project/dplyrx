
<!-- README.md is generated from README.Rmd. Please edit that file -->
dplyrx <img src="man/figures/logo.png" align="right" width=120/>
================================================================

[![Travis-CI Build Status](https://travis-ci.org/epix-project/dplyrx.svg?branch=master)](https://travis-ci.org/epix-project/dplyrx) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dplyrx)](https://cran.r-project.org/package=dplyrx)

`dplyrx` is a collection of functions that extend those in the [`dplyr`](https://dplyr.tidyverse.org) package.

Installation
------------

You can install dplyrx from github with:

``` r
# install.packages("devtools")
devtools::install_github("epix-project/dplyrx")
```

Example
-------

Below is a basic example which shows you how to solve a common problem. Imagine that you have a data frame that looks like this:

``` r
set.seed(30101976)
data <- expand.grid(letters[1:3], 1:3, 4:6)
data <- cbind(data, data.frame(replicate(3, sample(1:100, nrow(data), TRUE))))
```

where `Var1` could be thought of a spatial location name, `Var2` and `Var3` can be thought of year and month and `X1`, `X2` and `X3` can be tought of measured values for given points in space and time. If you want to aggregate the values "a" and "b" of the categorical the variable `Var1`, summing the values of variables X4, X5 and X6, it can be done like this:

``` r
dplyrx::aggregate_by(data, Var1, Var2, Var3, expr = . %in% c("a", "b"))
```