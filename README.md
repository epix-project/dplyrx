
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dplyrx <img src="man/figures/logo.png" align="right" width=120/>

[![Travis-CI Build
Status](https://travis-ci.org/epix-project/dplyrx.svg?branch=master)](https://travis-ci.org/epix-project/dplyrx)
[![AppVeyor Build
status](https://ci.appveyor.com/api/projects/status/y8exchgxy2rugjw8/branch/master?svg=true)](https://ci.appveyor.com/project/epixproject/dplyrx/branch/master)
[![Codecov test
coverage](https://img.shields.io/codecov/c/github/epix-project/dplyrx/master.svg)](https://codecov.io/github/epix-project/dplyrx?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dplyrx)](https://cran.r-project.org/package=dplyrx)

`dplyrx` is a collection of functions that extend those in the
[`dplyr`](https://dplyr.tidyverse.org) package.

## Installation

You can install dplyrx from github with:

``` r
# install.packages("devtools")
devtools::install_github("epix-project/dplyrx")
```

To load the package:

``` r
library(dplyrx)
```

## Example

Below is a basic example which shows you how to solve a common problem.
Imagine that you have a data frame that looks like this:

``` r
set.seed(30101976)
data <- expand.grid(letters[1:3], 1:3, 4:6)
data <- cbind(data, data.frame(replicate(3, sample(1:100, nrow(data), TRUE))))
```

where `Var1` could be thought of a spatial location name, `Var2` and
`Var3` can be thought of year and month and `X1`, `X2` and `X3` can be
tought of measured values for given points in space and time. If you
want to aggregate the values “a” and “b” of the categorical the variable
`Var1`, summing the values of variables X4, X5 and X6, it can be done
like this:

``` r
data <- transform(data, Var1 =  ifelse(data$Var1 == "a", "b",
                                        as.character(data$Var1)))
aggregate_by(data, Var1, Var2, Var3)
#>    Var1 Var2 Var3  X1  X2  X3
#> 1     b    1    4  77 136 142
#> 7     b    1    5 119 116 106
#> 13    b    1    6  14 111 107
#> 3     b    2    4  77 122 117
#> 9     b    2    5  78  33  98
#> 15    b    2    6 113 144 171
#> 5     b    3    4  54  81  82
#> 11    b    3    5 139 138 123
#> 17    b    3    6  49  47  58
#> 2     c    1    4  49  12  62
#> 8     c    1    5   5  27  17
#> 14    c    1    6  41  10  43
#> 4     c    2    4  42   9   6
#> 10    c    2    5  27  35  52
#> 16    c    2    6  35  38   3
#> 6     c    3    4  16   2  96
#> 12    c    3    5  21   5  10
#> 18    c    3    6  28  27   7
```
