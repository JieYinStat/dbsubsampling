
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbsubsampling <a href="https://jieyinstat.github.io/dbsubsampling/"><img src="man/figures/logo.png" align="right" height="139" alt="dbsubsampling website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/JieYinStat/dbsubsampling/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JieYinStat/dbsubsampling/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Providing a unified interface for IBOSS, Lowcon, OSS and other popular
design-based subsampling methods.

## Installation

You can install the development version of dbsubsampling from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JieYinStat/dbsubsampling")
```

## Example

This is a basic example which shows you how to get subsample index, such
as uniform sampling, OSMAC and IBOSS:

``` r
library(dbsubsampling)

data_binary <- data_binary_class

# Uniform sampling
subsampling(y_name = "y", data = data_binary, n = 10, method = "Unif", seed_1 = 123)
#>  [1] 2463 2511 8718 2986 1842 9334 3371 4761 6746 9819

# OSMAC-A
subsampling(y_name = "y", data = data_binary, n = 10, pilot_n = 100, method = "OSMAC_A", 
            seed_1 = 123, seed_2 = 456)
#>  [1] 5684 1620 5372 8297 8863 9783 6483 6103 2702 5735

# OSMAC-L
subsampling(y_name = "y", data = data_binary, n = 10, pilot_n = 100, method = "OSMAC_L",
            seed_1 = 123, seed_2 = 456)
#>  [1] 5813 1681 5372 8313 8863 9780 1630 6103 2702 5888

# IBOSS
data_numeric <- data_numeric_regression
subsampling(y_name = "y", data = data_numeric, n = 30, method = "IBOSS")
#>  [1]  419 1144 3395 3484 3896 5121 6203 7915 7967 8026 8156 8694 8841 9117 8438
#> [16] 3121
```

You can get more detailed examples from the article column on the
[website](jieyinstat.github.io/dbsubsampling/).
