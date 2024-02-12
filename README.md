
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbsubsampling

<!-- badges: start -->

[![R-CMD-check](https://github.com/JieYinStat/dbsubsampling/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JieYinStat/dbsubsampling/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of dbsubsampling is to provides a unified interface for IBOSS,
Lowcon, OSS and other popular design-based subsampling methods.

## Installation

You can install the development version of dbsubsampling from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JieYinStat/dbsubsampling")
```

## Example

This is a basic example which shows you how to get subsample index, such
as uniform sampling and OSMAC:

``` r
library(dbsubsampling)

data <- data_binary_class
# Uniform sampling
subsampling(y_name = "y", data = data, n = 30, method = "Unif", seed_1 = 123)
#>  [1] 2463 2511 8718 2986 1842 9334 3371 4761 6746 9819 2757 5107 9145 9209 2888
#> [16] 6170 2567 9642 9982 2980 1614  555 4469 9359 7789 9991 9097 1047 7067 3004
# OSMAC-A
subsampling(y_name = "y", data = data, n = 30, pilot_n = 100,
  method = "OSMAC_A", seed_1 = 123, seed_2 = 456)
#>  [1] 5684 1620 5372 8297 8863 9783 6483 6103 2702 5735 9382   40 9919 8623 2816
#> [16] 5035 6088 2006 4702 1993 4279 9827 8738 8892 7632 6836 6393 6405   99 3952
# OSMAC-L
subsampling(y_name = "y", data = data, n = 30, pilot_n = 100,
  method = "OSMAC_L", seed_1 = 123, seed_2 = 456)
#>  [1] 5813 1681 5372 8313 8863 9780 1630 6103 2702 5888 9382 9843 9913 8635 2816
#> [16] 5035 6211 2090 4702 2083 4385 9813 8776 8904 4425 6899 1615 6513   99 4076
```

You can get more detailed examples from the article column on the
[website](jieyinstat.github.io/dbsubsampling/).
