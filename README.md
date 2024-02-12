
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
subsampling(y_name = "y", data = data, n = 10, method = "Unif", seed_1 = 123)
#>  [1] 2463 2511 8718 2986 1842 9334 3371 4761 6746 9819
# OSMAC-A
subsampling(y_name = "y", data = data, n = 10, pilot_n = 100,
  method = "OSMAC_A", seed_1 = 123, seed_2 = 456)
#>  [1] 5684 1620 5372 8297 8863 9783 6483 6103 2702 5735
# OSMAC-L
subsampling(y_name = "y", data = data, n = 10, pilot_n = 100,
  method = "OSMAC_L", seed_1 = 123, seed_2 = 456)
#>  [1] 5813 1681 5372 8313 8863 9780 1630 6103 2702 5888
```

You can get more detailed examples from the article column on the
[website](jieyinstat.github.io/dbsubsampling/).
