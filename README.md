
# suntools

<!-- badges: start -->
[![R-CMD-check](https://github.com/adokter/suntools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adokter/suntools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Skeleton package to continue sun/solar functions and methods from maptools.

## Installation

You can install the development version of suntools from [GitHub](https://github.com/) with:

``` r
devtools::install_github("adokter/suntools")
```

You can install the released version of suntools from [CRAN](https://CRAN.R-project.org/) with:

``` r
install.packages("suntools")
```

## Example

``` r
library(suntools)

#Calculate sunset in Ithaca, NY, USA on June 1, 2023

sunriset(
  matrix(c(-76.4511, 42.4800), nrow = 1),
  as.POSIXct("2023-06-01", tz = "America/New_York"),
  direction='sunset',
  POSIXct.out=TRUE
)

#  day_frac                time
#1 0.858628 2023-06-01 20:36:25

```