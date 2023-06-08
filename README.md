
# suntools

<!-- badges: start -->
[![R-CMD-check](https://github.com/adokter/suntools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adokter/suntools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Suntools is a package derived from the larger `maptools` package, now deprecated. It provides a set of functions
for calculating the sun's position, sunrise, sunset, polar noon, and crepuscular times
for any given geographical location and time on earth.

Major functions in the package include:
* `crepuscule()`: Calculates crepuscular times, i.e. the time of dawn or dusk.
* `sunrise()`: Calculates the times of sunrise and sunset at a given geographical location. 
* `solarpos()`: Calculates the solar position, including the sun's elevation and azimuth at a 
specific geographical location and time.
* `solarnoon()`: Calculates solar noon time at a given geographical location.

This package is particularly useful for studies related to solar energy,
environmental science, astronomy, and related fields.

For more details on specific functions, refer to their respective help pages.

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