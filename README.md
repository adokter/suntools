
# suntools

<!-- badges: start -->
[![R-CMD-check](https://github.com/adokter/suntools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adokter/suntools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Suntools is a package derived from the larger `maptools` package, now deprecated. It provides a set of functions
for calculating sunrise, sunset, and times of dawn and dusk, with flexibility for the various formal definitions. They use algorithms provided by the National Oceanic & Atmospheric Administration (NOAA).

Major functions in the package include:
* `crepuscule()`: Calculates crepuscular times, i.e. the time of dawn or dusk.
* `sunriset()`: Calculates the times of sunrise and sunset at a given geographical location. 
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

## Author(s)

- Sebastian P. Luque ([spluque@gmail.com](mailto:spluque@gmail.com)), translated from Greg Pelletier's code    
- Greg Pelletier ([gpel461@ecy.wa.gov](mailto:gpel461@ecy.wa.gov)) - Original VBA code available from [Ecology WA Gov: Models & Tools for TMDLs](https://ecology.wa.gov/Research-Data/Data-resources/Models-spreadsheets/Modeling-the-environment/Models-tools-for-TMDLs), who in turn translated it from original Javascript code by NOAA (see Details)   
- Roger Bivand ([roger.bivand@nhh.no](mailto:roger.bivand@nhh.no)) adapted the code to work with sp classes.

## References

NOAA's [Sunrise/Sunset Calculator](https://gml.noaa.gov/grad/solcalc/sunrise.html) and [Solar Position Calculator](https://gml.noaa.gov/grad/solcalc/azel.html) were developed based on the reference provided below. These calculators incorporate algorithms that adjust for the effects of atmospheric refraction.

Meeus, J. (1991) Astronomical Algorithms. Willmann-Bell, Inc.