
# suntools

<!-- badges: start -->
[![R-CMD-check](https://github.com/adokter/suntools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adokter/suntools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

suntools provides functions for calculating the sun's position, sunrise, sunset, solar noon, and crepuscular (twilight)
times for any given geographical location and time on earth. Functions in suntools used to be part 
of the [maptools](https://CRAN.R-project.org/package=maptools) package.
The functions are based on equations provided by the National Oceanic & Atmospheric Administration (NOAA).

Main package functions:
* `crepuscule()`: Calculates crepuscular (twilight) times, i.e. the time of dawn or dusk.
* `sunriset()`: Calculates the times of sunrise and sunset at a given location.
* `solarpos()`: Calculates the solar position (elevation and azimuth) at a given location and time.
* `solarnoon()`: Calculates solar noon time at a given location.

## Installation
You can install the released version of suntools from [CRAN](https://CRAN.R-project.org/) with:

``` r
install.packages("suntools")
```

Alternatively, you can install the development version from [GitHub](https://github.com/) with:

``` r
devtools::install_github("adokter/suntools")
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

- Greg Pelletier ([gpel461@ecy.wa.gov](mailto:gpel461@ecy.wa.gov)) - Original VBA code available from [Ecology WA Gov: Models & Tools for TMDLs](https://ecology.wa.gov/Research-Data/Data-resources/Models-spreadsheets/Modeling-the-environment/Models-tools-for-TMDLs), who in turn translated it from original Javascript code by NOAA
- Sebastian P. Luque ([spluque@gmail.com](mailto:spluque@gmail.com)), translated from Greg Pelletier's code    
- Roger Bivand ([roger.bivand@nhh.no](mailto:roger.bivand@nhh.no)) adapted the code to work with sp classes.
- Adriaan M. Dokter ([amd427@cornell.edu](mailto:amd427@cornell.edu)) adapted the code to work with sf classes.
- Pieter Huijbrechts ([pieter.huybrechts@inbo.be](mailto:pieter.huybrechts@inbo.be)) set up continuous integration and automated tests.
- Alexander Tedeschi ([at744@cornell.edu](mailto:at744@cornell.edu)) made tests and documentation roxygen compatible

## References

This package uses algorithms provided by the National Oceanic & Atmospheric Administration (NOAA), for more information see

* Meeus, J. (1991) Astronomical Algorithms. Willmann-Bell, Inc.
* NOAA's [Sunrise/Sunset Calculator](https://gml.noaa.gov/grad/solcalc/sunrise.html)
* NOAA's [Solar Position Calculator](https://gml.noaa.gov/grad/solcalc/azel.html)
* NOAA'S [Solar Calculation Details](https://gml.noaa.gov/grad/solcalc/calcdetails.html)
* NOAA'S [Atmospheric Refraction Effects](https://gml.noaa.gov/grad/solcalc/calcdetails.html)

For sunrise and sunset calculations, we assume 0.833° of atmospheric refraction. In the solar position calculator, atmospheric refraction is modeled as:

| Solar Elevation | Approximate Atmospheric Refraction Correction (°) |
| --- | --- |
| 85° to 90° | 0 |
| 5° to 85° | $\frac{1}{3600}\left(\frac{58.1}{\tan(h)} - \frac{0.07}{\tan^3(h)} + \frac{0.000086}{\tan^5(h)}\right)$ |
| -0.575° to 5° | $\frac{1}{3600}\left(1735 - 518.2h + 103.4h^2 - 12.79h^3 + 0.711h^4\right)$ |
| < -0.575° | $\frac{1}{3600}\left(\frac{-20.774}{\tan(h)}\right)$ |

The effects of the atmosphere vary with atmospheric pressure, humidity and other variables. Therefore the solar position calculations presented here are approximate. Errors in sunrise and sunset times can be expected to increase the further away you are from the equator, because the sun rises and sets at a very shallow angle. Small variations in the atmosphere can have a larger effect.





