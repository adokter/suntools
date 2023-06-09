#' suntools: calculate sun position, sunrise, sunset, solar noon and twilight
#'
#' @details
#' suntools provides functions for calculating the sun's position, sunrise, sunset,
#' solar noon, and crepuscular (twilight) times for any given geographical location and time on earth.
#'
#' Main package functions:
#' * [crepuscule()]: Calculates crepuscular (twilight) times, i.e. the time of dawn or dusk.
#' * [sunriset()]: Calculates the times of sunrise and sunset at a given location.
#' * [solarpos()]: Calculates the solar position (elevation and azimuth) at a given location and time.
#' * [solarnoon()]: Calculates solar noon time at a given location.
#'
#' @references
#' This package uses algorithms provided by the National Oceanic & Atmospheric Administration (NOAA), for more information see
#' * NOAA's [Sunrise/Sunset Calculator](https://gml.noaa.gov/grad/solcalc/sunrise.html)
#' * NOAA's [Solar Position Calculator](https://gml.noaa.gov/grad/solcalc/azel.html)
#' * Meeus, J. (1991) Astronomical Algorithms. Willmann-Bell, Inc.
#' 
#' @keywords internal
#' @importFrom sf st_crs st_coordinates st_as_sf
#' @importFrom methods is
#'
"_PACKAGE"
