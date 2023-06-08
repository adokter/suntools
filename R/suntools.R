#' Suntools: A package for calculating the sun's position relative to earth
#'
#' @details
#' Suntools is a package derived from the larger maptools package, now deprecated. It provides a set of functions
#' for calculating the sun's position, sunrise, sunset, polar noon, and crepuscular times
#' for any given geographical location and time on earth.
#'
#' Major functions in the package include:
#' * `crepuscule()`: Calculates crepuscular times, i.e. the time of dawn or dusk.
#' * `sunrise()`: Calculates the times of sunrise and sunset at a given geographical location. 
#' * `solarpos()`: Calculates the solar position, including the sun's elevation and azimuth at a 
#' specific geographical location and time.
#' * `solarnoon()`: Calculates solar noon time at a given geographical location.
#'
#' This package is particularly useful for studies related to solar energy,
#' environmental science, astronomy, and related fields.
#'
#' For more details on specific functions, refer to their respective help pages.
#' 
#' @references
#' The original maptools package: \url{https://CRAN.R-project.org/package=maptools}
#' 
#' @keywords internal
#' @importFrom sf st_crs st_coordinates st_as_sf
#' @importFrom methods is
#'
"_PACKAGE"