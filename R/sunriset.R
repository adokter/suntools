#' @title Calculate sunrise/sunset
#' @description Calculates sunrise or sunset at a specific geographical location and time
#' depending on the `direction` parameter.
#' @param crds Geographical coordinates. It can be an object of
#' class `sf`, `matrix`, or `SpatialPoints`.
#' @param dateTime A `POSIXct` object representing the date and time.
#' @param direction Character, determines whether to calculate the time of sunrise or sunset.
#' @param POSIXct.out Logical, if `TRUE`, the result is returned as a `POSIXct` object, otherwise, it is returned as a fraction of a day.
#' @param ... Additional arguments that are passed to methods.
#' @references
#' * Meeus, J. (1991) Astronomical Algorithms. Willmann-Bell, Inc.
#' * NOAA's [sunrise/sunset calculator](https://gml.noaa.gov/grad/solcalc/sunrise.html).
#' These algorithms include corrections for atmospheric refraction effects.
#' * NOAA's [solar calculations details](https://gml.noaa.gov/grad/solcalc/calcdetails.html)
#' @details
#' Methods are available for different classes of geographical coordinates, including:
#' * `sf`: an object of class `sf`.
#' * `matrix`: An unnamed matrix of coordinates, with each row containing a pair of geographical coordinates in `c(lon, lat)` order. See the example below.
#' * `SpatialPoints`: an object of class `SpatialPoints`.
#' Input can consist of one location and at least one `POSIXct` time, or one `POSIXct`
#' time and at least one location. `solarDep` is recycled as needed.
#' Do not use the daylight savings time zone string for supplying `dateTime`, as many OS
#' will not be able to properly set it to standard time when needed.
#'
#' Compared to NOAA’s original Javascript code, the sunrise and sunset estimates from this translation
#' may differ by +/- 1 minute, based on tests using selected locations spanning the globe.
#' This translation does not include calculation of prior or next sunrises/sunsets for locations above the Arctic
#' Circle or below the Antarctic Circle.
#'
#' ## Solar position calculation
#'
#' Details for the calculations are provided by NOAA [here](https://gml.noaa.gov/grad/solcalc/calcdetails.html),
#' which we repeat below as a reference.
#'
#' The calculations in the NOAA Sunrise/Sunset and Solar Position Calculators are based on equations
#' from Astronomical Algorithms, by Jean Meeus. The sunrise and sunset results are theoretically accurate
#' to within a minute for locations between +/- 72° latitude, and within 10 minutes outside of those latitudes.
#' However, due to variations in atmospheric composition, temperature, pressure and conditions, observed values may vary from calculations.
#'
#' For the purposes of these calculators the current Gregorian calendar is extrapolated backward through time.
#' When using a date before 15 October, 1582, you will need to correct for this. The year preceding year 1 in
#' the calendar is year zero (0). The year before that is -1. The approximations used in these programs are very
#' good for years between 1800 and 2100. Results should still be sufficiently accurate for the range
#' from -1000 to 3000. Outside of this range, results may be given, but the potential for error is higher.
#'
#' ## Atmospheric refraction correction
#'
#' For sunrise and sunset calculations, we assume 0.833° of atmospheric refraction.
#' In the solar position calculator, atmospheric refraction is modeled as:
#'
#'  | Solar Elevation | Approximate Atmospheric Refraction Correction (°) |
#'  | --- | --- |
#'  | 85° to 90° | 0 |
#'  | 5° to 85° | \eqn{\frac{1}{3600}\left(\frac{58.1}{\tan(h)} - \frac{0.07}{\tan^3(h)} + \frac{0.000086}{\tan^5(h)}\right)} |
#'  | -0.575° to 5° | \eqn{\frac{1}{3600}\left(1735 - 518.2 h + 103.4 h^2 - 12.79 h^3 + 0.711 h^4\right)} |
#'  | < -0.575° | \eqn{\frac{1}{3600}\left(\frac{-20.774}{\tan(h)}\right)} |
#'
#' The effects of the atmosphere vary with atmospheric pressure, humidity and other variables.
#' Therefore the solar position calculations presented here are approximate. Errors in sunrise
#' and sunset times can be expected to increase the further away you are from the equator,
#' because the sun rises and sets at a very shallow angle. Small variations in the atmosphere
#' can have a larger effect.
#' @return The function returns the time of sunriset, either as a fraction of a day
#' or as a `POSIXct` object, depending on the `POSIXct.out` parameter.
#' @rdname sunriset
#' @export
setGeneric("sunriset", function(crds, dateTime, ...) {
    standardGeneric("sunriset")
})

#' @rdname sunriset
setMethod("sunriset", signature(crds="sf", dateTime="POSIXct"),
          function(crds, dateTime, direction=c("sunrise", "sunset"),
                   POSIXct.out=FALSE) {
              if (is.na(sf::st_crs(crds)))
                  stop("crds must be geographical coordinates")
              crdsmtx <- sf::st_coordinates(crds)
              eq.ll <- .balanceCrdsTimes(crdsmtx, dateTime)
              time.ll <- .timeData(eq.ll$dateTime)
              lon <- eq.ll$crds[, 1]
              lat <- eq.ll$crds[, 2]
              direction <- match.arg(direction)
              res <- .sunriset(lon=lon, lat=lat, year=time.ll$year,
                               month=time.ll$month, day=time.ll$day,
                               timezone=time.ll$timezone,
                               dlstime=time.ll$dlstime,
                               direction=direction)
              if (POSIXct.out) {
                  secs <- res * 86400
                  if (is.null(time.ll$tz)) Pct <- as.POSIXct(format(dateTime,
                       "%Y-%m-%d")) + secs
                  else Pct <- as.POSIXct(format(dateTime, "%Y-%m-%d"),
                       tz=time.ll$tz) + secs
                  res <- data.frame(day_frac=res, time=Pct)
              }
              res <- as.data.frame(res, col.names=c('day_frac'))
              res
          })

#' @param crs A "CRS" object representing the coordinate reference system.
#' Default is `sf::st_crs(4326)` which denotes WGS84 (World Geodetic System 1984).
#' @rdname sunriset
#' @examples
#' #Sunset in Ithaca, NY, USA on June 1, 2023
#'
#'sunriset(
#'  matrix(c(-76.4511, 42.4800), nrow = 1),
#'  as.POSIXct("2023-06-01", tz = "America/New_York"),
#'  direction='sunset',
#'  POSIXct.out=TRUE
#')
setMethod("sunriset", signature(crds="matrix", dateTime="POSIXct"),
          function(crds, dateTime,
                   crs=sf::st_crs(4326),
                   direction=c("sunrise", "sunset"), POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(as.data.frame(crds),coords=c(1,2), crs=crs)
              direction <- match.arg(direction)
              sunriset(crds.sf, dateTime=dateTime,
                       direction=direction, POSIXct.out=POSIXct.out)
          })

#' @rdname sunriset
setMethod("sunriset", signature(crds="SpatialPoints", dateTime="POSIXct"),
          function(crds, dateTime, direction=c("sunrise", "sunset"),
                   POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(crds)
              direction <- match.arg(direction)
              sunriset(crds.sf, dateTime=dateTime,
                       direction=direction, POSIXct.out=POSIXct.out)
          })
