#' @title Calculate sunrise/sunset
#' @description Calculates sunrise or sunset at a specific geographical location and time.
#' depending on the `direction` parameter. Methods are available
#' for different object types with geographical coordinates, including:
#' * `sf`: an object of class `sf`.
#' * `matrix`: An unnamed matrix of coordinates, with each row containing a pair of geographical coordinates in `c(lon, lat)` order. See the example below.
#' * `SpatialPoints`: an object of class `SpatialPoints`.
#'
#' @inheritParams crepuscule
#' @param direction Character, determines whether to calculate the time of sunrise or sunset.
#' @references
#' NOAA [sunrise/sunset calculator](https://gml.noaa.gov/grad/solcalc/sunrise.html)
#' These algorithms include corrections for atmospheric refraction effects.
#' @details
#' Input can consist of one location and at least one `POSIXct` time, or one `POSIXct` time and at least
#' one location. `solarDep` is recycled as needed.
#' Do not use the daylight savings time zone string for supplying `dateTime`, as many OS will not be
#' able to properly set it to standard time when needed.
#' Compared to NOAA’s original Javascript code, the sunrise and sunset estimates from this translation
#' may differ by +/- 1 minute, based on tests using selected locations spanning the globe. This translation does not include calculation of prior or next sunrises/sunsets for locations above the Arctic
#' Circle or below the Antarctic Circle.
#' NOAA notes that “for latitudes greater than 72 degrees N and S, calculations are accurate to within
#' 10 minutes. For latitudes less than +/- 72 degrees accuracy is approximately one minute.”
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
              rownames(res) <- 1:nrow(res)
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
