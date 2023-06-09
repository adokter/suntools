#' @title Compute solar position
#' @description Calculates the solar position, i.e., the sun's elevation and azimuth, at a specific geographical location and time. 
#' Methods are available for different object types with geographical coordinates, including:
#' * `sf`: an object of class `sf`.
#' * `matrix`: An unnamed matrix of coordinates, with each row containing a pair of geographical coordinates in `c(lon, lat)` order. See the example below.
#' * `SpatialPoints`: an object of class `SpatialPoints`.
#'
#' @inheritParams crepuscule
#' @references
#' NOAA [solar position calculator](https://gml.noaa.gov/grad/solcalc/azel.html)
#' These algorithms include corrections for atmospheric refraction effects.
#' @details
#' Input can consist of one location and at least one `POSIXct` time, or one `POSIXct` time and at least
#' one location. #' Do not use the daylight savings time zone string for supplying `dateTime`, as many OS will not be
#' able to properly set it to standard time when needed.
#' NOAA notes that “for latitudes greater than 72 degrees N and S, calculations are accurate to within
#' 10 minutes. For latitudes less than +/- 72 degrees accuracy is approximately one minute.”
#' @return Returns a matrix with the solar azimuth (in degrees from North), and elevation.
#' @rdname solarpos
#' @export
setGeneric("solarpos", function(crds, dateTime, ...) {
    standardGeneric("solarpos")
})

#' @rdname solarpos
setMethod("solarpos", signature(crds="sf", dateTime="POSIXct"),
          function(crds, dateTime, ...) {
              if (is.na(sf::st_crs(crds)))
                  stop("crds must be geographical coordinates")
              crdsmtx <- sf::st_coordinates(crds)
              eq.ll <- .balanceCrdsTimes(crdsmtx, dateTime)
              time.ll <- .timeData(eq.ll$dateTime)
              lon <- eq.ll$crds[, 1]
              lat <- eq.ll$crds[, 2]
              res <- .solarpos(lon=lon, lat=lat, year=time.ll$year,
                               month=time.ll$month, day=time.ll$day,
                               hours=time.ll$hour, minutes=time.ll$min,
                               seconds=time.ll$sec, timezone=time.ll$timezone,
                               dlstime=time.ll$dlstime)
              matrix(c(azimuth=res[, 1], elevation=res[, 2]), ncol=2)
          })

#' @param crs A `CRS` object representing the coordinate reference system.
#' Default is `sf::st_crs(4326)`.
#' @rdname solarpos
#' @examples
#' # Solar position in Ithaca, NY, USA on June 1, 2023 at 08:00:00
#' 
#' solarpos(
#'  matrix(c(-76.4511, 42.4800), nrow = 1),
#'  as.POSIXct("2023-06-01 08:00:00", tz = "America/New_York")
#' )
setMethod("solarpos", signature(crds="matrix", dateTime="POSIXct"),
          function(crds, dateTime,
                   crs=sf::st_crs(4326), ...) {
              crds.sf <- st_as_sf(as.data.frame(crds),coords=c(1,2), crs=crs)
              solarpos(crds.sf, dateTime=dateTime)
          })

#' @rdname solarpos
setMethod("solarpos", signature(crds="SpatialPoints", dateTime="POSIXct"),
          function(crds, dateTime, ...) {
              crds.sf <- st_as_sf(crds)
              solarpos(crds.sf, dateTime=dateTime)
          })
