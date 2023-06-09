#' @title Compute solar noon time
#' @description Calculates the solar noon, i.e., the time when the sun is at its highest point in the sky. 
#' at a specific geographical location and time. Methods are available for different object types with
#' geographical coordinates, including:
#' * `sf`: an object of class `sf`.
#' * `matrix`: An unnamed matrix of coordinates, with each row containing a pair of geographical coordinates in `c(lon, lat)` order. See the example below.
#' * `SpatialPoints`: an object of class `SpatialPoints`.
#'
#' @param crds Geographical coordinates. It can be an object of
#' class `sf`, `matrix`, or `SpatialPoints`.
#' @param dateTime A `POSIXct` object representing the date and time. It specifies
#' the moment for which the solar noon is calculated.
#' @param ... Additional arguments that are passed to methods. 
#' @param POSIXct.out Logical, if `TRUE`, the result is returned as a `POSIXct` object, otherwise, it is returned as a fraction of a day.
#' @details
#' Input can consist of one location and at least one `POSIXct`` time, or one `POSIXct`` time and at least
#' one location. Do not use the daylight savings time zone string for supplying `dateTime`, as many OS will not be
#' able to properly set it to standard time when needed.
#' NOAA notes that “for latitudes greater than 72 degrees N and S, calculations are accurate to within
#' 10 minutes. For latitudes less than +/- 72 degrees accuracy is approximately one minute.”
#' @return The function returns the time of solar noon, either as a fraction of a day
#' or as a `POSIXct` object, depending on the `POSIXct.out` parameter.
#' @rdname solarnoon
#' @export
setGeneric("solarnoon", function(crds, dateTime, ...) {
    standardGeneric("solarnoon")
})

#' @rdname solarnoon
setMethod("solarnoon", signature(crds="sf", dateTime="POSIXct"),
          function(crds, dateTime, POSIXct.out=FALSE) {
              if (is.na(sf::st_crs(crds)))
                  stop("crds must be geographical coordinates")
              crdsmtx <- sf::st_coordinates(crds)
              eq.ll <- .balanceCrdsTimes(crdsmtx, dateTime)
              time.ll <- .timeData(eq.ll$dateTime)
              lon <- eq.ll$crds[, 1]
              lat <- eq.ll$crds[, 2]
              res <- .solarnoon(lon=lon, lat=lat, year=time.ll$year,
                                month=time.ll$month, day=time.ll$day,
                                timezone=time.ll$timezone,
                                dlstime=time.ll$dlstime)
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

#' @param crs A `CRS` object representing the coordinate reference system.
#' Default is `sf::st_crs(4326)` which denotes WGS84 (World Geodetic System 1984).
#' @rdname solarnoon
#' @examples
#'# Solar noon in Ithaca, NY, USA on June 1, 2023
#' 
#'solarnoon(
#'  matrix(c(-76.4511, 42.4800), nrow = 1),
#'  as.POSIXct("2023-06-01", tz = "America/New_York"),
#'  POSIXct.out=TRUE
#')
setMethod("solarnoon", signature(crds="matrix", dateTime="POSIXct"),
          function(crds, dateTime,
                   crs=sf::st_crs(4326),
                   POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(as.data.frame(crds),coords=c(1,2), crs=crs)
              solarnoon(crds.sf, dateTime=dateTime,
                        POSIXct.out=POSIXct.out)
          })

#' @rdname solarnoon
setMethod("solarnoon", signature(crds="SpatialPoints", dateTime="POSIXct"),
          function(crds, dateTime, POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(crds)
              solarnoon(crds.sf, dateTime=dateTime,
                        POSIXct.out=POSIXct.out)
          })
