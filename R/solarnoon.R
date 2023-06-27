#' @title Compute solar noon time
#' @description Calculates the solar noon, i.e., the time when the sun is at its highest point in the sky
#' at a specific geographical location and time.
#' @inheritParams sunriset
#' @inherit sunriset details
#' @inherit solarpos references
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
              } else {
              res <- res['day_frac']
            }
            return(res)
           }
          )

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
