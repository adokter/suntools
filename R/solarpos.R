#' @title Compute solar position
#' @description Calculates the solar position, i.e., the sun's elevation and azimuth, at a specific geographical location and time. 
#' Methods are available for different object types with geographical coordinates, including:
#' * `sf`: an object of class `sf`.
#' * `matrix`: An unnamed matrix of coordinates, with each row containing a pair of geographical coordinates in `c(lon, lat)` order. See the example below.
#' * `SpatialPoints`: an object of class `SpatialPoints`.
#'
#' @param crds Geographical coordinates. It can be an object of
#' class `sf`, `matrix`, or `SpatialPoints`.
#' @param dateTime A `POSIXct` object representing the date and time. It specifies
#' the moment for which the solar position is calculated.
#' @param ... Additional arguments that are passed to methods. 
#' @return The function returns a matrix with the sun's azimuth and elevation.
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
