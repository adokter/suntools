#' @title Compute crepuscular time
#' @description Calculates the crepuscular time, i.e., the time of dawn or dusk, 
#' at a specific geographical location and time. Methods are available for different 
#' object types with geographical coordinates, including:
#' * `sf`: an object of class `sf`.
#' * `matrix`: An unnamed matrix of coordinates, with each row containing a pair of geographical coordinates in `c(lon, lat)` order. See the example below.
#' * `SpatialPoints`: an object of class `SpatialPoints`.
#'
#' @param crds Geographical coordinates. It can be an object of
#' class `sf`, `matrix`, or `SpatialPoints`.
#' @param dateTime A `POSIXct` object representing the date and time. It specifies
#' the moment for which the crepuscular time is calculated.
#' @param ... Additional arguments that are passed to methods. 
#' @param solarDep A numerical value representing the solar depression angle.
#' @param direction A character string representing the `direction`, either "dawn" or "dusk".
#' @param POSIXct.out Logical, if TRUE, the result is returned as a `POSIXct` object, 
#' otherwise, it is returned as a fraction of a day.
#' @return The function returns the time of crepuscular light, either as a fraction of a day
#' or as a `POSIXct` object, depending on the `POSIXct.out` parameter.
#' @rdname crepuscule
#' @export
setGeneric("crepuscule", function(crds, dateTime, ...) {
    standardGeneric("crepuscule")
})

#' @rdname crepuscule
setMethod("crepuscule",
          signature(crds="sf", dateTime="POSIXct"),
          function(crds, dateTime, solarDep,
                   direction=c("dawn", "dusk"), POSIXct.out=FALSE) {
              if (is.na(sf::st_crs(crds)))
                stop("crds must be geographical coordinates")
              if (missing(solarDep)) stop("solarDep must be given")
              crdsmtx <- sf::st_coordinates(crds)
              eq.ll <- .balanceCrdsTimes(crdsmtx, dateTime)
              time.ll <- .timeData(eq.ll$dateTime)
              lon <- eq.ll$crds[, 1]
              lat <- eq.ll$crds[, 2]
              direction <- match.arg(direction)
              res <- .crepuscule(lon=lon, lat=lat, year=time.ll$year,
                                 month=time.ll$month, day=time.ll$day,
                                 timezone=time.ll$timezone,
                                 dlstime=time.ll$dlstime,
                                 solarDep=solarDep, direction=direction)
              if (POSIXct.out) {
                secs <- res * 86400
                if (is.null(time.ll$tz)) Pct <- as.POSIXct(format(dateTime,
                                                                  "%Y-%m-%d")) + secs
                else Pct <- as.POSIXct(format(dateTime, "%Y-%m-%d"),
                                       tz=time.ll$tz) + secs
                res <- data.frame(day_frac=res, time=Pct)
              }
              res
          })

#' @rdname crepuscule
#' @param crs A `CRS` object representing the coordinate reference system.
#' Default is `sf::st_crs(4326)` which denotes WGS84 (World Geodetic System 1984).
#' @examples
#'#Civil dawn in Ithaca, NY on June 1, 2023
#' 
#'crepuscule(
#'        matrix(c(-76.4511, 42.4800), nrow = 1),
#'        as.POSIXct("2023-06-01", tz = "America/New_York"),
#'        solarDep = 6,
#'        direction = "dawn",
#'        POSIXct.out = TRUE
#'      )
#' 
setMethod("crepuscule", signature(crds="matrix", dateTime="POSIXct"),
          function(crds, dateTime,
                   crs=sf::st_crs(4326), solarDep,
                   direction=c("dawn", "dusk"), POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(as.data.frame(crds),coords=c(1,2), crs=crs)
              direction <- match.arg(direction)
              crepuscule(crds.sf, dateTime=dateTime, solarDep=solarDep,
                         direction=direction, POSIXct.out=POSIXct.out)
          })

#' @rdname crepuscule
setMethod("crepuscule", signature(crds="SpatialPoints", dateTime="POSIXct"),
          function(crds, dateTime, solarDep,
                   direction=c("dawn", "dusk"), POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(crds)
              direction <- match.arg(direction)
              crepuscule(crds.sf, dateTime=dateTime, solarDep=solarDep,
                         direction=direction, POSIXct.out=POSIXct.out)
          })

#' @title Calculate sunrise/sunset
#' @description Calculates sunrise or sunset at a specific geographical location and time.
#' depending on the `direction` parameter. Methods are available
#' for differet object types with geographical coordinates, including:
#' * `sf`: an object of class `sf`.
#' * `matrix`: An unnamed matrix of coordinates, with each row containing a pair of geographical coordinates in `c(lon, lat)` order. See the example below.
#' * `SpatialPoints`: an object of class `SpatialPoints`.
#'
#' @param crds Geographical coordinates. It can be an object of
#' class `sf`, `matrix`, or `SpatialPoints`.
#' @param dateTime A `POSIXct` object representing the date and time. It specifies
#' the moment for which the sunriset is calculated.
#' @param ... Additional arguments that are passed to methods. 
#' @param direction Character, determines whether to calculate the time of sunrise or sunset.
#' @param POSIXct.out Logical, if `TRUE`, the result is returned as a `POSIXct` object, otherwise, it is returned as a fraction of a day.
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
              res
          })

#' @param crs A "CRS" object representing the coordinate reference system.
#' Default is `sf::st_crs(4326)` which denotes WGS84 (World Geodetic System 1984).
#' @rdname sunriset
#' @examples
#' Sunset in Ithaca, NY, USA on June 1, 2023
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

#' @title Compute solar noon time
#' @description Calculates the solar noon, i.e., the time when the sun is at its highest point in the sky. 
#' at a specific geographical location and time. Methods are available for differet object types with
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
