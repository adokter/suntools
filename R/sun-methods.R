".balanceCrdsTimes" <- function(crds, dateTime)
{
    ## Value: list with crds and dateTime input matrices with equal number
    ## of rows
    ## --------------------------------------------------------------------
    ## Arguments: crds=matrix with lon and lat; dateTime=matrix with year,
    ## month, day, timezone, and dlstime rows, or a POSIXct time
    ## --------------------------------------------------------------------
    ncrds <- nrow(crds)
    nTimes <- ifelse(is(dateTime, "POSIXct"), length(dateTime), nrow(dateTime))
    if (ncrds == 1 && nTimes > 1) {
        crds <- crds[rep(1, nTimes), ]
    } else if (ncrds > 1 && nTimes == 1) {
        dateTime <- if (is(dateTime, "POSIXct")) {
            dateTime[rep(1, ncrds)]
        } else dateTime[rep(1, ncrds), ]
    } else if (ncrds != nTimes) {
        stop("mismatch in number of coordinates and times")
    }
    list(crds=crds, dateTime=dateTime)
}

###_ + crepuscule methods
setGeneric("crepuscule", function(crds, dateTime, ...) {
    standardGeneric("crepuscule")
})

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


setMethod("crepuscule", signature(crds="matrix", dateTime="POSIXct"),
          function(crds, dateTime,
                   crs=sf::st_crs(4326), solarDep,
                   direction=c("dawn", "dusk"), POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(as.data.frame(crds),coords=c(1,2), crs=crs)
              direction <- match.arg(direction)
              crepuscule(crds.sf, dateTime=dateTime, solarDep=solarDep,
                         direction=direction, POSIXct.out=POSIXct.out)
          })

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
#' depending on the "direction" parameter. Methods are available
#' for differet object types with geographical coordinates, including:
#' * "sf": an object of class "sf".
#' * "matrix": a matrix of coordinates.
#' * "SpatialPoints": an object of class "SpatialPoints".
#'
#' @param crds This represents geographical coordinates. It can be an object of
#' class "sf", "matrix", or "SpatialPoints".
#' @param dateTime A POSIXct object representing the date and time. It specifies
#' the moment for which the sunriset is calculated.
#' @param direction Character, determines whether to calculate the time of sunrise or sunset.
#' @param POSIXct.out Logical, if TRUE, the result is returned as a POSIXct object, otherwise, it is returned as a fraction of a day.
#' @return The function returns the time of sunriset, either as a fraction of a day
#' or as a POSIXct object, depending on the "POSIXct.out" parameter.
#' @rdname sunriset
#' @export
setGeneric("sunriset", function(crds, dateTime, ...) {
    standardGeneric("sunriset")
})

#' @description Method for sf objects.
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

#' @description Method for matrix objects. 
#' @param crs A "CRS" object representing the coordinate reference system.
#' Default is sf::st_crs(4326).
#' @rdname sunriset
setMethod("sunriset", signature(crds="matrix", dateTime="POSIXct"),
          function(crds, dateTime,
                   crs=sf::st_crs(4326),
                   direction=c("sunrise", "sunset"), POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(as.data.frame(crds),coords=c(1,2), crs=crs)
              direction <- match.arg(direction)
              sunriset(crds.sf, dateTime=dateTime,
                       direction=direction, POSIXct.out=POSIXct.out)
          })

#' @description Method for SpatialPoints objects.
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
#' * "sf": an object of class "sf".
#' * "matrix": a matrix of coordinates.
#' * "SpatialPoints": an object of class "SpatialPoints".
#'
#' @param crds This represents geographical coordinates. It can be an object of
#' class "sf", "matrix", or "SpatialPoints".
#' @param dateTime A POSIXct object representing the date and time. It specifies
#' the moment for which the solar noon is calculated.
#' @param POSIXct.out Logical, if TRUE, the result is returned as a POSIXct object, otherwise, it is returned as a fraction of a day.
#' @return The function returns the time of solar noon, either as a fraction of a day
#' or as a POSIXct object, depending on the "POSIXct.out" parameter.
#' @rdname solarnoon
#' @export
setGeneric("solarnoon", function(crds, dateTime, ...) {
    standardGeneric("solarnoon")
})

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

#' @description Method for matrix objects. 
#' @param crs A "CRS" object representing the coordinate reference system.
#' Default is sf::st_crs(4326).
#' @rdname solarnoon
setMethod("solarnoon", signature(crds="matrix", dateTime="POSIXct"),
          function(crds, dateTime,
                   crs=sf::st_crs(4326),
                   POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(as.data.frame(crds),coords=c(1,2), crs=crs)
              solarnoon(crds.sf, dateTime=dateTime,
                        POSIXct.out=POSIXct.out)
          })

#' @description Method for SpatialPoints objects.
#' @rdname solarnoon
setMethod("solarnoon", signature(crds="SpatialPoints", dateTime="POSIXct"),
          function(crds, dateTime, POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(crds)
              solarnoon(crds.sf, dateTime=dateTime,
                        POSIXct.out=POSIXct.out)
          })

###_ + solarpos methods
setGeneric("solarpos", function(crds, dateTime, ...) {
    standardGeneric("solarpos")
})


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

setMethod("solarpos", signature(crds="matrix", dateTime="POSIXct"),
          function(crds, dateTime,
                   crs=sf::st_crs(4326), ...) {
              crds.sf <- st_as_sf(as.data.frame(crds),coords=c(1,2), crs=crs)
              solarpos(crds.sf, dateTime=dateTime)
          })

setMethod("solarpos", signature(crds="SpatialPoints", dateTime="POSIXct"),
          function(crds, dateTime, ...) {
              crds.sf <- st_as_sf(crds)
              solarpos(crds.sf, dateTime=dateTime)
          })
