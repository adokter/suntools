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
if (!isGeneric("crepuscule")) {
    setGeneric("crepuscule", function(crds, dateTime, ...) {
        standardGeneric("crepuscule")
    })
}

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

###_ + sunriset methods
if (!isGeneric("sunriset")) {
    setGeneric("sunriset", function(crds, dateTime, ...) {
        standardGeneric("sunriset")
    })
}

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

setMethod("sunriset", signature(crds="matrix", dateTime="POSIXct"),
          function(crds, dateTime,
                   crs=sf::st_crs(4326),
                   direction=c("sunrise", "sunset"), POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(as.data.frame(crds),coords=c(1,2), crs=crs)
              direction <- match.arg(direction)
              sunriset(crds.sf, dateTime=dateTime,
                       direction=direction, POSIXct.out=POSIXct.out)
          })

setMethod("sunriset", signature(crds="SpatialPoints", dateTime="POSIXct"),
          function(crds, dateTime, direction=c("sunrise", "sunset"),
                   POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(crds)
              direction <- match.arg(direction)
              sunriset(crds.sf, dateTime=dateTime,
                       direction=direction, POSIXct.out=POSIXct.out)
          })


###_ + solarnoon methods
if (!isGeneric("solarnoon")) {
    setGeneric("solarnoon", function(crds, dateTime, ...) {
        standardGeneric("solarnoon")
    })
}

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

setMethod("solarnoon", signature(crds="matrix", dateTime="POSIXct"),
          function(crds, dateTime,
                   crs=sf::st_crs(4326),
                   POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(as.data.frame(crds),coords=c(1,2), crs=crs)
              solarnoon(crds.sf, dateTime=dateTime,
                        POSIXct.out=POSIXct.out)
          })

setMethod("solarnoon", signature(crds="SpatialPoints", dateTime="POSIXct"),
          function(crds, dateTime, POSIXct.out=FALSE) {
              crds.sf <- st_as_sf(crds)
              solarnoon(crds.sf, dateTime=dateTime,
                        POSIXct.out=POSIXct.out)
          })

###_ + solarpos methods
if (!isGeneric("solarpos")) {
    setGeneric("solarpos", function(crds, dateTime, ...) {
        standardGeneric("solarpos")
    })
}

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
