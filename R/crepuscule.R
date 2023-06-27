#' @title Compute crepuscular time
#' @description Calculates the crepuscular time, i.e., the time of dawn or dusk
#' at a specific geographical location and time.
#' @inheritParams sunriset
#' @param solarDep A numerical value representing the solar depression angle.
#' @inherit sunriset details references
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
              rownames(res) <- 1:nrow(res)
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
