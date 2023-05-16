\name{sun-methods}
\docType{methods}

\alias{crepuscule}
\alias{sunriset}
\alias{solarnoon}
\alias{solarpos}

\alias{crepuscule-methods}
\alias{crepuscule,sf,POSIXct-method}
\alias{crepuscule,matrix,POSIXct-method}
\alias{crepuscule,SpatialPoints,POSIXct-method}
\alias{solarnoon-methods}
\alias{solarnoon,sf,POSIXct-method}
\alias{solarnoon,matrix,POSIXct-method}
\alias{solarnoon,SpatialPoints,POSIXct-method}
\alias{solarpos-methods}
\alias{solarpos,sf,POSIXct-method}
\alias{solarpos,matrix,POSIXct-method}
\alias{solarpos,SpatialPoints,POSIXct-method}
\alias{sunriset-methods}
\alias{sunriset,sf,POSIXct-method}
\alias{sunriset,matrix,POSIXct-method}
\alias{sunriset,SpatialPoints,POSIXct-method}

\title{Methods for sun ephemerides calculations}


\description{Functions for calculating sunrise, sunset, and times of
  dawn and dusk, with flexibility for the various formal definitions.
  They use algorithms provided by the National Oceanic & Atmospheric
  Administration (NOAA).}


\usage{

\S4method{crepuscule}{sf,POSIXct}(crds, dateTime, solarDep, direction=c("dawn", "dusk"),
           POSIXct.out=FALSE)
\S4method{crepuscule}{matrix,POSIXct}(crds, dateTime,
           crs=sf::st_crs(4326), solarDep,
           direction=c("dawn", "dusk"), POSIXct.out=FALSE)
\S4method{crepuscule}{SpatialPoints,POSIXct}(crds, dateTime, solarDep, direction=c("dawn", "dusk"),
           POSIXct.out=FALSE)
\S4method{sunriset}{sf,POSIXct}(crds, dateTime, direction=c("sunrise", "sunset"),
         POSIXct.out=FALSE)
\S4method{sunriset}{matrix,POSIXct}(crds, dateTime,
         crs=sf::st_crs(4326),
         direction=c("sunrise", "sunset"), POSIXct.out=FALSE)
\S4method{sunriset}{SpatialPoints,POSIXct}(crds, dateTime, direction=c("sunrise", "sunset"),
         POSIXct.out=FALSE)
\S4method{solarnoon}{sf,POSIXct}(crds, dateTime, POSIXct.out=FALSE)
\S4method{solarnoon}{matrix,POSIXct}(crds, dateTime,
          crs=sf::st_crs(4326),
          POSIXct.out=FALSE)
\S4method{solarnoon}{SpatialPoints,POSIXct}(crds, dateTime, POSIXct.out=FALSE)
\S4method{solarpos}{sf,POSIXct}(crds, dateTime, ...)
\S4method{solarpos}{matrix,POSIXct}(crds, dateTime,
         crs=sf::st_crs(4326), ...)
\S4method{solarpos}{SpatialPoints,POSIXct}(crds, dateTime, ...)

}


\arguments{

  \item{crds}{a \code{sf} or \code{matrix} object, containing
    x and y coordinates (in that order).}

  \item{dateTime}{a POSIXct object with the date and time associated to
    calculate ephemerides for points given in crds.}

  \item{solarDep}{numeric vector with the angle of the sun below the
    horizon in degrees.}

  \item{direction}{one of "dawn", "dusk", "sunrise", or "sunset",
    indicating which ephemerides should be calculated.}

  \item{POSIXct.out}{logical indicating whether POSIXct output should be
    included.}

  \item{crs}{object of class \code{crs} describing the
    projection of data in \code{crds}}

  \item{\dots}{other arguments passed through.}

}


\details{

  NOAA used the reference below to develop their Sunrise/Sunset

  \url{https://gml.noaa.gov/grad/solcalc/sunrise.html} and Solar
  Position 

  \url{https://gml.noaa.gov/grad/solcalc/azel.html}
  Calculators.  The algorithms include corrections for atmospheric
  refraction effects.

  Input can consist of one location and at least one POSIXct times, or one
  POSIXct time and at least one location.  \var{solarDep} is recycled as
  needed.

  Do not use the daylight savings time zone string for supplying
  \var{dateTime}, as many OS will not be able to properly set it to
  standard time when needed.

}


\section{Warning}{Compared to NOAA's original Javascript code, the
  sunrise and sunset estimates from this translation may differ by +/- 1
  minute, based on tests using selected locations spanning the globe.
  This translation does not include calculation of prior or next
  sunrises/sunsets for locations above the Arctic Circle or below the
  Antarctic Circle.}


\value{

  \code{crepuscule}, \code{sunriset}, and \code{solarnoon} return a
  numeric vector with the time of day at which the event occurs,
  expressed as a fraction, if POSIXct.out is FALSE; otherwise they
  return a data frame with both the fraction and the corresponding
  POSIXct date and time.

  \code{solarpos} returns a matrix with the solar azimuth (in degrees
  from North), and elevation.

}


\note{NOAA notes that \dQuote{for latitudes greater than 72 degrees N
    and S, calculations are accurate to within 10 minutes.  For
    latitudes less than +/- 72 degrees accuracy is approximately one
    minute.}}


\references{
  Meeus, J.  (1991)  Astronomical Algorithms.  Willmann-Bell, Inc.
}

\author{Sebastian P. Luque \email{spluque@gmail.com}, translated from
  Greg Pelletier's \email{gpel461@ecy.wa.gov} VBA code (available from
  \url{https://ecology.wa.gov/Research-Data/Data-resources/Models-spreadsheets/Modeling-the-environment/Models-tools-for-TMDLs}), who in turn
  translated it from original Javascript code by NOAA (see Details).
  Roger Bivand \email{roger.bivand@nhh.no} adapted the code to work with
  \pkg{sp} classes. Adriaan Dokter \email{amd427@cornell.edu} adapted the
  code to work with \pkg{sf} classes.}

\examples{
library(sf)
## Location of Helsinki, Finland, in decimal degrees,
## as listed in NOAA's website
hels <- matrix(c(24.97, 60.17), nrow=1)
Hels <- st_as_sf(as.data.frame(hels),coords=c(1,2), crs=st_crs(4326))
d041224 <- as.POSIXct("2004-12-24", tz="EET")
## Astronomical dawn
crepuscule(hels, d041224, solarDep=18, direction="dawn", POSIXct.out=TRUE)
crepuscule(Hels, d041224, solarDep=18, direction="dawn", POSIXct.out=TRUE)
## Nautical dawn
crepuscule(hels, d041224, solarDep=12, direction="dawn", POSIXct.out=TRUE)
crepuscule(Hels, d041224, solarDep=12, direction="dawn", POSIXct.out=TRUE)
## Civil dawn
crepuscule(hels, d041224, solarDep=6, direction="dawn", POSIXct.out=TRUE)
crepuscule(Hels, d041224, solarDep=6, direction="dawn", POSIXct.out=TRUE)
solarnoon(hels, d041224, POSIXct.out=TRUE)
solarnoon(Hels, d041224, POSIXct.out=TRUE)
solarpos(hels, as.POSIXct(Sys.time(), tz="EET"))
solarpos(Hels, as.POSIXct(Sys.time(), tz="EET"))
sunriset(hels, d041224, direction="sunrise", POSIXct.out=TRUE)
sunriset(Hels, d041224, direction="sunrise", POSIXct.out=TRUE)
## Using a sequence of dates
Hels_seq <- seq(from=d041224, length.out=365, by="days")
up <- sunriset(Hels, Hels_seq, direction="sunrise", POSIXct.out=TRUE)
down <- sunriset(Hels, Hels_seq, direction="sunset", POSIXct.out=TRUE)
day_length <- down$time - up$time
plot(Hels_seq, day_length, type="l")

}

\keyword{methods}
\keyword{manip}
\keyword{utilities}
