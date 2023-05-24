# tests taken from the examples in sun-methods.Rd
hels <- matrix(c(24.97, 60.17), nrow = 1)
Hels <- sf::st_as_sf(as.data.frame(hels),
  coords = c(1, 2),
  crs = sf::st_crs(4326)
)
d041224 <- as.POSIXct("2004-12-24", tz = "EET")

test_that("crepuscule() calculates Astronomical dawn", {
  # allow for floating point errors in test
  expect_equal(
    crepuscule(
      hels,
      d041224,
      solarDep = 18,
      direction = "dawn",
      POSIXct.out = TRUE
    ),
    structure(
      list(
        day_frac = 0.276199993629954,
        time = structure(1103863063.67945,
          tzone = "EET",
          class = c("POSIXct", "POSIXt")
        )
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
})

test_that("crepuscule() can calculate Nautical dawn", {
  # allow for floating point errors in test
  expect_equal(
    crepuscule(hels, d041224, solarDep = 12, direction = "dawn", POSIXct.out = TRUE),
    structure(
      list(
        day_frac = 0.312182180938729,
        time = structure(1103866172.54043,
          tzone = "EET",
          class = c("POSIXct", "POSIXt")
        )
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
})

test_that("crepescule() can calulate Civil dawn", {
  # allow for floating point errors in test
  expect_equal(
    crepuscule(Hels, d041224, solarDep = 6, direction = "dawn", POSIXct.out = TRUE),
    structure(
      list(
        day_frac = 0.351924864850489,
        time = structure(1103869606.30832,
          tzone = "EET",
          class = c("POSIXct", "POSIXt")
        )
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
})

test_that("sunriset() can calculate the sunrise", {
  # allow for floating point errors in test
  expect_equal(
    sunriset(hels, d041224, direction = "sunrise", POSIXct.out = TRUE),
    structure(
      list(
        day_frac = 0.392424945734093,
        time = structure(1103873105.51531,
          tzone = "EET",
          class = c("POSIXct", "POSIXt")
        )
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
})

test_that("solarnoon() can calculate the solar noon", {
  # allow for floating point errors in test
  expect_equal(
    solarnoon(Hels, d041224, POSIXct.out = TRUE),
    structure(
      list(
        day_frac = 0.513796564070136,
        time = structure(1103883592.02314,
          tzone = "EET",
          class = c("POSIXct", "POSIXt")
        )
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
})

test_that("solarpos can caluclate the solar position", {
  # allow for floating point errors in test
  expect_equal(
    solarpos(hels, as.POSIXct("2023-05-24 10:29:07", tz = "CEST")),
    structure(c(184.478313910191, 50.5446683354915), dim = 1:2)
  )
})
