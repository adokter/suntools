# define a location in Helsinki
hels <- matrix(c(24.97, 60.17), nrow = 1)
Hels <- sf::st_as_sf(as.data.frame(hels),
  coords = c(1, 2),
  crs = sf::st_crs(4326)
)
d041224 <- as.POSIXct("2004-12-24", tz = "Europe/Helsinki")

# define a location near Sydney, Australia
Sydney <- sf::st_as_sf(data.frame(lon=153.5885, lat=-28.84979),coords = c(1, 2), crs = sf::st_crs(4326))

# define a location in Ithaca NY, USA
Ithaca <- sf::st_as_sf(data.frame(lon=-76.5019, lat=42.443),coords = c(1, 2), crs = sf::st_crs(4326))

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
          tzone = "Europe/Helsinki",
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
          tzone = "Europe/Helsinki",
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
          tzone = "Europe/Helsinki",
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
          tzone = "Europe/Helsinki",
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
          tzone = "Europe/Helsinki",
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
    solarpos(hels, as.POSIXct("2023-05-24 10:29:07", tz = "UTC")),
    structure(c(184.478313910191, 50.5446683354915), dim = 1:2)
  )
})

test_that("daylight savings time is accounted for", {
  # allow for floating point errors in test
  ithaca_st1 <- sunriset(Ithaca, as.POSIXct("2025-03-08", tz="America/New_York"), POSIXct.out = TRUE)
  ithaca_dst1 <- sunriset(Ithaca, as.POSIXct("2025-03-09", tz="America/New_York"), POSIXct.out = TRUE)
  ithaca_dst2 <- sunriset(Ithaca, as.POSIXct("2025-11-01", tz="America/New_York"), POSIXct.out = TRUE)
  ithaca_st2 <- sunriset(Ithaca, as.POSIXct("2025-11-02", tz="America/New_York"), POSIXct.out = TRUE)
  sydney_dst1 <- sunriset(Sydney, as.POSIXct("2025-04-05", tz="Australia/Sydney"), POSIXct.out = TRUE)
  sydney_st1 <- sunriset(Sydney, as.POSIXct("2025-04-06", tz="Australia/Sydney"), POSIXct.out = TRUE)
  sydney_st2 <- sunriset(Sydney, as.POSIXct("2025-10-04", tz="Australia/Sydney"), POSIXct.out = TRUE)
  sydney_dst2 <- sunriset(Sydney, as.POSIXct("2025-10-05", tz="Australia/Sydney"), POSIXct.out = TRUE)
  expect_equal(as.numeric(format(sydney_dst1$time,"%H"))-as.numeric(format(sydney_st1$time,"%H")),1)
  expect_equal(as.numeric(format(sydney_dst2$time,"%H"))-as.numeric(format(sydney_st2$time,"%H")),1)
  expect_equal(as.numeric(format(ithaca_dst1$time,"%H"))-as.numeric(format(ithaca_st1$time,"%H")),1)
  expect_equal(as.numeric(format(ithaca_dst2$time,"%H"))-as.numeric(format(ithaca_st2$time,"%H")),1)
})

