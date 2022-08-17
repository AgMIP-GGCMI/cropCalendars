test_that("calcMonthlyClimate returns a list", {
  # Read sample daily climate
  syear <- 1991
  eyear <- 2000
  grid_sample <- data.frame(lon = -93.25, lat = 37.25)
  # Temperature
  temp_fn <- paste0(
  "../testdata/", "temp_", "lon", grid_sample$lon, "_lat", grid_sample$lat, "_", syear, "_", eyear, ".csv"
  )
  d_temp <- unlist(read.csv(temp_fn, header = F))
  # Precipitation
  prec_fn <- paste0(
  "../testdata/", "prec_", "lon", grid_sample$lon, "_lat", grid_sample$lat, "_", syear, "_", eyear, ".csv"
  )
  d_prec <- unlist(read.csv(prec_fn, header = F))

  # Calculate monthly climate
  mclm <- cropCalendars::calcMonthlyClimate(
    lat = grid_sample$lat,
    temp = d_temp,
    prec = d_prec,
    syear = syear,
    eyear = eyear
    )
  expect_type(mclm, "list")
}
)