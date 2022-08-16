#' @title Calculate Potential Evapo-Transpiration (PET)
#'
#' @description Calculate potential ET as in LPJmL numeric/petpar.c
#'
#' @param temp mean temperature (degree Celsius)
#' @param lat latitude
#' @param day day of the year (DOY)
#'
#' @export
calcPET <- function(temp,
                    lat,
                    day
                    ) { #(kg H2O m-2 d-1 = mm d-1)

  ndays_year <- 365
  M_1_PI     <- 0.318309886183790671538
  beta       <- 0.17
  a          <- 107.0 #
  b          <- 0.2 #
  qoo        <- 1360.0 #  solar constant (1360 W/m2)
  c          <- 0.25 #
  d          <- 0.5 #
  k          <- 13750.98708 # conversion factor from solar angular units to seconds (12/pi*3600)
  sun        <- 0.01

  gamma_t <- 65.05 + temp * 0.064 #psychrometer constant (Pa K-1)
  lambda  <- 2.495e6 - temp * 2380 #latent heat of vaporization of water (J kg-1)

  delta <- deg2rad(-23.4 * cos(2 * pi * (day + 10.0) / ndays_year))
  u <- sin(deg2rad(lat)) * sin(delta)
  v <- cos(deg2rad(lat)) * cos(delta)
  # net short wave
  w <- (c + d * sun) * (1 - beta) * qoo *
    (1.0 + 2.0 * 0.01675 * cos(2.0 * pi * day / ndays_year))

  if (u >= v) {
    daylength <- 24
    par <- w / (1 - beta) * u * pi * k
  } else if (u <= -v) {
    daylength <- par <- 0
  } else {
    hh <- acos(-u / v)
    par <- w / (1 - beta) * (u * hh + v * sin(hh)) * k
    daylength <- 24 * hh * M_1_PI
  }

  u <- w * u - (b + (1 - b) * sun) * (a - temp)
  v <- w

  #  polar night
  if (u <= -v) {
    eeq <- 0
  } else {
    s <- 2.503e6 * exp(17.269 * temp / (237.3 + temp)) / ((237.3 + temp) * (237.3 + temp))
    if (u >= v) {
      eeq <- 2 * (s / (s + gamma_t) / lambda) * u * pi * k
    } else {
        hh <- acos(-u / v)
        eeq <- 2 * (s / (s + gamma_t) / lambda) * (u * hh + v * sin(hh)) * k
      }
  }

return(eeq)

}
