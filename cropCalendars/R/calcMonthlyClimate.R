#' @title Calculate monthly climate
#'
#' @description Calculate monthly climate variables needed for the computation
#' of the rule-based crop calendars (Waha et al., 2012; Minoli et al., 2019)
#' @param lat latitude (decimal value)
#' @param temp daily temperature (degree Celsius) for a number of years. It should be passed
#' in form of a matrix with dimensions [nyears, 365].
#' @param temp daily precipitation for a number of years. It should be passed
#' in form of a matrix with dimensions [nyears, 365].
#' @param syear start year in the climate time series.
#' @param eyear end yeat in the climate time series.
#' @examples
#' d_temp <- matrix(rnorm(365*3, 15), nrow = 3)
#' d_prec <- matrix(rnorm(365*3, 3, 50), nrow = 3)
#' d_prec[d_prec <= 0] <- 0
#' calcMonthlyClimate(lat = 45, temp = d_temp, prec = d_prec,
#'                    syear = 2001, eyear = 2003)
#' @export

calcMonthlyClimate <- function(lat,
                               temp,
                               prec,
                               syear,
                               eyear) {

  years   <- syear:eyear
  nyears  <- length(years)
  days    <- seq_len(365)
  ndays   <- length(days)
  nmonths <- 12

  # Compute PET (Potential ET)
  cat("Computing daily PET ...\n")
  pet <- array(NA, dim = dim(temp))
  for (yy in seq_len(nyears)) {
    for (dd in seq_len(ndays)) {
      pet[yy, dd] <- calcPET(temp = temp[yy, dd], lat = lat, day = dd)
    }
  }

  # Compute monthly climate for each year
  cat("Computing monthly climate ...\n")

  mtemp_y <- array(NA, dim = c(nyears, nmonths))
  mprec_y <- mpet_y <- mppet_y <- mtemp_y


  for (yy in seq_len(nyears)) {
    for (mm in seq_len(nmonths)) {

      # which days belong to month mm
      mtemp_y[yy, mm] <- mean(temp) # mean temp
      mprec_y[yy, mm] <- sum(prec)  # cumulative pr
      mpet_y[yy, mm]  <- sum(pet)   # cumulative pet
      mppet_y[yy,mm]  <- mprec_y[yy, mm]/mpet_y[yy, mm]          # ppet ratio

    }
  }

  # Compute 20-years Average
  cat("Computing climate 20-years mean ...\n")
  mtemp      <- round(apply(mtemp_y, 2, mean), digits = 5)
  mprec      <- round(apply(mprec_y, 2, mean), digits = 5)
  mpet       <- round(apply(mpet_y,  2, mean), digits = 5)
  mppet      <- round(apply(mppet_y, 2, mean), digits = 5)
  mppet_diff <- mppet - c(mppet[-1], mppet[1])

  names(mtemp) <- names(mprec) <- c("month" = seq_len(12))
  names(mpet)  <- names(mppet) <- c("month" = seq_len(12))
  names(mppet_diff) <- c("month" = seq_len(12))

  return(list(mtemp      = mtemp,
              mprec      = mprec,
              mpet       = mpet,
              mppet      = mppet,
              mppet_diff = mppet_diff
              )
         )
}
