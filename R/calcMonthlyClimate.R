#' @title Calculate monthly climate
#'
#' @description Calculate monthly climate variables needed for the computation
#' of the rule-based crop calendars (Waha et al., 2012; Minoli et al., 2019)
#'
#' @param lat latitude (decimal value)
#' @param temp daily temperature (degree Celsius) for a number of years
#' (syear:eyear). It should be passed in form of a vector.
#' @param prec daily precipitation (mm) for a number of years (syear:eyear).
#' It should be passed in form of a vector.
#' @param syear start year in the climate time series.
#' @param eyear end year in the climate time series.
#' @param incl_feb29 Does the time series include February 29th in leap years?
#' @examples
#' d_temp <- matrix(rnorm(365*3, 15), nrow = 3)
#' d_prec <- matrix(rnorm(365*3, 3, 50), nrow = 3)
#' d_prec[d_prec <= 0] <- 0
#' calcMonthlyClimate(lat = 45, temp = d_temp, prec = d_prec,
#'                    syear = 2001, eyear = 2003)
#' @export

calcMonthlyClimate <- function(lat        = NULL,
                               temp       = NULL,
                               prec       = NULL,
                               syear      = NULL,
                               eyear      = NULL,
                               incl_feb29 = TRUE
                               ) {

  years   <- syear:eyear
  nyears  <- length(years)
  nmonths <- 12

  if (incl_feb29 == FALSE) {
    dates <- createDateSeq(nstep = 365, years = years)

  } else {
    dates <- seqDates(start_date = paste0(syear, "-01-01"),
                      end_date   = paste0(eyear, "-12-31"),
                      step       = "day")
  }
  y_dates <- date_to_year(dates)
  m_dates <- date_to_month(dates)
  d_dates <- date_to_doy(dates, skip_feb29 = TRUE)


  # Compute daily PET (Potential ET)
  pet <- mapply(calcPET, temp = temp, lat = lat, day = d_dates)

  # Compute monthly climate for each year
  mtemp_y <- array(NA, dim = c(nyears, nmonths))
  mprec_y <- mpet_y <- mppet_y <- mtemp_y

  for (yy in seq_len(nyears)) {
    for (mm in seq_len(nmonths)) {

      # which days belong to year yy and month mm
      idx <- which(y_dates == years[yy] & m_dates == mm)

      mtemp_y[yy, mm] <- mean(temp[idx]) # mean temp
      mprec_y[yy, mm] <- sum(prec[idx])  # cumulative pr
      mpet_y[yy, mm]  <- sum(pet[idx])   # cumulative pet
      mppet_y[yy,mm]  <- mprec_y[yy, mm]/mpet_y[yy, mm]          # ppet ratio

    }
  }

  # Compute 20-years Average
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
