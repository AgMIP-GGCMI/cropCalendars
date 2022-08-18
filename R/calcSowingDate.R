#' @title Calculate sowing date (Waha et al., 2012)
#'
#' @param monthly_temp Numeric vector of length 12. Average (e.g. 20-years)
#' monthly mean temperatures (degree Celsius).
#' @param monthly_ppet Numeric vector of length 12. Average (e.g. 20-years)
#' monthly P/PET ratio.
#' @param seasonality character value indicating the seasonality type as
#' computed by calcSeasonality
#' @export
calcSowingDate <- function(croppar,
                           monthly_temp,
                           monthly_ppet,
                           seasonality,
                           lat
                           ) {

  # Middle day of each month
  midday <- c(15, 43, 74, 104, 135, 165, 196, 227, 257, 288, 318, 349)

  # extract individual parameter names and values
  for (i in colnames(croppar)) {
    assign(i, croppar[[i]])
  }

  # Constrain first possible date for winter crop sowing
  earliest_sdate  <- ifelse(lat >= 0, initdate.sdatenh, initdate.sdatesh)
  earliest_smonth <- doy2month(earliest_sdate)
  DEFAULT_DOY     <- ifelse(lat >= 0, 1, 182)
  DEFAULT_MONTH   <- 0

  # What type of winter is it?
  if ((min(monthly_temp) > basetemp.low) &
      (seasonality %in% c("TEMP", "TEMPPREC", "PRECTEMP", "PREC"))) {
    # "Warm winter" (allowing non-vernalizing winter-sown crops)
    # sowing 2.5 months before coldest midday
    # it seems a good approximation for both India and South US)
    coldestday     <- midday[which.min(monthly_temp)]
    firstwinterdoy <- ifelse(coldestday-75<=0, coldestday-75+365, coldestday-75)

  } else if ((min(monthly_temp) < -10) &
             (seasonality %in% c("TEMP", "TEMPPREC", "PRECTEMP", "PREC"))) {
    # "Cold winter" (winter too harsh for winter crops, only spring sowing possible)
    firstwinterdoy <- -9999

  } else {
    # "Mild winter" (allowing vernalizing crops)
    firstwinterdoy <- calcDoyCrossThreshold(
      monthly_temp, temp_fall)[["doy_cross_down"]]

  }

  # First day of winter
  firstwintermonth <- ifelse(
    firstwinterdoy == -9999, DEFAULT_MONTH, doy2month(firstwinterdoy)
    )
  firstwinterdoy   <- ifelse(
    firstwinterdoy == -9999, DEFAULT_DOY, firstwinterdoy
    )

  # First day of spring
  firstspringdoy   <- calcDoyCrossThreshold(
    monthly_temp, temp_spring)[["doy_cross_up"]]
  firstspringmonth <- ifelse(
    firstspringdoy == -9999, DEFAULT_MONTH, doy2month(firstspringdoy)
    )

  # If winter type
  if (calcmethod_sdate == "WTYP_CALC_SDATE") {

    if (firstwinterdoy > earliest_sdate &
        firstwintermonth != DEFAULT_MONTH) {

      sowing_month  <- firstwintermonth
      sowing_doy    <- firstwinterdoy
      sowing_season <- "winter"

    } else if (firstwinterdoy <= earliest_sdate &
               min(monthly_temp) > temp_fall &
               firstwintermonth != DEFAULT_MONTH) {

      sowing_month  <- earliest_smonth
      sowing_doy    <- earliest_sdate
      sowing_season <- "winter"

    } else {

      sowing_month  <- firstspringmonth
      sowing_doy    <- firstspringdoy
      sowing_season <- "spring"

    }

  } else {

    if (seasonality == "NO_SEASONALITY") {

      sowing_month <- DEFAULT_MONTH
      sowing_doy <- DEFAULT_DOY
      sowing_season <- "spring"

    } else if (seasonality == "PREC" || seasonality == "PRECTEMP") {

      sowing_doy <- calcDoyWetMonth(monthly_ppet)
      sowing_month <- doy2month(sowing_doy)
      sowing_season <- "spring"

    } else {

      sowing_month <- firstspringmonth
      sowing_doy <- firstspringdoy
      sowing_season <- "spring"

    }
  } # STYP_CALC_SDATE

  sowing_doy    <- ifelse(
    sowing_month == DEFAULT_MONTH, DEFAULT_DOY, sowing_doy
    )
  sowing_month  <- ifelse(
    calcmethod_sdate == "WTYP_CALC_SDATE" & sowing_season == "spring",
    DEFAULT_MONTH, sowing_month
    )

  sd_vector <- list("sowing_month"  = sowing_month,
                    "sowing_doy"    = sowing_doy,
                    "sowing_season" = sowing_season)

  return(sd_vector)
}

