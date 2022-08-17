#' @title Calculate seasonality type
#'
#' @description Calculate seasonality type based on average monthly climate
#' as from Waha et al. (2012).
#'
#' @param monthly_temp Numeric vector of length 12. Average (e.g. 20-years)
#' monthly mean temperatures (degree Celsius).
#' @param monthly_prec Numeric vector of length 12. Average (e.g. 20-years)
#' monthly cumulative precipitation (mm).
#' @param temp_min Threshold of temperature of the coldest month
#' (degree Celsius). Default value is 10.
#'
#' @export
calcSeasonality <- function(monthly_temp,
                            monthly_prec,
                            temp_min = 10
                            ) {

  var_coeff_prec <- calcVarCoeff(monthly_prec)
  var_coeff_temp <- calcVarCoeff(deg2k(monthly_temp))
  min_temp       <- min(monthly_temp)

  if      (var_coeff_prec <= 0.4 && var_coeff_temp <= 0.010) {

    seasonality <- "NO_SEASONALITY"

  } else if (var_coeff_prec > 0.4 && var_coeff_temp <= 0.010)  {

    seasonality <- "PREC"

  } else if (var_coeff_prec > 0.4 &&
             (var_coeff_temp > 0.010 && min_temp > temp_min)) {

    seasonality <- "PRECTEMP"

  } else if (var_coeff_prec > 0.4 &&
           (var_coeff_temp > 0.010 && min_temp <= temp_min)) {

    seasonality <- "TEMPPREC"

  }  else {

    seasonality <- "TEMP"

    }

  return(seasonality)
}
