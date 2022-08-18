#' @title Calculate beginning of the rainy season
#'
#' @description Find first day of 120 wettest days, calculated as the maximum
#' 120-days cumulative sum of the precipitation to potential evapotranspiration
#' (P/PET) ratio.
#'
#' @param monthly_value numeric vector of length 12 representing monthly
#' value of mppet as calculated by calcMonthlyClimate.
#' @export

calcDoyWetMonth <- function(monthly_value
                            ) {

  doys <- 1:365
  daily_value <- interpolateMonthlyToDaily(monthly_value)[["y"]]

  x <- NULL
  for (i in 0:364) {
    x <- c(x, sum(daily_value[c(1:120) + i]))
  }

  return(doys[which(x == max(x))[1]])

}
