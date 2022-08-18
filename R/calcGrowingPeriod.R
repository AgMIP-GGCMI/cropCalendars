#' @title Calculate growing-period length
#'
#' @description Calculates growing period (days) from sowing to harvest.
#'
#' @param sowing numerical value 1:365 (DOY) or 1:12 (month)
#' @param harvest numerical value 1:365 (DOY) or 1:12 (month)
#' @param max_time_unit e.g. 365 (days), 12 (month)
#'
#' @export
calcGrowingPeriod <- function(sowing,
                              harvest,
                              max_time_unit
                              ) {

  growperiod <- ifelse(
    sowing <= harvest, harvest - sowing, max_time_unit + harvest - sowing
    )
  return(growperiod)
}
