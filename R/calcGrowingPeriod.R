#' @title Calculate growing-period duration
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
