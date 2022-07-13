#' @title Tests if given season should be classified as winter crop
#'
#' @param start Sowing date as day of the year (DOY)
#' @param end Harvest (or maturity) date as day of the year (DOY)
#'
#' @details This is the rule suggested by Portman et al. 2010, slightly
#' changed in that <= 7 instead of 6°C is used.
#' @export
isWinterCrop <- function(start, end, tcm, lat) {

  # tcm = temp of coldest month
  # start / end = sdate / hdate

  growp <- ifelse(start <= end, end - start, 365 + end - start)
  wc <- 0

  if (!is.na(start) && start > 0 && !is.na(lat) && !is.na(tcm)) {

    if (lat > 0) {

      if ( ((start + growp > 365) && (growp >= 150)) &&
           (tcm >= -10 && tcm <= 7) ) {
        wc <- 1
      }

    } else {

      if ( ((start < 182) && (start + growp > 182) && (growp >= 150)) &&
           (tcm >= (-10) && tcm <= 7) ) {
        wc <- 1
      }

    }
  }

  return(wc)

}
