#' @title Calculate Required Vernalization Days
#'
#' @param temp_mean_month  = sample(-10:30, 12), # monthly mean temperature
#' @param max.vern.days maximum vernalization requirements (days)
#' @param max.vern.months maximum vernalization
#' @param tv2 vern.temp.opt.min
#' @param tv3 vern.temp.opt.max
#'
#'@export
calcVd <- function(temp_mean_month  = sample(-10:30, 12), # monthly mean temperature
                   max.vern.days   = 70,     # maximum vernalization requirements (days)
                   max.vern.months = 5,      # maximum vernalization
                   tv2             = 3,      # vern.temp.opt.min
                   tv3             = 10      # vern.temp.opt.max
                   ) {

  max.vern.days.month <- max.vern.days/max.vern.months # Vsat_max [days per month]

  # Find indices of 5 coldest months
  temp     <- array(0,c(12,2))
  temp[,1] <- temp_mean_month
  temp[,2] <- c(1:12)
  sort_temp      <- temp[order(temp[,1]),]
  coldest_months <- sort_temp[1:5,2]

  # Sum up effective vernalization
  days_required <- 0

  # Loop through coldest months and compute vernalization days per month
  for (m in 1:max.vern.months) {

    temp_coldest_month_m <- temp_mean_month[coldest_months[m]]

    if       (temp_coldest_month_m <= tv2) {

      days <- max.vern.days.month

    } else if(temp_coldest_month_m >= tv3) {

      days <- 0

    } else if(temp_coldest_month_m > tv2 &
              temp_coldest_month_m < tv3) {

      days <- max.vern.days.month*(1-(temp_coldest_month_m-tv2)/(tv3-tv2))

    }

    # Add vernalization days of month m
    days_required <- days_required + days

  }

  # Required Vernalization Days
  vd <- round(days_required)

  return(vd)

} # calc.vreq
