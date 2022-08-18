#' @title Calculate day of crossing threshold
#'
#' @description Calculate days where thresholds are crossed from monthly values
#'
#' @param monthly_value numeric vector of length 12 representing monthly value
#' of a variable.
#' @param threshold numeric value representing a threshold.
#'
#' @export
calcDoyCrossThreshold <- function(monthly_value,
                                  threshold
                                  ) {

  ndays_year  <- 365
  daily_value <- interpolateMonthlyToDaily(monthly_value)

  # Find days when value above threshold
  is_value_above  <- daily_value$y >= threshold
  is_value_above2 <- c(is_value_above[length(is_value_above)],
                       is_value_above[1:(length(is_value_above) - 1)])

  # Find days when value crosses threshold
  value_cross_threshold <- is_value_above - is_value_above2
  day_cross_up   <- daily_value[["x"]][which(value_cross_threshold == 1)]
  day_cross_down <- daily_value[["x"]][which(value_cross_threshold == -1)]

  # Convert values to 1:365
  iup <- day_cross_up > ndays_year
  idw <- day_cross_down > ndays_year
  day_cross_up[iup]   <- day_cross_up[iup] - ndays_year
  day_cross_down[idw] <- day_cross_down[idw] - ndays_year
  day_cross_up <- sort(unique(day_cross_up))[1]
  day_cross_down <- sort(unique(day_cross_down))[1]

  # No crossing == -9999
  if ( is.na(day_cross_up)   == TRUE ) day_cross_up   <- -9999
  if ( is.na(day_cross_down) == TRUE ) day_cross_down <- -9999

  return(list("doy_cross_up"   = day_cross_up,
              "doy_cross_down" = day_cross_down))
}
