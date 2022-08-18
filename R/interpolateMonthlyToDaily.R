#' @title Interpolate monthly values to daily values
#'
#' @description Daily values are obtained by linear interpolation of monthly
#' values. Monthly values are assumed to refer to the mid-day of each month.
#' In order to allow interpolation also between December and January, monthly
#' values are repeated twice.
#'
#' @param monthly_value Numeric vector of monthly values of a variable,
#' typically a climate variable (e.g. temperature).
#'
#' @return list of two vectors (x and y) each of length 722. x indicates the
#' day of the year, y indicates the interpolated daily value.
#'
#' @export
interpolateMonthlyToDaily <- function(monthly_value
                                      ) {

  # Middle day of each month
  midday   <- c(15, 43, 74, 104, 135, 165, 196, 227, 257, 288, 318, 349)
  ndays_year <- 365

  # Replicate values twice (two years)
  midday2 <- c(midday[1:12], midday[1:12] + ndays_year)
  monthly_value2 <- rep(monthly_value, 2)

  # Interpolate monthly to daily values
  daily_value <- list(x = NULL, y = NULL)
  for (i in seq_len(length(monthly_value2[-1]))) {
    m1 <- i
    m2 <- i + 1
    value <- approx(x      = c(midday2[m1:m2]),
                    y      = monthly_value2[m1:m2],
                    method = "linear",
                    n      = midday2[m2] - midday2[m1] + 1)
    daily_value[["x"]] <- c(daily_value[["x"]], value[["x"]])
    daily_value[["y"]] <- c(daily_value[["y"]], value[["y"]])
  }
  return(daily_value)
}
