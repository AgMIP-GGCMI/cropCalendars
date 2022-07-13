#' @title Unit conversion functions
#'
#' @description Collection of functions for unit conversions.
#'
#' @param doy Day of the year. Integer value between 1 and 365.

#' @export

# Dates
doy2month <- function(doy  = 1,
                      year = 2015
) {
  mon <- strptime(paste(year, doy), format = "%Y %j")$mon+1
  return(mon)
}

# Temperature
k2deg   <- function(t_kelvin) {t_kelvin - 273.15}
deg2k   <- function(t_deg) {t_deg + 273.15}

# Area
ha2kmq  <- function(ha) {ha / 100}
kmq2ha  <- function(kmq) {kmq * 100}

# Degrees
deg2rad <- function(deg) {deg * pi * 0.00555555555555555555}
