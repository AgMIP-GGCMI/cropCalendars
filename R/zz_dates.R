#' @title Create a character vector of dates.
#'
#' @description Create a vector of dates to be used as
#' e.g. names of a time dimension. Dates are written as.characters, not as.Date,
#' in order to avoid issues with leap years. By default, for annual/monthly outputs,
#' it returns the last day of the year/month.
#' @return A character vector of dates in the format "YYYY-MM-DD".
#' @param nstep An integer value defining the time step of the output file.
#' Valid values are 1 (yearly), 12 (monthly), 365 (daily).
#' @param years An integer vector of (sequential or non-sequential) years.
#' @seealso strptime, as.Date, format
#' @export

createDateSeq <- function(nstep = 365,
                          years = 2000
                          ) {

  # Number of days per month
  ndays_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  # Days and months in two-digits format (e.g. "01")
  dd <- sprintf("%02d", unlist(lapply(ndays_in_month, FUN = seq_len)))
  mm <- sprintf("%02d", seq_len(12))

  # daily data: YYYY-MM-DD
  d_mmdd     <- paste(rep(mm, times = ndays_in_month), dd, sep = "-")
  d_yyyymmdd <- paste(rep(years, each = 365),
                      rep(d_mmdd, times = length(years)), sep = "-")
  # monthly data: YYYY-MM-LastDayOfMonth
  m_mmdd     <- paste(mm, ndays_in_month, sep = "-")
  m_yyyymmdd  <- paste(rep(years, each = 12),
                       rep(m_mmdd, times = length(years)), sep = "-")
  # yearly data: YYYY-12-31
  y_yyyymmdd <- paste(years, 12, 31, sep = "-")

  # Select time vector according to nstep
  time_dimnames <- switch(
    as.character(nstep),
    "365" = d_yyyymmdd,
    "12"  = m_yyyymmdd,
    "1"   = y_yyyymmdd
  )

  return(time_dimnames)
}




# ------------------------------------ #
# Other time-related functions

# Create sequence of dates and return as character
seqDates <- function(start_date = "1980-01-01",
                     end_date   = "1980-12-31",
                     step       = "day"
                     ) {
  as.character(
    seq.Date(from = as.Date(start_date),
             to   = as.Date(end_date),
             by   = step)
  )
}

# Check if date is 29th February
is29Feb <- function(date = "1980-01-01"
                    ) {
  day <- format(as.Date(date), "%d")
  mon <- format(as.Date(date), "%m")

  return(paste(mon, day) == "02 29")
}

# Convert day-of-the-year (DOY) to date "YYYY-MM-DD"
doy_to_date <- function(doy  = NULL,
                        year = NULL
                        ) {
  if (length(doy) != length(year)) stop("doy and year have different length")

  date <- strptime(paste(year, doy), format = "%Y %j")
  return(date)
}

# Convert date "YYYY-MM-DD" to day-of-the-year (DOY)
date_to_doy <- function(date       = "2010-01-29",
                        skip_feb29 = TRUE
                        ) {

  if (skip_feb29 == TRUE) {
    # What DOY is Dec. 31 for the year of date, 365 or 366?
    dec31 <- paste(format(as.Date(date), "%Y"), "12-31", sep = "-")
    doy_dec31 <- as.integer(format(as.Date(dec31), "%j"))
    # Convert date to DOY, including Feb. 29
    doy1 <- as.integer(format(as.Date(date), "%j"))
    # If is leap year, subtract 1 to all DOYs after Feb. 28
    doy <- ifelse(doy_dec31 == 366 & doy1 > 28,
                  doy1 - 1,
                  doy1)
  } else {
    # Just convert date to DOY, including Feb. 29
    doy <- as.integer(format(as.Date(date), "%j"))
  }
  return(doy)
}

# Convert date "YYYY-MM-DD" to day-of-the-year (DOY)
date_to_month <- function(date = "2010-01-29"
                          ) {
  as.integer(format(as.Date(date), "%m"))
}

# Convert date "YYYY-MM-DD" to day-of-the-year (DOY)
date_to_year <- function(date = "2010-01-29"
                         ) {
  as.integer(format(as.Date(date), "%Y"))
}

