#' @title Index of overlapping growing seasons
#'
#' @description This function finds the index of overlapping
#' growing seasons. sdate and hdate are vectors of sowing and
#' harvest date (DOY) of length = length(syear:eyear).
#'
#' @param sdate vector of sowing dates (DOY)
#' @param hdate vector of harvest dates (DOY)
#' @param syear numeric value, first year in the dates time serie
#' @param eyear numeric value, last year in the dates time serie
#'
#' @examples
#' whichOverlappingSeasons(sdate = c(360, 90, 90),
#'                         hdate = c(120, 120, 210),
#'                         syear = 2000,
#'                         eyear = 2002)
#' @export

whichOverlappingSeasons <- function(sdate = NULL,
                                    hdate = NULL,
                                    syear = NULL,
                                    eyear = NULL
                                    ) {

  years  <- syear:eyear
  nyears <- length(syear:eyear)

  # Adjust hdate if occurring next year
  hdate_adj <- ifelse(sdate < hdate, hdate, hdate + 365)

  # Convert doy to days since first day in the time series
  dec31_doys <- 365 * (0:(nyears - 1)) # seq of 31st Dec DOYs
  sdate_seq  <- hdate_seq <- rep(NA, nyears)
  for (dd in seq_len(length(dec31_doys))) {
    sdate_seq[dd] <- dec31_doys[dd] + sdate[dd]
    hdate_seq[dd] <- dec31_doys[dd] + hdate_adj[dd]
  } # dd

  # Shift time series by one year
  sdate_years <- (syear + 1):eyear
  sdate_t1 <- sdate_seq[which(years %in% sdate_years)]
  sdate_t1[is.nan(sdate_t1)] <- NA

  hdate_years <- syear:(eyear - 1)
  hdate_t0 <- hdate_seq[which(years %in% hdate_years)]

  # Compute difference between sdate next year - hdate previous year ----
  date_diff  <- sdate_t1 - hdate_t0
  overlap_sd1_hd0 <- ifelse(date_diff < 0, TRUE, FALSE)

  # Count years with overlap
  years_with_overlap <- which(overlap_sd1_hd0 == TRUE)

  # Return indey of years with overlapping growing seasons
  return(years_with_overlap)
}
