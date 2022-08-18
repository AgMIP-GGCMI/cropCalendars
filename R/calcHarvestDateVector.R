#' @title Calculate vector of possible harvest dates (Minoli et al., 2019)
#'
#' @description Harvest reasons are the events that can trigger the harvest
#' of a crop within an agro-climatic zone:
#' Earliest-maturing cultivar (GPmin);
#' Cultivar with longest grain filling (GPmaxrp);
#' Latest-maturing cultivar (GPmax);
#' Escape terminal water stress (w. lim);
#' Grain filling in warmest period (mid. t.);
#' Escape high temperature (high t.).
#'
#' @param croppar data.frame with crop parematers as returned by getCropParam
#' @param sowing_date numeric value as day of the year (DOY). This can be either
#' caculated with calcSowingDate or prescribed.
#' @param sowing_season character value. Can be either "winter" or "spring". See
#' calcSowingDate.
#' @param monthly_temp numeric vector of length 12. Mean monthly air temperature
#' (degree Celsius).
#' @param monthly_ppet numeric vestor of length 12. Mean Potential Evapotranspiration (mm). See caclMonthlyClimate.
#' @param monthly_ppet_diff numeric vestor of length 12. Mean difference of
#' Potential Evapotranspiration (mm). See caclMonthlyClimate.
#'
#' @seealso getCropParam, calcMonthlyClimate, calcSowingDate, calcCropCalendars
#' @export
calcHarvestDateVector <- function(croppar,
                                  sowing_date,
                                  sowing_season,
                                  monthly_temp,
                                  monthly_ppet,
                                  monthly_ppet_diff
                                  ) {

  # Extract individual parameter names and values
  for(i in colnames(croppar)) {
    assign(i, croppar[[i]])
  }

  ndays_year <- 365
  midday     <- c(15, 43, 74, 104, 135, 165, 196, 227, 257, 288, 318, 349, 380)

  # Shortest cycle: crop lower biological limit
  hd_first <- sowing_date + min_growingseason
  # Medium cycle: best trade-off vegetative and reproductive growth
  hd_maxrp <- sowing_date + maxrp_growingseason
  # Longest cycle: crop upper biological limit
  hd_last <- ifelse(sowing_season == "winter",
                    sowing_date + max_growingseason_wt,
                    sowing_date + max_growingseason_st)

  # End of wet season ----
  doy_wet1 <- calcDoyCrossThreshold(
    monthly_ppet,
    ppet_ratio
    )[["doy_cross_down"]]
  doy_wet2 <- calcDoyCrossThreshold(
    monthly_ppet_diff,
    ppet_ratio_diff
    )[["doy_cross_down"]]
  doy_wet_vec <- ifelse(
    c(doy_wet1, doy_wet2) < sowing_date & c(doy_wet1, doy_wet2) != -9999,
    c(doy_wet1, doy_wet2) + ndays_year,
    c(doy_wet1, doy_wet2)
    )
  # If more than one wet seasons take the first one, else -9999
  doy_wet_first <- ifelse(
    length(doy_wet_vec[doy_wet_vec != -9999]) > 0,
    min(doy_wet_vec[doy_wet_vec != -9999]),
    -9999
    )
  # If does not find harvest date and it is always high rainfall
  if (doy_wet1 == -9999) {
    if (min(monthly_ppet) >= ppet_min) {
      hd_wetseas <- hd_last
    } else {
      hd_wetseas <- hd_first
    }
  } else {
    hd_wetseas <- doy_wet_first + rphase_duration
  }

  # Warmest day of the year ----
  warmest_day <- midday[monthly_temp == max(monthly_temp)][1]
  hd_temp_base <- ifelse(
    sowing_season == "winter", warmest_day, warmest_day + rphase_duration
    )

  # First hot day ----
  doy_exceed_opt_rp <- calcDoyCrossThreshold(
    monthly_temp, temp_opt_rphase
    )[["doy_cross_up"]]
  idx <- which(doy_exceed_opt_rp < sowing_date & doy_exceed_opt_rp != -9999)
  doy_exceed_opt_rp[idx] <- doy_exceed_opt_rp[idx] + ndays_year
  doy_exceed_opt_rp      <- sort(doy_exceed_opt_rp)[1]

  # Last hot day ----
  doy_below_opt_rp <- calcDoyCrossThreshold(
    monthly_temp,
    temp_opt_rphase
    )[["doy_cross_down"]]
  idx <- which(doy_below_opt_rp < sowing_date & doy_below_opt_rp != -9999)
  doy_below_opt_rp[idx] <- doy_below_opt_rp[idx] + ndays_year
  doy_below_opt_rp      <- sort(doy_below_opt_rp)[1]

  # Winter type: First hot day; Spring type: Last hot day
  doy_opt_rp  <- ifelse(
    sowing_season == "winter", doy_exceed_opt_rp, doy_below_opt_rp
    )
  if (doy_opt_rp == -9999) {
    hd_temp_opt <- hd_maxrp
  } else {
    hd_temp_opt <- ifelse(
      sowing_season == "winter", doy_opt_rp, doy_opt_rp+rphase_duration
      )
  }

  # If harvest date < sowing date, it occurs the following year, so add 365 days
  hd_wetseas    <- ifelse(
    hd_wetseas < sowing_date, hd_wetseas + ndays_year, hd_wetseas
    )
  hd_temp_base  <- ifelse(
    hd_temp_base < sowing_date, hd_temp_base + ndays_year, hd_temp_base
    )
  hd_temp_opt   <- ifelse(
    hd_temp_opt < sowing_date, hd_temp_opt + ndays_year, hd_temp_opt
    )

  # hd_vector ----
  hd_vector        <- c(hd_first, hd_maxrp, hd_last,
                        hd_wetseas, hd_temp_base, hd_temp_opt)
  names(hd_vector) <- c("hd_first", "hd_maxrp", "hd_last",
                        "hd_wetseas", "hd_temp_base", "hd_temp_opt")

  return(hd_vector)
}
