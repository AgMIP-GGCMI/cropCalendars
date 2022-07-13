#' @title Calculate crop calendars
#'
#' @description Wrapper function calling sub-functions to
#' calculate sowing and harvest dates.
#'
calcCropCalendars <- function(lon          = NULL,
                              lat          = NULL,
                              mclimate     = NULL,
                              crop         = NULL,
                              croppar_file = NULL
                              ) {

  # Import crop parameters
  if (is.null(croppar_file)) {
    croppar_file <- system.file("extdata", "crop_parameters.csv",
                                package = "cropCalendars", "mustWork" = TRUE)
  }
  crop_parameters <- getCropParam(cropparam_file = croppar_file,
                                  crops          = crop
                                  )

  # Get monthly weather data of the grid cell
  mtemp      <- mclimate$mtemp
  mprec      <- mclimate$mprec
  mppet      <- mclimate$mppet
  mppet_diff <- mclimate$mppet_diff

  # Seasonality type
  seasonality <- calcSeasonality(mtemp, mprec, 10)

  # Sowing date
  sowing <- calcSowingDate(crop_parameters, mtemp, mppet, seasonality, lat)

  sowing_month  <- sowing[["sowing_month"]]
  sowing_day    <- sowing[["sowing_doy"]]
  sowing_season <- sowing[["sowing_season"]]

  # Harvest date
  harvest_rule  <- calcHarvestRule(crop_parameters, mtemp, mppet, seasonality)

  harvest_vector  <- calcHarvestDateVector(crop_parameters, sowing_day,
                                           sowing_season, mtemp, mppet,
                                           mppet_diff)

  harvest <- calcHarvestDate(crop_parameters, mtemp, sowing_day,
                             sowing_month, sowing_season, seasonality,
                             harvest_rule, harvest_vector)

  harvest_day_rf  <- harvest[["hd_rf"]]
  harvest_day_ir  <- harvest[["hd_ir"]]
  harvest_reas_rf <- harvest[["harvest_reason_rf"]]
  harvest_reas_ir <- harvest[["harvest_reason_ir"]]

  # Growing period length
  growpriod_rf <- calcGrowingPeriod(sowing_day, harvest_day_rf, 365)
  growpriod_ir <- calcGrowingPeriod(sowing_day, harvest_day_ir, 365)

  # Output table
  pixel_df <- data.frame("lon"              = rep(lon, 2),
                         "lat"              = rep(lat, 2),
                         "cft_id"           = rep(crop_parameters$cft_id, 2),
                         "crop"             = rep(crop_parameters$crop_name, 2),
                         "irrigation"       = c("Rainfed", "Irrigated"),
                         "seasonality_type" = rep(seasonality, 2),
                         "sowing_season"    = rep(sowing_season, 2),
                         "sowing_month"     = rep(sowing_month, 2),
                         "sowing_doy"       = rep(sowing_day, 2),
                         "harvest_rule"     = rep(harvest_rule, 2),
                         "harvest_reason"   = c(harvest_reas_rf, harvest_reas_ir),
                         "maturity_doy"     = c(harvest_day_rf, harvest_day_ir),
                         "growing_period"   = c(growpriod_rf, growpriod_ir)  )

  return(pixel_df)

}
