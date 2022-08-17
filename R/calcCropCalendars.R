#' @title Calculate crop calendars
#'
#' @description Wrapper function calling sub-functions to
#' calculate sowing and harvest dates.
#' @param lon Longitude (decimal degrees)
#' @param lat Latitude (decimal degrees)
#' @param mclimate Monthly climate. A list returned by the calcMonthlyClimate()
#' function.
#' @param crop A crop name (chr), among those specified in the croppar_file
#' @param croppar_file Crop parameter file. If not specified, the default one is
#' used.
#' @seealso calcMonthlyClimate
#' @export

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
  crop_parameters <- getCropParam(
    crops          = crop,
    cropparam_file = croppar_file
  )

  # Get monthly weather data of the grid cell
  mtemp      <- mclimate$mtemp
  mprec      <- mclimate$mprec
  mppet      <- mclimate$mppet
  mppet_diff <- mclimate$mppet_diff

  # Seasonality type
  seasonality <- calcSeasonality(
    monthly_temp = mtemp,
    monthly_prec = mprec,
    temp_min     = 10
  )

  # Sowing date
  sowing <- calcSowingDate(
    croppar      = crop_parameters,
    monthly_temp = mtemp,
    monthly_ppet = mppet,
    seasonality  = seasonality,
    lat          = lat
  )

  sowing_month  <- sowing[["sowing_month"]]
  sowing_day    <- sowing[["sowing_doy"]]
  sowing_season <- sowing[["sowing_season"]]

  # Harvest date
  harvest_rule  <- calcHarvestRule(
    croppar      = crop_parameters,
    monthly_temp = mtemp,
    monthly_ppet = mppet,
    seasonality  = seasonality
  )

  harvest_vector <- calcHarvestDateVector(
    croppar           = crop_parameters,
    sowing_date       = sowing_day,
    sowing_season     = sowing_season,
    monthly_temp      = mtemp,
    monthly_ppet      = mppet,
    monthly_ppet_diff = mppet_diff
  )

  harvest <- calcHarvestDate(
    croppar       = crop_parameters,
    monthly_temp  = mtemp,
    sowing_date   = sowing_day,
    sowing_month  = sowing_month,
    sowing_season = sowing_season,
    seasonality   = seasonality,
    harvest_rule  = harvest_rule,
    hd_vector     = harvest_vector
  )

  harvest_day_rf  <- harvest[["hd_rf"]]
  harvest_day_ir  <- harvest[["hd_ir"]]
  harvest_reas_rf <- names(harvest[["harvest_reason_rf"]])
  harvest_reas_ir <- names(harvest[["harvest_reason_ir"]])

  # Growing period length
  growpriod_rf <- calcGrowingPeriod(sowing_day, harvest_day_rf, 365)
  growpriod_ir <- calcGrowingPeriod(sowing_day, harvest_day_ir, 365)

  # Output table
  pixel_df <- data.frame(
    "lon"              = rep(lon, 2),
    "lat"              = rep(lat, 2),
    "crop"             = rep(crop_parameters$crop_name, 2),
    "irrigation"       = c("Rainfed", "Irrigated"),
    "seasonality_type" = rep(seasonality, 2),
    "sowing_season"    = rep(sowing_season, 2),
    "sowing_month"     = rep(sowing_month, 2),
    "sowing_doy"       = rep(sowing_day, 2),
    "harvest_rule"     = rep(names(harvest_rule), 2),
    "harvest_reason"   = c(harvest_reas_rf, harvest_reas_ir),
    "maturity_doy"     = c(harvest_day_rf, harvest_day_ir),
    "growing_period"   = c(growpriod_rf, growpriod_ir)
  )

  return(pixel_df)

}
