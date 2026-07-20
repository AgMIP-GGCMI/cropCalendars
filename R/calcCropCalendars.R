#' @title Calculate crop calendars
#'
#' @description Wrapper function calling sub-functions to
#' calculate sowing and harvest dates.
#' @param lon Longitude (decimal degrees)
#' @param lat Latitude (decimal degrees)
#' @param mclimate Monthly climate. A list returned by the calcClimatology()
#' function.
#' @param crop A crop name (chr), among those specified in the croppar_file
#' @param croppar_file Crop parameter file. If not specified, the default one is
#' used.
#' @param crop_parameters Optional pre-extracted crop-parameter row (one row
#' \code{data.frame}, as returned by \code{getCropParam}). When supplied, the
#' parameter file is not read — essential when calling this per cell in a loop
#' (avoids re-reading the CSV on every call). \code{crop}/\code{croppar_file}
#' are then ignored.
#' @param prev_wet_doy Integer DOY of last year's wettest-window start (or
#' \code{NA}), forwarded to \code{calcSowingDate}/\code{calcDoyWetMonth} for
#' temporal hysteresis (sliding-window pipeline). Only used when
#' \code{wet_window_eps > 0}. The wettest-window argmax is crop-independent, so
#' this state is per-cell. The resolved DOY is also returned as
#' \code{attr(., "wet_doy")} for the caller to carry forward.
#' @param wet_window_eps Non-negative relative hysteresis band forwarded to
#' \code{calcDoyWetMonth} (default 0 = off). See \code{?calcDoyWetMonth}.
#' @param wet_window_decay Positive Gaussian decay scale forwarded to
#' \code{calcDoyWetMonth} (default 0.3; smaller = stickier). See
#' \code{?calcDoyWetMonth}.
#' @param cross_min_duration Integer minimum sustained-excursion length (days)
#' forwarded to \code{calcSowingDate}/\code{calcHarvestDateVector} ->
#' \code{calcDoyCrossThreshold} (default 1 = off). See
#' \code{?calcDoyCrossThreshold}.
#' @param cross_min_area Non-negative integrated-excursion (deficit-days) budget forwarded to
#' \code{calcHarvestDateVector} for the wet-season-end level crossing only -- magnitude-weights
#' the persistence guard so a grazing P/PET crossing must persist far longer to count
#' (default 0 = off). See \code{?calcDoyCrossThreshold} (\code{min_area}).
#' @param temp_cross_min_area Non-negative integrated-excursion (degree-days) budget forwarded to
#' \code{calcHarvestDateVector} for the reproductive hot-day crossing (\code{hd_temp_opt}) -- the
#' temperature analogue of \code{cross_min_area}, damping the \code{hd_temp_opt} existence flicker in
#' marginal cells whose warm plateau grazes \code{temp_opt_rphase} (subtropical winter wheat). A
#' length-2 \code{c(lo, hi)} HYSTERETIC pair carried per cell AND per crop (the 24x bit of
#' \code{prev_harv}); default 0 = off. See \code{?calcHarvestDateVector}.
#' @param wet_near_min_area Non-negative integrated-excursion budget (default 0 = off) for the wet-near
#' gate, forwarded to \code{calcHarvestDateVector}. Replaces the brittle single-day max test with an
#' area-above-threshold test so a thin P/PET touch reads as NOT wet; \code{c(lo, hi)} HYSTERETIC on
#' \code{prev_wet_near}. Chiefly affects Rice. See \code{?calcHarvestDateVector}.
#' @param smooth_window Integer day-window for the daily-climatology smoothing -- the SINGLE
#' global window applied to BOTH the threshold-crossing inputs (spring/fall temperature,
#' hot-day, wet-season-end P/PET) AND the daily reductions (coldest/warmest-window means and
#' anchor DOYs, driest-window P/PET, the moisture-trend diff). Default 31 (odd, so the
#' centred smoothing/argmax is exactly symmetric). The raw daily
#' climatology (\code{mclimate}) is kept; this sets the window the rules read it through.
#' Forwarded to \code{calcSeasonality}/\code{calcSowingDate}/\code{calcHarvestRule}/
#' \code{calcHarvestDate}/\code{calcHarvestDateVector}. The 120-day wettest window
#' (\code{calcDoyWetMonth}) has its own fixed window and is unaffected.
#' @param prev_seas Character seasonality class from last year (or \code{NA}),
#' forwarded to \code{calcSeasonality} for class hysteresis. The class is
#' crop-independent, so this is per-cell state; the resolved class is returned as
#' \code{attr(., "seas_type")} for the caller to carry forward. See
#' \code{?calcSeasonality}.
#' @param seas_eps Non-negative seasonality-threshold deadband forwarded to
#' \code{calcSeasonality} (default 0 = off).
#' @param seas_mtemp_margin Absolute min-temperature deadband (deg C) forwarded to
#' \code{calcSeasonality} as \code{mtemp_margin} (default 1; only used when
#' \code{seas_eps > 0}).
#' @param prev_harv Integer packed harvest-rule hysteresis state from last year (or
#' \code{NA}), encoding the thermal class and the wet-near-sowing flag (\code{tclass +
#' 3*harv_hi}, \code{harv_hi = rw1 + 12*wet_near}). Unlike the seasonality/wet-window state this is
#' crop-DEPENDENT, so the caller carries it per cell AND per crop. The resolved value is returned as
#' \code{attr(., "harv_state")}. Used for the wet-near Schmitt window and when
#' \code{harv_tmax_margin > 0}.
#' @param harv_tmax_margin Absolute deadband (deg C, default 0 = off) on the harvest
#' rule's \code{temp_max} vs base/optimum reproductive thresholds (thermal class),
#' forwarded to \code{calcHarvestRule}. Suppresses harvest-formula flips from a
#' grazing \code{temp_max}.
#' @param harv_ppet_eps DEPRECATED, ignored. The always-wet test it deadbanded was retired:
#' the no-wet-end \code{hd_last}/\code{hd_first} decision is now made by the persistence-guarded
#' two-tier rule (wet-near gate at \code{ppet_ratio}, then the \code{ppet_min} floor) in
#' \code{calcHarvestDateVector}, so there is no \code{min_ppet} graze to deadband. Kept in the
#' signature only so existing callers do not error.
#' @param prev_winter Integer winter regime last year (1 warm / 0 mild / -1 cold, or \code{NA}),
#' forwarded to \code{calcSowingDate} for hysteresis on both winter-regime boundaries. Like the harvest
#' state this is crop-DEPENDENT (the warm/cold/mild branch is gated on the crop's seasonality), so the
#' caller carries it per cell AND per crop. The resolved regime is returned as
#' \code{attr(., "winter_regime")}. Only winter-type crops act on it.
#' @param winter_margin Absolute deadband (deg C, default 0 = off) on the WARM winter-regime boundary
#' (\code{basetemp.low}), forwarded to \code{calcSowingDate}. Suppresses the ~half-year warm<->mild sowing
#' flip when \code{coldest_t} grazes the boundary. See \code{?calcSowingDate}.
#' @param winter_cold_margin Absolute deadband (deg C, default 0 = off) on the COLD winter-regime boundary
#' (\eqn{-10}\,°C), forwarded to \code{calcSowingDate}. Decoupled from \code{winter_margin} so the
#' continental autumn<->spring WW flip (Russia) can be widened independently. See \code{?calcSowingDate}.
#' @seealso calcClimatology
#' @export

calcCropCalendars <- function(lon                   = NULL,
                              lat                   = NULL,
                              mclimate              = NULL,
                              crop                  = NULL,
                              croppar_file          = NULL,
                              crop_parameters       = NULL,
                              prev_wet_doy          = NA_integer_,
                              wet_window_eps        = 0,
                              wet_window_decay      = 0.3,
                              cross_min_duration    = 1L,
                              cross_min_area        = 0,
                              temp_cross_min_area   = 0,
                              wet_near_min_area     = 0,
                              smooth_window         = 31L,
                              prev_seas             = NA_character_,
                              seas_eps              = 0,
                              seas_mtemp_margin     = 1,
                              prev_harv             = NA_integer_,
                              harv_tmax_margin      = 0,
                              harv_ppet_eps         = 0,
                              harv_exist_eps        = 0,
                              prev_winter           = NA_integer_,
                              winter_margin         = 0,
                              winter_cold_margin    = 0
                              ) {

  # Import crop parameters (unless already supplied by the caller).
  if (is.null(crop_parameters)) {
    if (is.null(croppar_file)) {
      croppar_file <- system.file("extdata", "crop_parameters.csv",
                                  package = "cropCalendars", "mustWork" = TRUE)
    }
    crop_parameters <- getCropParam(
      crops          = crop,
      cropparam_file = croppar_file
    )
  }

  # Get weather data of the grid cell. The sliding-window ring serves only the daily
  # climatology, so the monthly temperature/precipitation used by the seasonality CV
  # classifier are reconstructed from the daily series when absent (per-pixel
  # calcClimatology still supplies them directly). The monthly P/PET fields are
  # only the harvest-rule monthly FALLBACK inputs -- never used when the daily series
  # are present -- so they stay NULL on the ring path.
  dtemp      <- mclimate$dtemp
  dprec      <- mclimate$dprec
  dpet       <- mclimate$dpet
  mtemp      <- if (!is.null(mclimate$mtemp)) mclimate$mtemp else .monthlyFromDaily(dtemp, "mean")
  mprec      <- if (!is.null(mclimate$mprec)) mclimate$mprec else .monthlyFromDaily(dprec, "sum")
  mppet      <- mclimate$mppet
  mppet_diff <- mclimate$mppet_diff

  # Seasonality type
  seasonality <- calcSeasonality(
    monthly_temp = mtemp,
    monthly_prec = mprec,
    temp_min     = 10,
    prev_seas    = prev_seas,
    seas_eps     = seas_eps,
    mtemp_margin = seas_mtemp_margin,
    daily_temp   = dtemp,
    smooth_window = smooth_window
  )

  # Resolved wettest-window start, carried forward as hysteresis state. This is the
  # crop-INDEPENDENT wettest-window DOY (pure climate), computed once here and then
  # passed into calcSowingDate (the PREC/PRECTEMP spring sowing uses exactly this
  # value) so the 120-day rolling P/PET selection -- the dominant inner-loop cost --
  # runs once per cell instead of twice. It is taken here rather than from sowing_day
  # because vernalizing/winter crops take the winter sowing branch even in
  # PREC/PRECTEMP cells, so their sowing_day is NOT the wet-window argmax. Carrying
  # that wrong anchor poisoned prev_wet whenever crop #1 was a winter crop (e.g.
  # Winter_Wheat in the combined run), flipping the eps-weighted pick at the
  # cold-start -> first-sticky-year boundary. Only PREC/PRECTEMP cells have wet-window
  # state; elsewhere report NA.
  wet_doy <- if (seasonality %in% c("PREC", "PRECTEMP"))
    calcDoyWetMonth(dprec, dpet, prev_doy = prev_wet_doy, eps = wet_window_eps,
                    decay = wet_window_decay) else NA_integer_

  # Sowing date
  sowing <- calcSowingDate(
    croppar        = crop_parameters,
    monthly_temp   = mtemp,
    daily_prec     = dprec,
    daily_pet      = dpet,
    daily_temp     = dtemp,
    seasonality           = seasonality,
    lat                   = lat,
    prev_wet_doy          = prev_wet_doy,
    wet_window_eps        = wet_window_eps,
    wet_window_decay      = wet_window_decay,
    cross_min_duration    = cross_min_duration,
    smooth_window         = smooth_window,
    wet_doy               = wet_doy,
    prev_winter           = prev_winter,
    winter_margin         = winter_margin,
    winter_cold_margin    = winter_cold_margin
  )

  sowing_month  <- sowing[["sowing_month"]]
  sowing_day    <- sowing[["sowing_doy"]]
  sowing_season <- sowing[["sowing_season"]]

  # Harvest date
  # Harvest-rule hysteresis state carried from last year, packed into one integer:
  # harv_state = tclass(0-2) + 3*harv_hi, harv_hi = rw1(0-2) + 6*wf + 12*wn + 24*tf + 48*ff encodes the
  # level wet-end EXISTENCE regime (rw1: 0 absent-DRY / 1 found / 2 absent-WET, cc_regime path only), the
  # wet-end-found flag (wf, cross_min_area Schmitt), the wet-near-sowing flag (wn), the reproductive
  # hot-day-found flag (tf, temp_cross_min_area Schmitt) and the TIER-2 floor-found flag (ff, the floor
  # cross_min_area Schmitt). Each bit sits at a fixed place (wf at /6%%2, wn at /12%%2, tf at /24%%2,
  # ff at /48) so the decode is path-independent.
  prev_tclass        <- if (is.na(prev_harv)) NA_integer_ else prev_harv %% 3L
  prev_hi            <- if (is.na(prev_harv)) NA_integer_ else prev_harv %/% 3L
  prev_rw1           <- if (is.na(prev_hi)) NA_integer_ else prev_hi %% 3L
  prev_wetend_found  <- if (is.na(prev_hi)) NA else as.logical((prev_hi %/% 6L) %% 2L)
  prev_wet_near      <- if (is.na(prev_hi)) NA else as.logical((prev_hi %/% 12L) %% 2L)
  prev_topt_found    <- if (is.na(prev_hi)) NA else as.logical((prev_hi %/% 24L) %% 2L)
  prev_floor_found   <- if (is.na(prev_hi)) NA else as.logical(prev_hi %/% 48L)

  harvest_rule  <- calcHarvestRule(
    croppar      = crop_parameters,
    monthly_temp = mtemp,
    monthly_ppet = mppet,
    seasonality  = seasonality,
    daily_temp   = dtemp,
    prev_tclass      = prev_tclass,
    harv_tmax_margin = harv_tmax_margin,
    smooth_window    = smooth_window
  )

  harvest_vector <- calcHarvestDateVector(
    croppar           = crop_parameters,
    sowing_date       = sowing_day,
    sowing_season     = sowing_season,
    monthly_temp      = mtemp,
    monthly_ppet      = mppet,
    monthly_ppet_diff = mppet_diff,
    daily_temp        = dtemp,
    daily_prec        = dprec,
    daily_pet         = dpet,
    cross_min_duration = cross_min_duration,
    cross_min_area     = cross_min_area,
    smooth_window      = smooth_window,
    prev_rw1          = prev_rw1,
    harv_exist_eps    = harv_exist_eps,
    prev_wet_near     = prev_wet_near,
    prev_wetend_found = prev_wetend_found,
    temp_cross_min_area = temp_cross_min_area,
    prev_topt_found   = prev_topt_found,
    wet_near_min_area = wet_near_min_area,
    prev_floor_found  = prev_floor_found
  )

  harvest <- calcHarvestDate(
    croppar       = crop_parameters,
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

  # Resolved harvest-rule hysteresis state, re-packed for the caller to carry forward.
  # Unlike seas_type/wet_doy this is crop-DEPENDENT (thresholds are crop parameters),
  # so the driver keeps it per cell AND per crop.
  harv_state <- as.integer(attr(harvest_rule, "tclass") +
                           3L * as.integer(attr(harvest_vector, "harv_hi")))

  attr(pixel_df, "wet_doy")     <- wet_doy
  attr(pixel_df, "seas_type")   <- seasonality   # crop-independent; carried as prev_seas state
  attr(pixel_df, "harv_state")  <- harv_state    # crop-dependent; carried as prev_harv state
  attr(pixel_df, "winter_regime") <- attr(sowing, "winter_regime")  # crop-dependent; carried as prev_winter
  return(pixel_df)

}
