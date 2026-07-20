#' @title Calculate sowing date (Waha et al., 2012)
#'
#' @param monthly_temp Numeric vector of length 12. Average (e.g. 20-years)
#' monthly mean temperatures (degree Celsius). Retained as a fallback: the
#' coldest-month temperature and the coldest-day anchor are taken from
#' \code{daily_temp} (coldest 30-day window) when supplied, and only derived from
#' these monthly values if \code{daily_temp} is \code{NULL}.
#' @param daily_prec Numeric vector of length 365. Climatological daily
#' precipitation (mm, one value per DOY, averaged across years). The
#' \code{dprec} element from \code{calcClimatology}. Used by
#' \code{calcDoyWetMonth} (with \code{daily_pet}) to find the start of the wet
#' season as the wettest 120-day window (\eqn{\sum P / \sum PET}). Preferred input
#' for the wettest-window start in PREC/PRECTEMP cells; if absent, the monthly
#' fallback (\code{monthly_prec} / \code{monthly_pet}) is used instead.
#' @param daily_pet Numeric vector of length 365. Climatological daily PET (mm,
#' one value per DOY). The \code{dpet} element from \code{calcClimatology}.
#' Paired with \code{daily_prec} (see there).
#' @param daily_temp Numeric vector of length 365. Climatological daily mean
#' temperature (°C), one value per DOY (the \code{dtemp} element from
#' \code{calcClimatology}). Used for the coldest-month reductions (coldest
#' 30-day window mean and centre DOY) and by \code{calcDoyCrossThreshold} for the
#' spring/fall threshold crossings. \code{NULL} falls back to the legacy monthly
#' rule (calendar-month minimum and coldest-month mid-day) and interpolates the
#' monthly means onto the daily grid for the crossings.
#' @param seasonality character value indicating the seasonality type as
#' computed by calcSeasonality
#' @param prev_wet_doy Integer DOY of last year's wettest-window start (or
#' \code{NA}), forwarded to \code{calcDoyWetMonth} for temporal hysteresis in the
#' PREC/PRECTEMP sowing branch. Only used when \code{wet_window_eps > 0}.
#' @param wet_window_eps Non-negative relative hysteresis band forwarded to
#' \code{calcDoyWetMonth} (default 0 = off). See \code{?calcDoyWetMonth}.
#' @param wet_window_decay Positive Gaussian decay scale forwarded to
#' \code{calcDoyWetMonth} (default 0.3; smaller = stickier). See
#' \code{?calcDoyWetMonth}.
#' @param cross_min_duration Integer minimum sustained-excursion length (days)
#' forwarded to \code{calcDoyCrossThreshold} for the spring/fall temperature
#' crossings (default 1 = off). See \code{?calcDoyCrossThreshold}.
#' @param smooth_window Integer day-window for the daily-climatology smoothing -- the
#' single global window applied to BOTH the threshold-crossing input (\code{daily_temp}
#' smoothed before the spring/fall crossings) AND the daily reductions here (coldest-window
#' mean and coldest/warmest anchor DOYs). Default 31 (odd). See \code{calcCropCalendars}. The raw
#' daily climatology is still kept; this only sets the window the rules read it through.
#' @param monthly_prec,monthly_pet Optional numeric vectors of length 12, the
#' monthly precipitation and PET TOTALS (the \code{mprec} / \code{mpet} elements
#' from \code{calcClimatology}). Used only as the bug-free monthly fallback for
#' the wettest-window start in PREC/PRECTEMP cells, when
#' \code{daily_prec}/\code{daily_pet} (and \code{wet_doy}) are not supplied: the
#' wettest 4-month ratio-of-sums (\eqn{\sum P / \sum PET}). Coarser (~1-month
#' resolution) than the daily 120-day rule, but identical in form (sums P and PET
#' separately, no daily interpolation).
#' @param wet_doy Optional pre-computed wettest-window start DOY (the crop-independent
#' \code{calcDoyWetMonth} result). When supplied it is used directly in the
#' PREC/PRECTEMP spring-sowing branch, so the caller (\code{calcCropCalendars}) can
#' compute it once per cell instead of twice. \code{NULL} (default) recomputes it.
#' @param prev_winter Integer winter regime chosen last year (1 = warm, 0 = mild, -1 = cold), or
#' \code{NA} (default). Used (when \code{winter_margin > 0} or \code{winter_cold_margin > 0}) to make the
#' winter-regime boundaries (\code{basetemp.low} and \eqn{-10}\,°C) hysteretic. Returned as
#' \code{attr(., "winter_regime")} for the caller to carry forward. Only winter-type
#' (\code{WTYP_CALC_SDATE}) crops act on it.
#' @param winter_margin Absolute deadband (deg C, default 0 = off) on the WARM winter-regime boundary
#' (\code{basetemp.low}, the warm<->mild / autumn-anchor split). With a prior regime it relaxes toward
#' last year's regime so the regime is sticky within a \code{2 * winter_margin} band, suppressing the
#' ~half-year warm<->mild sowing flip when \code{coldest_t} grazes the boundary. Mirrors
#' \code{seas_mtemp_margin} / \code{harv_tmax_margin}.
#' @param winter_cold_margin Absolute deadband (deg C, default 0 = off) on the COLD winter-regime boundary
#' (\eqn{-10}\,°C, the mild<->cold / autumn-sowing<->spring-fallback split). Decoupled from
#' \code{winter_margin} so the continental cold boundary (Russia autumn<->spring WW flip) can be widened
#' independently of the warm boundary. Same prior-regime relaxation (sticky within \code{2 * winter_cold_margin}).
#' @export
calcSowingDate <- function(croppar,
                           monthly_temp,
                           daily_prec            = NULL,
                           daily_pet             = NULL,
                           daily_temp            = NULL,
                           seasonality,
                           lat,
                           prev_wet_doy          = NA_integer_,
                           wet_window_eps        = 0,
                           wet_window_decay      = 0.3,
                           cross_min_duration    = 1L,
                           wet_doy               = NULL,
                           monthly_prec          = NULL,
                           monthly_pet           = NULL,
                           smooth_window         = 31L,
                           prev_winter           = NA_integer_,
                           winter_margin         = 0,
                           winter_cold_margin    = 0
                           ) {

  # extract individual parameter names and values
  list2env(croppar, environment())  # 1-row data.frame: columns -> scalar params

  # Coldest-month temperature (coldest_t) and coldest-day anchor (coldest_doy).
  # With the daily climatology these are the coldest smooth_window-day window mean and its
  # centre DOY: a daily mean is already a per-DOY 30-year mean, so the smooth_window window
  # reproduces the calendar-month coldness
  # continuously -- removing the ~30-day quantisation that made the warm-winter
  # sowing date and the spring-crossing anchor jump between adjacent climate windows.
  # When daily_temp is absent, fall back to the exact legacy monthly rule (the
  # calendar-month minimum and the coldest-month mid-day), and interpolate the
  # monthly means onto the daily grid only for the threshold-crossing scans below
  # (the spring/fall crossings have no pure-monthly form).
  if (!is.null(daily_temp)) {
    coldest_t   <- .coldestWindowMean(daily_temp, width = smooth_window)
    coldest_doy <- .doyColdestWindow(daily_temp, width = smooth_window)
    warmest_doy <- .doyWarmestWindow(daily_temp, width = smooth_window)
  } else {
    midday      <- c(15, 43, 74, 104, 135, 165, 196, 227, 257, 288, 318, 349)
    coldest_t   <- min(monthly_temp)
    coldest_doy <- midday[which.min(monthly_temp)]
    warmest_doy <- midday[which.max(monthly_temp)]
    daily_temp  <- .monthlyToDoy365(monthly_temp)
  }

  # Smoothed daily temperature for the threshold crossings. Same global smooth_window the
  # coldest-window reductions above use, so crossing inputs and reductions share one window.
  daily_temp_x <- .circRoll(daily_temp, smooth_window, "center", mean = TRUE)

  # Constrain first possible date for winter crop sowing
  earliest_sdate  <- ifelse(lat >= 0, initdate.sdatenh, initdate.sdatesh)
  earliest_smonth <- doy2month(earliest_sdate)
  DEFAULT_DOY     <- ifelse(lat >= 0, 1, 182)
  DEFAULT_MONTH   <- 0

  # Winter-regime classification (warm / mild / cold) with optional hysteresis on EACH boundary. The
  # two thresholds each pick a different autumn-sowing anchor, so when coldest_t grazes either boundary
  # the sowing date can jump between adjacent years:
  #   * warm boundary (coldest_t > basetemp.low): warm -> coldest_doy-75, mild -> temp_fall down-crossing
  #   * cold boundary (coldest_t < -10):          cold -> -9999 (spring fallback), mild -> down-crossing
  # Each boundary has its OWN deadband -- winter_margin for the warm boundary, winter_cold_margin for the
  # cold boundary -- so the continental cold boundary (Russia autumn<->spring flip) can be widened without
  # loosening the warm (India/US) boundary. With last year's regime (prev_winter: 1 warm / 0 mild / -1
  # cold) each boundary relaxes TOWARD the previous regime (mirroring seas_mtemp_margin /
  # harv_tmax_margin): the regime you were in stays sticky unless coldest_t moves a full margin past its
  # edge, so each band is 2*margin wide.
  warm_thr <- basetemp.low
  cold_thr <- -10
  if (!is.na(prev_winter)) {
    if (winter_margin > 0)
      warm_thr <- if (prev_winter >=  1L) basetemp.low - winter_margin      else basetemp.low + winter_margin
    if (winter_cold_margin > 0)
      cold_thr <- if (prev_winter <= -1L) -10          + winter_cold_margin else -10          - winter_cold_margin
  }
  winter_is_temp <- seasonality %in% c("TEMP", "TEMPPREC", "PRECTEMP", "PREC")

  # What type of winter is it?
  if ((coldest_t > warm_thr) & winter_is_temp) {
    # "Warm winter" (allowing non-vernalizing winter-sown crops)
    # sowing 2.5 months before the coldest day
    # it seems a good approximation for both India and South US)
    coldestday     <- coldest_doy
    firstwinterdoy <- ifelse(coldestday-75<=0, coldestday-75+365, coldestday-75)
    winter_regime  <- 1L

  } else if ((coldest_t < cold_thr) & winter_is_temp) {
    # "Cold winter" (winter too harsh for winter crops, only spring sowing possible)
    firstwinterdoy <- -9999
    winter_regime  <- -1L

  } else {
    # "Mild winter" (allowing vernalizing crops). Anchor the autumn down-crossing scan
    # at the warmest day -- the mirror of the coldest-day anchor on the spring
    # up-crossing below. Scanning forward from the summer peak catches the genuine
    # autumn cooling and skips a non-monotonic spring ascent grazing temp_fall, which
    # from DOY 1 would register a spurious autumn-cooling date ~half a year early
    # (the same failure the spring fix removed, mirrored).
    firstwinterdoy <- calcDoyCrossThreshold(
      daily_temp_x, temp_fall, min_duration = cross_min_duration,
      from = warmest_doy)[["doy_cross_down"]]
    winter_regime  <- 0L

  }

  # First day of winter
  firstwintermonth <- ifelse(
    firstwinterdoy == -9999, DEFAULT_MONTH, doy2month(firstwinterdoy)
    )
  firstwinterdoy   <- ifelse(
    firstwinterdoy == -9999, DEFAULT_DOY, firstwinterdoy
    )

  # First day of spring: the first upward temp_spring crossing AFTER the coldest day, scanned forward
  # from the centroid coldest_doy (the phase-anchored winter trough). Anchoring the scan to the winter
  # minimum skips an autumn temperature plateau grazing temp_spring -- which sits before the coldest
  # day -- that would otherwise register a spurious ~half-year-early sowing in mild-winter cells,
  # without adding any temporal lag. No depth gate is needed: when the trough never dips below
  # temp_spring there is simply no up-crossing and calcDoyCrossThreshold returns -9999, which the
  # mean-based fallback below resolves to coldest_doy.
  firstspringdoy <- calcDoyCrossThreshold(
    daily_temp_x, temp_spring, min_duration = cross_min_duration,
    from = coldest_doy)[["doy_cross_up"]]
  firstspringmonth <- ifelse(
    firstspringdoy == -9999, DEFAULT_MONTH, doy2month(firstspringdoy)
    )
  # When temp_spring is never crossed, the spring sowing DOY falls back to the day the smoothed daily
  # temperature comes CLOSEST to the threshold, rather than the fixed DEFAULT_DOY (Jan-1 / Jul-1), so
  # the default<->found transition is continuous (the placeholder sits where the real crossing emerges)
  # instead of a ~half-year jump. A single discriminant -- the ANNUAL-MEAN smoothed temperature
  # relative to temp_spring -- picks the closest-approach day:
  #   - MEAN BELOW temp_spring (cold cell): the series only reaches up toward the threshold at its summer
  #     peak, so the closest approach (and where a marginally warmer year first crosses) is the WARMEST day.
  #   - MEAN ABOVE temp_spring (warm cell): the series only dips toward the threshold at its winter trough,
  #     so the closest approach is the COLDEST day (centroid coldest_doy, ~Jan) -- e.g. a warm subtropical
  #     cell, or a shallow-maritime cell whose real sowing is the winter trough, not midsummer.
  # sowing_month stays DEFAULT_MONTH (set just above), so the cell is still flagged "no real season"
  # (dflag) and the harvest too-cold guard still fires -- only the placeholder DOY moves.
  # Scalar if/else (not ifelse): the mean is only needed on the no-crossing fallback -- ifelse would
  # evaluate both arms for every cell.
  firstspringdoy <- if (firstspringdoy != -9999) firstspringdoy            # genuine up-crossing
                    else if (mean(daily_temp_x) > temp_spring) coldest_doy # warm cell -> winter trough (~Jan)
                    else warmest_doy                                       # cold cell -> summer peak

  # Wettest-window sowing DOY (crop-independent: calcCropCalendars computes it once and passes it in;
  # recompute only for direct callers). Hoisted out of the STYP branch because the winter-type PREC
  # sowing below now shares it. Prefer the daily 120-day SUM P / SUM PET rule (calcDoyWetMonth); without
  # the daily series, fall back to the monthly 4-month ratio-of-sums (.wetDoyMonthly) -- bug-free (it
  # sums P and PET separately, never the monthly P/PET ratios a near-zero-PET month makes explode), just
  # coarser (~1-month resolution).
  #
  # Only compute it where a downstream branch actually CONSUMES it: STYP uses it for PREC and PRECTEMP,
  # but WTYP uses it for PREC only (WTYP PRECTEMP/TEMP/TEMPPREC take the thermal autumn rule and never
  # read wet_doy). Scoping the guard this way avoids both a wasted computation AND a spurious stop() for
  # a winter-type PRECTEMP cell supplied with temperature but no P/PET.
  wet_doy_needed <- if (calcmethod_sdate == "WTYP_CALC_SDATE") seasonality == "PREC"
                    else                                       seasonality %in% c("PREC", "PRECTEMP")
  if (is.null(wet_doy) && wet_doy_needed) {
    if (!is.null(daily_prec) && !is.null(daily_pet)) {
      wet_doy <- calcDoyWetMonth(daily_prec, daily_pet,
                                 prev_doy = prev_wet_doy, eps = wet_window_eps,
                                 decay = wet_window_decay)
    } else if (!is.null(monthly_prec) && !is.null(monthly_pet)) {
      wet_doy <- .wetDoyMonthly(monthly_prec, monthly_pet)
    } else {
      stop("PREC/PRECTEMP sowing needs P and PET: supply daily_prec/daily_pet (preferred, ",
           "120-day window), monthly_prec/monthly_pet (4-month fallback), or a precomputed wet_doy.")
    }
  }

  # If winter type
  if (calcmethod_sdate == "WTYP_CALC_SDATE") {

    # Winter wheat dispatches on seasonality like the STYP crops in cells with NO thermal winter to
    # anchor to: the thermal autumn rule (coldest_doy-75 / temp_fall down-crossing) is meaningless there
    # (coldest_doy is the argmin of a near-flat temperature curve, swinging across the year). Only
    # PRECTEMP/TEMP/TEMPPREC -- which have a genuine thermal winter, the vernalization signal winter
    # wheat is defined by -- keep the thermal logic.
    if (seasonality == "NO_SEASONALITY") {

      # No thermal winter and no wet season: pin to the stable default, as every STYP crop does. Non-
      # viable cell -> dflag 0.
      sowing_month  <- DEFAULT_MONTH
      sowing_doy    <- DEFAULT_DOY
      sowing_season <- "spring"

    } else if (seasonality == "PREC") {

      # Wet season but no thermal winter: sow at the wettest-window onset, as every STYP crop does in
      # PREC cells. The favorable period is set by water, not temperature; wet_doy carries the wet-window
      # stickiness that damps flicker. A real season -> keep the real month (dflag 1).
      sowing_doy    <- wet_doy
      sowing_month  <- doy2month(sowing_doy)
      sowing_season <- "spring"

    } else if (firstwinterdoy > earliest_sdate &
               firstwintermonth != DEFAULT_MONTH) {

      sowing_month  <- firstwintermonth
      sowing_doy    <- firstwinterdoy
      sowing_season <- "winter"

    } else if (firstwinterdoy <= earliest_sdate &
               firstwintermonth != DEFAULT_MONTH) {

      # firstwinterdoy is a REAL autumn date (warm regime: coldest_doy-75; mild regime: temp_fall
      # down-crossing) but falls on/before the earliest allowed sowing date -> CLAMP to earliest_sdate
      # and winter-sow. The clamp applies to any viable (warm or mild) winter cell: without it, a mild
      # vernalizing cell whose autumn temp_fall crossing grazes earliest_sdate would be kicked to the
      # spring fallback on a one-day crossing wobble -- a half-year winter<->spring flip. Cold cells
      # (coldest_t < cold_thr) already set firstwinterdoy = -9999 above, so firstwintermonth ==
      # DEFAULT_MONTH routes them to the spring fallback below.
      sowing_month  <- earliest_smonth
      sowing_doy    <- earliest_sdate
      sowing_season <- "winter"

    } else {

      # No viable winter sowing found -> spring fallback, flagged "no real winter season" (dflag 0).
      # sowing_month is set to DEFAULT_MONTH here explicitly (a blanket WTYP override would also clobber
      # the PREC wet-season month set above).
      sowing_month  <- DEFAULT_MONTH
      sowing_doy    <- firstspringdoy
      sowing_season <- "spring"

    }

  } else {

    if (seasonality == "NO_SEASONALITY") {

      sowing_month <- DEFAULT_MONTH
      sowing_doy <- DEFAULT_DOY
      sowing_season <- "spring"

    } else if (seasonality == "PREC" || seasonality == "PRECTEMP") {

      # wet_doy computed above (now shared with the winter-type PREC branch).
      sowing_doy    <- wet_doy
      sowing_month  <- doy2month(sowing_doy)
      sowing_season <- "spring"

    } else {

      sowing_month <- firstspringmonth
      sowing_doy <- firstspringdoy
      sowing_season <- "spring"

    }
  } # STYP_CALC_SDATE

  # Defensive: convert any still-unresolved crossing (-9999) to the canonical default DOY. The
  # temperature spring fallback above already remaps the no-crossing case to the warmest day, so this
  # only catches genuinely unresolved DOYs from reduced / monthly-only call paths (it is keyed on the
  # DOY, not sowing_month, so it does not clobber the warmest-day anchor); NO_SEASONALITY already sets
  # sowing_doy = DEFAULT_DOY explicitly.
  sowing_doy    <- ifelse(
    sowing_doy == -9999, DEFAULT_DOY, sowing_doy
    )
  # Each WTYP spring-season branch sets sowing_month explicitly: the thermal spring fallback and
  # NO_SEASONALITY set DEFAULT_MONTH (dflag 0), while PREC keeps doy2month(wet_doy) (dflag 1).

  sd_vector <- list("sowing_month"  = sowing_month,
                    "sowing_doy"    = sowing_doy,
                    "sowing_season" = sowing_season)

  # Carry the winter regime (1 warm / 0 mild / -1 cold) for next year's boundary hysteresis above.
  attr(sd_vector, "winter_regime") <- winter_regime

  return(sd_vector)
}

