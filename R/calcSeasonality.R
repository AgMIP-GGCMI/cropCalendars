#' @title Calculate seasonality type
#'
#' @description Calculate seasonality type based on average monthly climate
#' as from Waha et al. (2012).
#'
#' @param monthly_temp Numeric vector of length 12. Average (e.g. 20-years)
#' monthly mean temperatures (degree Celsius).
#' @param monthly_prec Numeric vector of length 12. Average (e.g. 20-years)
#' monthly cumulative precipitation (mm).
#' @param temp_min Threshold of temperature of the coldest month
#' (degree Celsius). Default value is 10.
#' @param prev_seas Character seasonality type chosen for the \emph{previous} year
#'   (one of the five class strings), or \code{NA} (default) for no prior state.
#'   Used only when \code{seas_eps > 0} to apply threshold hysteresis.
#' @param seas_eps Non-negative numeric (default 0 = off). Relative deadband on the
#'   classifier thresholds for the sliding window. The class is a tree of threshold
#'   tests on three nearly-continuous variables (\code{CV_prec} vs 0.4,
#'   \code{CV_temp} vs 0.010, \code{min_temp} vs \code{temp_min}); a cell whose
#'   variable grazes a threshold flips class — and hence its whole sowing rule —
#'   between adjacent years. With \code{seas_eps > 0} and a \code{prev_seas}, each
#'   threshold is relaxed toward keeping last year's class: a test the previous
#'   class was on the high side of uses \code{thr*(1-seas_eps)}, otherwise
#'   \code{thr*(1+seas_eps)} (thermostat-style deadband). So the class only switches
#'   when a variable moves \emph{decisively} past its boundary, suppressing graze
#'   flips while still following a genuine multi-decadal shift. \code{seas_eps ~0.25}
#'   cuts year-to-year class flips ~85\%. With \code{seas_eps = 0} or
#'   \code{prev_seas = NA} the plain Waha thresholds are used (backward compatible).
#' @param mtemp_margin Absolute deadband (deg C) applied to the \code{min_temp}
#'   test when \code{seas_eps > 0} (default 1). A relative \code{seas_eps} is not
#'   meaningful on a temperature threshold, so an absolute margin is used there.
#' @param daily_temp Numeric vector of length 365, the climatological daily mean
#'   temperature (the \code{dtemp} element from \code{calcClimatology}). When
#'   supplied, the coldest-month test uses the coldest 30-day window mean of this
#'   series instead of \code{min(monthly_temp)} (continuous, no month-boundary
#'   quantisation). The CV classifiers stay on the 12 monthly values -- a daily CV
#'   has far larger variance and would invalidate the calibrated 0.4 / 0.010
#'   thresholds. \code{NULL} (default) keeps the monthly minimum (backward compatible).
#' @param smooth_window Integer day-window for the daily-climatology reductions (here the
#'   coldest-window mean); the single global smoothing window (default 31). See
#'   \code{calcCropCalendars}.
#'
#' @export
calcSeasonality <- function(monthly_temp,
                            monthly_prec,
                            temp_min     = 10,
                            prev_seas    = NA_character_,
                            seas_eps     = 0,
                            mtemp_margin = 1,
                            daily_temp   = NULL,
                            smooth_window = 31L
                            ) {

  var_coeff_prec <- calcVarCoeff(monthly_prec)
  var_coeff_temp <- calcVarCoeff(deg2k(monthly_temp))
  # Coldest-month temperature: daily coldest-30-day-window mean when the daily
  # climatology is supplied, else the calendar-month minimum (see @param daily_temp).
  min_temp       <- if (!is.null(daily_temp)) .coldestWindowMean(daily_temp, width = smooth_window) else min(monthly_temp)

  # Threshold deadband (hysteresis). With no prior state / seas_eps = 0 these are
  # the plain Waha thresholds, so the classification is unchanged (backward compatible).
  if (is.na(prev_seas) || seas_eps <= 0) {
    thr_prec <- 0.4; thr_temp <- 0.010; thr_mint <- temp_min
  } else {
    thr_prec <- if (prev_seas %in% c("PREC", "PRECTEMP", "TEMPPREC")) 0.4   * (1 - seas_eps) else 0.4   * (1 + seas_eps)
    thr_temp <- if (prev_seas %in% c("PRECTEMP", "TEMPPREC", "TEMP"))  0.010 * (1 - seas_eps) else 0.010 * (1 + seas_eps)
    thr_mint <- if (prev_seas == "PRECTEMP") temp_min - mtemp_margin else temp_min + mtemp_margin
  }

  has_prec <- var_coeff_prec > thr_prec
  has_temp <- var_coeff_temp > thr_temp
  is_warm  <- min_temp      > thr_mint

  if      (!has_prec && !has_temp)             seasonality <- "NO_SEASONALITY"
  else if ( has_prec && !has_temp)             seasonality <- "PREC"
  else if ( has_prec &&  has_temp &&  is_warm) seasonality <- "PRECTEMP"
  else if ( has_prec &&  has_temp && !is_warm) seasonality <- "TEMPPREC"
  else                                         seasonality <- "TEMP"

  return(seasonality)
}
