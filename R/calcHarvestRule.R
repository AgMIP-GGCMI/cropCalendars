#' @title Calculate harvest rule (Minoli et al., 2019)
#'
#' @description This function performs an agro-climatic classification of
#' climate, based on monthly temperature and precipitation profiles.
#' The classification is derived by intersecting the seasonality classes (see
#' calcSeasonality) with the temperature of the warmest month, compared to
#' crop-specific thresholds (base and optimal temperatures for reproductive
#' growth):
#' t-low, temperatures always lower than the base temperature;
#' t-mid, temperatures exceed the base temperature, but are always lower than
#' the optimum temperature;
#' t-high, temperatures exceed the optimum temperature.
#'
#' @seealso calcSeasonality
#'
#' @param daily_temp Numeric vector of length 365, the climatological daily mean
#'   temperature (\code{dtemp} from \code{calcClimatology}). When supplied, the
#'   warmest-month temperature uses the warmest 30-day window mean of this series
#'   instead of \code{max(monthly_temp)} (continuous, no month-boundary
#'   quantisation). \code{NULL} (default) uses the monthly maximum (backward compatible).
#' @param prev_tclass Integer thermal class chosen last year (0 = t-low, 1 = t-mid,
#'   2 = t-high), or \code{NA} (default). Used only when \code{harv_tmax_margin > 0}
#'   to apply a deadband on the base/optimum temperature thresholds.
#' @param harv_tmax_margin Absolute deadband (deg C, default 0 = off) on the
#'   \code{temp_base_rphase} / \code{temp_opt_rphase} thresholds: with a prior class,
#'   each threshold is relaxed toward keeping last year's thermal class (thermostat),
#'   so a sub-degree \code{temp_max} wobble between sliding windows no longer flips the
#'   harvest rule -- and hence the whole harvest formula. Mirrors \code{seas_mtemp_margin}.
#' @param smooth_window Integer day-window for the daily-climatology reductions (here the
#'   warmest-window mean driving the thermal class); the single global smoothing window
#'   (default 31). See \code{calcCropCalendars}.
#' @export
calcHarvestRule <- function(croppar,
                            monthly_temp,
                            monthly_ppet,
                            seasonality,
                            daily_temp       = NULL,
                            prev_tclass      = NA_integer_,
                            harv_tmax_margin = 0,
                            smooth_window    = 31L
                            ) {

  # extract individual parameter names and values
  list2env(croppar, environment())  # 1-row data.frame: columns -> scalar params

  # Warmest-month temperature driving the t-low/-mid/-high split: daily
  # warmest-30-day-window mean when the daily climatology is supplied, else the
  # calendar-month maximum.
  temp_max <- if (!is.null(daily_temp)) .warmestWindowMean(daily_temp, width = smooth_window) else max(monthly_temp)

  # Thermal class (0 = t-low, 1 = t-mid, 2 = t-high) from temp_max vs the crop's base
  # and optimum reproductive-phase temperatures. With harv_tmax_margin > 0 and a prior
  # class, a thermostat deadband (absolute harv_tmax_margin) relaxes each threshold toward
  # keeping last year's class, so temp_max must move decisively past base/opt to flip
  # the class (and the whole harvest formula) -- the seas_eps pattern on the harvest
  # rule's thermal split.
  thr_base <- temp_base_rphase
  thr_opt  <- temp_opt_rphase
  if (harv_tmax_margin > 0 && !is.na(prev_tclass)) {
    thr_base <- if (prev_tclass >= 1L) thr_base - harv_tmax_margin else thr_base + harv_tmax_margin
    thr_opt  <- if (prev_tclass >= 2L) thr_opt  - harv_tmax_margin else thr_opt  + harv_tmax_margin
  }
  tclass <- if (temp_max <= thr_base) 0L else if (temp_max <= thr_opt) 1L else 2L

  grp <- switch(seasonality, "NO_SEASONALITY" = 1L, "PREC" = 2L, 3L)   # 1 no-seas, 2 prec, 3 mix
  rule_tab <- matrix(c(1, 4, 7,  2, 5, 8,  3, 6, 9), nrow = 3, byrow = TRUE)
  name_tab <- matrix(c("t-low_no-seas",  "t-mid_no-seas",  "t-high_no-seas",
                       "t-low_prec-seas","t-mid_prec-seas","t-high_prec-seas",
                       "t-low_mix-seas", "t-mid_mix-seas", "t-high_mix-seas"),
                     nrow = 3, byrow = TRUE)
  harvest_rule <- rule_tab[grp, tclass + 1L]
  names(harvest_rule) <- name_tab[grp, tclass + 1L]
  attr(harvest_rule, "tclass") <- tclass   # carried as prev_tclass hysteresis state
  return(harvest_rule)
}
