#' @title Calculate monthly climate
#'
#' @description Calculate monthly climate variables needed for the computation
#' of the rule-based crop calendars (Waha et al., 2012; Minoli et al., 2019):
#' average monthly mean temperature (mtemp);
#' average monthly cumulative precipitation (mprec);
#' average monthly cumulative potential evapotranspiration (mpet);
#' dryness index 1, mprec-to-mpet ratio (mppet);
#' dryness index 2, difference of mppet of two consecutive months (mppet_diff).
#' P-to-PET (mppet) ratio indicates the water surplus or deficit with respect to
#' the plant water demand; P-to-PET ratio difference (ppet_diff) indicates the
#' monthly trend in moisture conditions, if mppet_diff[m] > 0,
#' the trend is declining, indicating that the following month (m + 1) is
#' dryer than month m.
#'
#' @param lat latitude (decimal value)
#' @param temp daily mean temperature (degree Celsius) for a number of years
#' (syear:eyear). It should be passed in form of a vector.
#' @param prec daily precipitation (mm) for a number of years (syear:eyear).
#' It should be passed in form of a vector.
#' @param syear start year in the climate time series.
#' @param eyear end year in the climate time series.
#' @param incl_feb29 Does the time series include February 29th in leap years?
#' @param pet_method PET method: \code{"pt"} (default) for Priestley-Taylor
#'   equilibrium ET (calcPET), or \code{"fao56"} for FAO-56 Penman-Monteith
#'   reference ET (calcPET_FAO56).
#' @param swdown  surface downwelling shortwave radiation (W/m2, 24 h mean),
#'   vector matching length of \code{temp}. Used by both methods when supplied;
#'   if NULL with \code{pet_method = "pt"} falls back to orbital-geometry Rn.
#' @param lwdown  surface downwelling longwave radiation (W/m2, 24 h mean),
#'   vector matching length of \code{temp}. Required when \code{swdown} is given.
#' @param windspeed near-surface wind speed at 10 m (m/s). Required for
#'   \code{pet_method = "fao56"}.
#' @param humid   near-surface specific humidity (kg/kg, ISIMIP3b variable
#'   \code{huss}). Required for \code{pet_method = "fao56"}.
#' @param ps      surface air pressure (Pa). Used only with
#'   \code{pet_method = "fao56"}; defaults to 101325 Pa (sea level).
#'   \code{lat} and \code{day} (used internally for date handling and the PT
#'   orbital path) are not passed to \code{calcPET_FAO56}, which uses full
#'   24 h fluxes and requires no geometric daylength.
#'
#' @return list with five monthly vectors (length 12) and three daily vectors
#' (length 365):
#' \describe{
#'   \item{mtemp, mprec, mpet, mppet, mppet_diff}{Monthly climate (length 12).}
#'   \item{dtemp}{Climatological daily mean temperature (°C), one value per
#'     DOY 1–365, averaged across years. Used by \code{calcDoyCrossThreshold}.}
#'   \item{dprec, dpet}{Climatological daily precipitation and PET (mm), one
#'     value per DOY 1–365, averaged across years. Used by \code{calcDoyWetMonth}
#'     and the wet-season-end harvest rule as a spike-free \eqn{\sum P / \sum PET}.}
#' }
#'
#' @examples
#' d_temp <- matrix(rnorm(365*3, 15), nrow = 3)
#' d_prec <- matrix(rnorm(365*3, 3, 50), nrow = 3)
#' d_prec[d_prec <= 0] <- 0
#' calcClimatology(lat = 45, temp = d_temp, prec = d_prec,
#'                    syear = 2001, eyear = 2003)
#' @export

calcClimatology <- function(lat        = NULL,
                               temp       = NULL,
                               prec       = NULL,
                               syear      = NULL,
                               eyear      = NULL,
                               incl_feb29 = TRUE,
                               pet_method = c("pt", "fao56"),
                               swdown     = NULL,
                               lwdown     = NULL,
                               windspeed  = NULL,
                               humid      = NULL,
                               ps         = 101325
                               ) {

  pet_method <- match.arg(pet_method)

  if (pet_method == "fao56") {
    miss <- names(which(sapply(
      list(windspeed = windspeed, humid = humid, swdown = swdown, lwdown = lwdown),
      is.null)))
    if (length(miss) > 0)
      stop("pet_method = 'fao56' requires: ", paste(miss, collapse = ", "))
  }

  # Accumulate the monthly climate one year at a time via the shared, cell-vectorised
  # engine (initClimateAccum / addYearClimate / finalizeClimate). This
  # is a single grid cell, so the gridded pipeline (01a) and this per-pixel function
  # share one implementation of the aggregation, PET switch and DOY handling.
  if (incl_feb29) {
    dates <- seqDates(start_date = paste0(syear, "-01-01"),
                      end_date   = paste0(eyear, "-12-31"), step = "day")
  } else {
    dates <- createDateSeq(nstep = 365, years = syear:eyear)
  }
  yr <- date_to_year(dates)

  acc <- initClimateAccum(ncells = 1L, pet_method = pet_method)
  for (y in unique(yr)) {
    sel <- which(yr == y)
    sub <- function(x) if (is.null(x)) NULL else x[sel]
    acc <- addYearClimate(
      acc, temp = temp[sel], prec = prec[sel], dates = dates[sel],
      swdown = sub(swdown), lwdown = sub(lwdown),
      windspeed = sub(windspeed), humid = sub(humid),
      ps  = if (length(ps) > 1L) ps[sel] else ps,
      lat = lat
    )
  }

  mclm <- finalizeClimate(acc)
  for (f in c("mtemp", "mprec", "mpet", "mppet", "mppet_diff")) {
    names(mclm[[f]]) <- seq_len(12)
  }
  mclm
}
