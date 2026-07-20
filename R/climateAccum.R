#' @title Streaming, cell-vectorised monthly-climate accumulator
#'
#' @description Building blocks for computing the average monthly climate (and the
#' daily DOY climatologies) used by the rule-based crop calendars, without holding
#' the full multi-year daily series in memory at once. Years are added one at a
#' time; each call is vectorised over an arbitrary number of grid cells.
#'
#' `calcClimatology()` is a thin per-pixel wrapper around these; the gridded
#' pipeline (utils/ggcmi_ph3/01a) feeds the full grid one year at a time. Both share
#' this single implementation so the aggregation, the PET method switch, the P/PET
#' flooring and the leap-year DOY handling live in exactly one place.
#'
#' Aggregation (reproduces the previous `tapply`-based code):
#' \itemize{
#'   \item monthly fields = mean over years of the per-year monthly statistic
#'         (mean temperature; summed precipitation, PET; summed P/PET ratio);
#'   \item daily fields (`dtemp`, `dprec`, `dpet`) = per-DOY sum / per-DOY count over all
#'         days of all years, i.e. `tapply(., DOY, mean)` (the count is not uniform:
#'         `date_to_doy(skip_feb29 = TRUE)` folds Feb 29 onto DOY 59).
#' }
#'
#' @param ncells number of grid cells the accumulator covers.
#' @param pet_method PET method, \code{"pt"} (Priestley-Taylor, calcPET) or
#'   \code{"fao56"} (FAO-56 Penman-Monteith, calcPET_FAO56).
#' @param daily_only If \code{TRUE}, accumulate and return only the daily fields
#'   (\code{dtemp}, \code{dprec}, \code{dpet}) -- the monthly aggregates are neither
#'   allocated nor computed. Used by the sliding
#'   ring, whose only consumer reconstructs the monthly seasonality stats from the
#'   daily climatology downstream. Default \code{FALSE} = the full output.
#' @return `initClimateAccum` returns an accumulator (a list of zeroed matrices);
#'   `finalizeClimate` returns the climate as a list of fields (vectors when
#'   `ncells == 1`, otherwise `[ncells x 12]` / `[ncells x 365]` matrices): the full
#'   eight fields matching `calcClimatology()`, or only `dtemp`/`dprec`/`dpet` when
#'   the accumulator was built with `daily_only = TRUE`.
#' @name climateAccum
#' @export
initClimateAccum <- function(ncells, pet_method = c("fao56", "pt"),
                               daily_only = FALSE) {
  pet_method <- match.arg(pet_method)
  acc <- list(
    # Separate daily P and PET sums so the wet-season rules can use a spike-free
    # ratio-of-sums (ΣP/ΣPET) instead of the mean of daily P/PET ratios, which
    # blows up on the rare day where PET ≈ 0.
    D_tsum   = matrix(0, ncells, 365), D_prsum  = matrix(0, ncells, 365),
    D_petsum = matrix(0, ncells, 365), D_cnt    = numeric(365),
    nyears = 0L, pet_method = pet_method, ncells = ncells,
    daily_only = daily_only
  )
  # The monthly accumulators (mtemp/mprec/mpet/mppet) are only needed for the per-pixel
  # calcClimatology() full output. The sliding ring runs daily_only -- it accumulates only
  # the daily fields, and the seasonality monthly stats are reconstructed from the daily
  # climatology downstream -- which also trims the ring's per-slot footprint.
  if (!daily_only) {
    acc$M_tas <- matrix(0, ncells, 12); acc$M_pr    <- matrix(0, ncells, 12)
    acc$M_pet <- matrix(0, ncells, 12); acc$M_ppet  <- matrix(0, ncells, 12)
  }
  acc
}

# Daily PET for one year, vectorised over cells. temp/swdown/... are [ncells x ndays].
# fao56 and pt-with-radiation are elementwise; pt without radiation (orbital fallback)
# is scalar per day and only supported for a single cell (the per-pixel path).
.petDaily <- function(temp, swdown, lwdown, windspeed, humid, ps, lat, day, method) {
  if (method == "fao56") {
    return(calcPET_FAO56(temp, windspeed, humid, swdown, lwdown, ps))
  }
  if (!is.null(swdown) && !is.null(lwdown)) {
    return(calcPET(temp, swdown = swdown, lwdown = lwdown)) # observed Rn: no lat/day needed
  }
  if (nrow(temp) > 1L) {
    stop("pet_method = 'pt' without swdown/lwdown is not cell-vectorised; supply radiation.")
  }
  matrix(mapply(calcPET, temp = temp[1, ], lat = lat, day = day), nrow = 1L)
}

#' @rdname climateAccum
#' @param acc accumulator from \code{initClimateAccum}.
#' @param temp,prec daily mean temperature (deg C) and precipitation (mm) for one
#'   calendar year: a vector (single cell) or an \code{[ncells x ndays]} matrix.
#' @param dates character vector of the year's dates ("YYYY-MM-DD"), length ndays.
#' @param swdown,lwdown,windspeed,humid,ps daily forcings matching \code{temp}'s
#'   shape (see \code{calcClimatology}); \code{ps} may be scalar.
#' @param lat latitude(s); only used by the Priestley-Taylor orbital fallback.
#' @export
addYearClimate <- function(acc, temp, prec, dates,
                                  swdown = NULL, lwdown = NULL, windspeed = NULL,
                                  humid = NULL, ps = 101325, lat = NULL) {
  as_mat <- function(x) if (is.null(x) || !is.null(dim(x))) x else matrix(x, nrow = 1L)
  temp <- as_mat(temp); prec <- as_mat(prec)
  swdown <- as_mat(swdown); lwdown <- as_mat(lwdown)
  windspeed <- as_mat(windspeed); humid <- as_mat(humid); ps <- as_mat(ps)

  mon <- date_to_month(dates)
  doy <- date_to_doy(dates, skip_feb29 = TRUE)

  pet  <- .petDaily(temp, swdown, lwdown, windspeed, humid, ps, lat, doy, acc$pet_method)

  daily_only <- isTRUE(acc$daily_only)
  if (!daily_only) {
    for (m in 1:12) {
      dom     <- which(mon == m)
      pr_mon  <- rowSums(prec[, dom, drop = FALSE])
      pet_mon <- rowSums(pet[,  dom, drop = FALSE])
      acc$M_tas[,  m] <- acc$M_tas[,  m] + rowMeans(temp[, dom, drop = FALSE])
      acc$M_pr[,   m] <- acc$M_pr[,   m] + pr_mon
      acc$M_pet[,  m] <- acc$M_pet[,  m] + pet_mon
      acc$M_ppet[, m] <- acc$M_ppet[, m] + pr_mon / pmax(pet_mon, 1e-6)
    }
  }

  # A DOY can recur within a leap year (Feb 29 -> 59), so accumulate sum and count.
  for (k in seq_len(ncol(temp))) {
    d <- doy[k]
    acc$D_tsum[,   d] <- acc$D_tsum[,   d] + temp[, k]
    acc$D_prsum[,  d] <- acc$D_prsum[,  d] + prec[, k]
    acc$D_petsum[, d] <- acc$D_petsum[, d] + pet[,  k]
  }
  acc$D_cnt  <- acc$D_cnt + tabulate(doy, nbins = 365)
  acc$nyears <- acc$nyears + 1L
  acc
}

#' @rdname climateAccum
#' @export
finalizeClimate <- function(acc) {
  if (acc$nyears == 0L) stop("No years accumulated.")
  dtemp <- sweep(acc$D_tsum,   2, acc$D_cnt, "/")
  dprec <- sweep(acc$D_prsum,  2, acc$D_cnt, "/")
  dpet  <- sweep(acc$D_petsum, 2, acc$D_cnt, "/")

  # The daily climatology is returned RAW; the global daily-climatology smoothing
  # (smooth_window) is applied at rule time (see .circRoll and the rule stage),
  # so it is tunable without recomputing this climatology.

  # daily_only accumulators (the sliding ring) carry no monthly sums; the seasonality
  # monthly stats are reconstructed from the daily climatology downstream.
  if (isTRUE(acc$daily_only)) {
    out <- list(dtemp = dtemp, dprec = dprec, dpet = dpet)
  } else {
    mtemp      <- round(acc$M_tas  / acc$nyears, 5)
    mprec      <- round(acc$M_pr   / acc$nyears, 5)
    mpet       <- round(acc$M_pet  / acc$nyears, 5)
    mppet      <- round(acc$M_ppet / acc$nyears, 5)
    mppet_diff <- mppet - mppet[, c(2:12, 1), drop = FALSE]
    out <- list(mtemp = mtemp, mprec = mprec, mpet = mpet, mppet = mppet,
                mppet_diff = mppet_diff, dtemp = dtemp,
                dprec = dprec, dpet = dpet)
  }
  if (acc$ncells == 1L) out <- lapply(out, as.vector) # match per-pixel contract
  out
}
