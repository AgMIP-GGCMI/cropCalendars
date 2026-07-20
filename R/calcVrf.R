#' @title Calculate Vernalization Reduction Factor
#'
#' @param sdate sowing date (DOY)
#' @param hdate maturity date (DOY)
#' @param mdt maturity date (DOY)
#' @param vd vern requirement (days) from calc.vreq()
#' @param vd_b vern ineffective until 20 \% of vd is met
#' @param max.vern.days maximum vernalization requirements (days)
#' @param max.vern.months maximum vernalization
#' @param tv1 vern.temp.min
#' @param tv2 vern.temp.opt.min
#' @param tv3 vern.temp.opt.max
#' @param tv4 vern.temp.max
#'
#' @details Single-cell wrapper around the vectorised core \code{.build_vrf_mat} (the one
#'   source of truth, in \code{generatePHUTserie_isimip3.R}). Daily vernalization
#'   effectiveness is a trapezoid in temperature (0 below \code{tv1}, ramp to 1 across
#'   \code{[tv1,tv2]}, plateau on \code{[tv2,tv3]}, ramp back to 0 across \code{[tv3,tv4]});
#'   accumulating it from \code{sdate}, the reduction factor stays 0 until \code{vd_b} (20\%)
#'   of the requirement \code{vd} is banked, then ramps linearly to 1 at full \code{vd}.
#'   \code{vd <= 0} returns all-ones (no requirement). \code{max.vern.days}/
#'   \code{max.vern.months} are vestigial (unused by the body, as in the original).
#'   Equivalent to the old scalar to ~1e-14 (the cumsum core reorders the summation), and
#'   returns a plain numeric vector (the old 1-D-\code{array} dim attribute is dropped).
calcVrf <- function(sdate           = NA,
                    hdate           = NA,
                    mdt             = rep(NA, 365),
                    vd              = 0,
                    vd_b            = 0.2,
                    max.vern.days   = 70,
                    max.vern.months = 5,
                    tv1             = -4,
                    tv2             = 3,
                    tv3             = 10,
                    tv4             = 17
                    ) {

  as.vector(.build_vrf_mat(sdate, hdate, matrix(mdt, nrow = 1L), vd,
                           vd_b = vd_b, tv1 = tv1, tv2 = tv2, tv3 = tv3, tv4 = tv4))
}
