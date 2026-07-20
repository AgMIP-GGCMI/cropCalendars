#' @title Calculate Required Vernalization Days
#'
#' @param temp_mean_month  = sample(-10:30, 12), # monthly mean temperature
#' @param max.vern.days maximum vernalization requirements (days)
#' @param max.vern.months maximum vernalization
#' @param tv2 vern.temp.opt.min
#' @param tv3 vern.temp.opt.max
#'
#' @details Single-cell wrapper around the vectorised core \code{.calc_vd_vec} (the one
#'   source of truth, in \code{generatePHUTserie_isimip3.R}): rank the months coldest-first,
#'   keep the \code{max.vern.months} coldest, credit each a piecewise vernalization-day
#'   amount (full below \code{tv2}, zero above \code{tv3}, linear ramp between), sum and round.
#'@export
calcVd <- function(temp_mean_month  = sample(-10:30, 12), # monthly mean temperature
                   max.vern.days   = 70,     # maximum vernalization requirements (days)
                   max.vern.months = 5,      # maximum vernalization
                   tv2             = 3,      # vern.temp.opt.min
                   tv3             = 10      # vern.temp.opt.max
                   ) {

  .calc_vd_vec(matrix(temp_mean_month, nrow = 1L), max.vern.days,
               max.vern.months = max.vern.months, tv2 = tv2, tv3 = tv3)[1L]
}
