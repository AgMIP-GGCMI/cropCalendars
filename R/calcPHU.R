#' @title Calculate PHU requirements
#'
#' @param sdate sowing date (DOY)
#' @param hdate maturity date (DOY)
#' @param mdt daily mean temperature length(mdt) == 365
#' @param vern_factor from calcVf()
#' @param basetemp minimum cardinal temperature
#' @param phen_model thermal = "t"; thermal-vernal = "tv"; thermal-photo = "tp";
#' thermal-vernal-photo = "tvp"
#'
#' @details Single-cell wrapper around the vectorised cores
#'   \code{.calc_phu_thermal_vec} / \code{.calc_phu_vernal_vec} (the one source of truth, in
#'   \code{generatePHUTserie_isimip3.R}): sum the daily effective thermal units
#'   \code{max(mdt - basetemp, 0)} (scaled by \code{vern_factor} for the \code{"tv"} model)
#'   over the growing window \code{[sdate, hdate)}, wrapping the year when \code{hdate <= sdate}.
#'   The \code{"tv"} sum is returned negated (LPJmL reads a negative PHU as "needs
#'   vernalization"). Returns 0 when \code{sdate}/\code{hdate} is \code{NA} or 0.
#'
#'   NOTE: this now matches the vectorised core used by the pipeline, which fixes two
#'   edge bugs in the former scalar body — at \code{sdate == 1} the old code dropped day 1
#'   from the growing period, and at \code{sdate == hdate} it dropped ~2 days; both are
#'   handled correctly here. Identical to the old scalar for all other inputs.
#' @export
calcPHU <- function(sdate       = NA, # sowing date (DOY)
                    hdate       = NA, # maturity date (DOY)
                    mdt         = rep(NA, 365), # daily mean temperature
                    vern_factor = rep(1, 365), # from calc.vf()
                    basetemp    = 0,    # minimum cardinal temperature
                    phen_model  = "t" # "tv", "tp", "tvp"
                    ) {

  if (is.na(sdate) || is.na(hdate) || sdate == 0 || hdate == 0) return(0L)

  m <- matrix(mdt, nrow = 1L)
  if (phen_model == "t") {
    .calc_phu_thermal_vec(sdate, hdate, m, basetemp)
  } else if (phen_model == "tv") {
    .calc_phu_vernal_vec(sdate, hdate, m, matrix(vern_factor, nrow = 1L), basetemp)
  } else {
    stop("calcPHU: phen_model must be 't' (thermal) or 'tv' (thermal-vernal)")
  }
}
