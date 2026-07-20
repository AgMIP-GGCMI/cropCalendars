#' @title Tests if a given growing season should be classified as winter crop
#'
#' @param start Sowing date as day of the year (DOY)
#' @param end Harvest (or maturity) date as day of the year (DOY)
#' @param tcm Temperature of the coldest month (deg C)
#' @param lat Latitude (decimal degrees)
#'
#' @details This is the rule suggested by Portman et al. 2010, slightly
#' changed in that <= 7 instead of 6°C is used.
#'
#' Single-cell wrapper around the vectorised core \code{.wintercrop_vec} (the one source of
#' truth, in \code{generatePHUTserie_isimip3.R}): a long season (growp >= 150 d) whose
#' coldest month is cold-but-not-killing (tcm in [-10, 7]) and that overwinters -- crossing
#' the year boundary in the N hemisphere, or mid-winter (DOY 182) in the S -- is a winter
#' crop (1), else 0. Returns 0/1 (integer). Unlike the former scalar body it returns NA
#' rather than erroring when \code{end} is NA.
#' @export
isWinterCrop <- function(start = NULL,
                         end   = NULL,
                         tcm   = NULL,
                         lat   = NULL
                         ) {

  .wintercrop_vec(start, end, tcm, lat)[1L]

}
