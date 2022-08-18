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

  # Initialize Vernalization Components
  veff        <- array(0, 365)   # Vernalization Effectiveness
  endday.vern <- 0               # Day of end of vernalization period
  vrf <- array(1.0, 365) # Vernalization Reduction Factor

  # ------------------------------------------------------#

  # Calculate Vernalization Effectiveness for each day of the year
  # veff == 1 means full vernalization day

  for (k in 1:365) {

    if      (mdt[k] >= tv1 && mdt[k] <  tv2) veff[k] <- (mdt[k]-tv1)/(tv2-tv1)
    else if (mdt[k] >= tv2 && mdt[k] <= tv3) veff[k] <- 1
    else if (mdt[k] >  tv3 && mdt[k] <  tv4) veff[k] <- (tv4-mdt[k])/(tv4-tv3)
    else if (mdt[k] >= tv4)                  veff[k] <- 0
    else if (mdt[k] <  tv1)                  veff[k] <- 0
    else {
      print(paste("Stop! no veff associated on day:", k))
      break
    }
  }
  veff[veff>1] <- 1 # cannot be larger 1
  veff[veff<0] <- 0 # cannot be smaller 0

  veff <- c(veff, veff) # Repeat twice

  # ------------------------------------------------------#

  # Calculate Day when Vernalization Requirements (vd) are met,
  #  starting from sdate
  vdsum <- 0
  k     <- sdate
  hd    <- ifelse(sdate < hdate, hdate, hdate+365)
  while (vdsum < vd && k < hd) {
    vdsum <- vdsum + veff[k]
    if (vdsum < vd) k <- k + 1
  }

  if (vdsum >= vd) {
    endday.vern <- k
  } else {
    endday.vern <- Inf
  }

  # ------------------------------------------------------#

  # Calculate Vernalization Reduction Factor (vf) for each day between
  #  sdate and day when Vreq are met

  vdsum <- 0

  for (k in sdate:min(endday.vern, hd)) {

    vdsum <- vdsum + veff[k]

    # vrf = 0 until 20% vernalization requirements reached
    # if VDD < 20% Vreq (Vb)
    if (vdsum<(vd*vd_b)) {
      vrf[ifelse(k>365,k-365,k)] <- 0.0
    } else {
      vrf.tmp <- max(0, min(1, (vdsum-(vd*vd_b))/(vd-(vd*vd_b)) )  )
      vrf[ifelse(k>365,k-365,k)] <- vrf.tmp
    }
  } # k


  # in cases of no vernalization requirements
  #  (winter crop in warm regions or spring crop)
  if (vd == 0) {
    vrf <- 1
    endday.vern <- sdate
  }

  return(vrf)

}
