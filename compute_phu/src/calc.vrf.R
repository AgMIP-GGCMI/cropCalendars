# ------------------------------------------------------# ----
# Project: crop_calendars
#
# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
#
# Date Created: 2021-04-08
#
# Description:
# # Calculate Vernalization reduction factor
# # Based on scripts by Jonas Jägermeyr
# ------------------------------------------------------#

rm(list=ls(all=T))

# ----

calc.vrf <- function(sdate = NA, # sowing date (DOY)
                     hdate = NA, # maturity date (DOY)
                     mdt   = rep(NA, 365), # daily mean temperature
                     vd    = 0,              # vern requirement (days) from calc.vreq()
                     vd_b  = 0.2,            # vern ineffective until 20% vd is met
                     max.vern.days   = 70,   # maximum vernalization requirements (days)
                     max.vern.months = 5,    # maximum vernalization
                     tv1             = -4,   # vern.temp.min
                     tv2             = 3,    # vern.temp.opt.min
                     tv3             = 10,   # vern.temp.opt.max
                     tv4             = 17) { # vern.temp.max
  
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
  hd <- ifelse(sdate < hdate, hdate, hdate+365)
  while (vdsum < vd) {
    vdsum <- vdsum + veff[k]
    if (k>hd || vdsum>=vd) break
    k <- k + 1
  }
  
  if (vdsum >= vd) {
    endday.vern <- k
  } else {
    endday.vern <- (-10)
  }
  
  # ------------------------------------------------------#
  
  # Calculate Vernalization Reduction Factor (vf) for each day between
  #  sdate and day when Vreq are met
  
  if (endday.vern > 0) {
    
    vdsum <- 0
    
    for (k in sdate:endday.vern) {
      
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
    
  } # if (endday.vern > 0)
  
  # in cases of no vernalization requirements
  #  (winter crop in warm regions or spring crop)
  if (vd == 0) {
    vrf <- 1
    endday.vern <- sdate
  }
  
  endday.vern <- ifelse(endday.vern>365, endday.vern-365, endday.vern)
  
  return(list("vrf" = vrf,
              "endday.vern" = endday.vern))
  
}

# # For testing
# # ------------------------------------------------------#
# mdt   = round(runif(365,-10,30)) # daily mean temperature
# calc.vrf(sdate = 300, hdate = 160, mdt = mdt, vd = vd)
