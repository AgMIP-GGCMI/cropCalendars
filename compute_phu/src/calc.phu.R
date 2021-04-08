# ------------------------------------------------------# ----
# Project: crop_calendars
#
# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
#
# Date Created: 2021-04-08
#
# Description:
# # Calculate Thermal Unit Requirements (PHUs) as in LPJmL
# # Based on scripts by Jonas Jägermeyr
# ------------------------------------------------------#

rm(list=ls(all=T))

# ----

calc.phu <- function(sdate = NA, # sowing date (DOY)
                     hdate = NA, # maturity date (DOY)
                     mdt         = rep(NA, 365), # daily mean temperature
                     vern_factor = rep(1, 365), # from calc.vf()
                     basetemp    = 0,    # minimum cardinal temperature
                     phen_model  = "t" # "tv", "tp", "tvp"
                     ) {
  
  husum <- 0
  
  # Select days not in growing period
  hdate <- ifelse(sdate < hdate, hdate, hdate+365)
  if (hdate <= 365) days_no_gp <- c(1:(sdate-1),hdate:365)
  if (hdate >  365) days_no_gp <- c((hdate-365):(sdate-1))
  
  # Compute Effective Thermal Units (teff)
  
  # Thermal time only
  if (phen_model == "t") {
    
    teff <- mdt-basetemp       # daily temp - base temp
    teff[teff<0] <- 0          # teff cannot be < 0
    teff[days_no_gp] <- 0      # Remove days outside growing period
    
  # Vernal-Thermal  
  } else if (phen_model == "tv") {
    
    teff <- mdt-basetemp       # daily temp - base temp
    teff[teff<0] <- 0          # teff cannot be < 0
    teff <- teff * vern_factor # Vernalization reduction factor
    teff[days_no_gp] <- 0      # Remove days outside growing period
    
  } else {
    
    cat ("\nError: phen_model not declared!")
    stop()
    
  } # phen_model
  
  # Total Thermal Unit Requirements
  husum <- sum(teff)
  
  return(husum)
  
} # calc.phu()

# For testing
# ------------------------------------------------------#
# mdt   = round(runif(365,-10,30)) # daily mean temperature
# calc.phu(sdate = 300, hdate = 160, mdt = mdt, phen_model = "tv", vern_factor = runif(365))
