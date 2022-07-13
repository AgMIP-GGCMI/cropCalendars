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
#' @export
calcPhu <- function(sdate       = NA, # sowing date (DOY)
                    hdate       = NA, # maturity date (DOY)
                    mdt         = rep(NA, 365), # daily mean temperature
                    vern_factor = rep(1, 365), # from calc.vf()
                    basetemp    = 0,    # minimum cardinal temperature
                    phen_model  = "t" # "tv", "tp", "tvp"
) {

  husum <- 0

  if (is.na(sdate) | is.na(hdate) | sdate == 0 | hdate == 0) { return(husum) }

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
  if (phen_model == "t") {

    husum <- as.integer(sum(teff))

  } else if (phen_model == "tv") {

    # negative phus are recognized by lpjml as requiring vernalization
    husum <- as.integer(sum(teff))*(-1L)

  } else {

    cat ("\nError: phen_model not declared!")
    stop()

  }

  return(husum)

}
