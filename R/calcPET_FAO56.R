#' @title Calculate FAO-56 reference evapotranspiration (ET0)
#'
#' @description Penman-Monteith reference ET for a hypothetical grass surface
#' (FAO Irrigation and Drainage Paper No. 56, Allen et al. 1998).
#'
#' All fluxes use 24 h means (standard FAO-56 daily formulation):
#'   Rns = (1 - 0.23) * rsds * 86400  [J/m2/day]
#'   Rnl = (rlds - sigma*T^4) * 86400  [J/m2/day]
#'   aerodynamic term integrated over full day
#'
#' Reference crop parameters (FAO 1998):
#'   albedo = 0.23, surface resistance rs = 70 s/m,
#'   roughness length z0m = 0.123 * 0.12 m = 0.01476 m
#'
#' Aerodynamic resistance follows LPJmL getpet.c (cropref = TRUE):
#'   ustar = u10 * 0.41 / ln(10 / z0m)
#'   raH   = ln(2 / (0.1*z0m)) / (0.41 * ustar)
#'
#' @param temp      daily mean temperature (degree Celsius)
#' @param windspeed wind speed at 10 m height (m/s)
#' @param humid     near-surface specific humidity (kg/kg) -- variable huss in ISIMIP3b
#' @param swdown    surface downwelling shortwave radiation (W/m2, 24 h mean) -- rsds
#' @param lwdown    surface downwelling longwave radiation (W/m2, 24 h mean) -- rlds
#' @param ps        surface air pressure (Pa). Default 101325 Pa (sea level).
#'
#' @return FAO-56 reference evapotranspiration ET0 (mm/day)
#' @export

calcPET_FAO56 <- function(temp,
                           windspeed,
                           humid,
                           swdown,
                           lwdown,
                           ps = 101325
                           ) {

  sigma  <- 5.67e-8
  Mair   <- 0.0289652
  Mvap   <- 0.018016
  rugas  <- 8.31446
  cp     <- 1003.5
  d622   <- Mvap / Mair
  d378   <- 1 - d622

  # FAO-56 reference crop aerodynamic resistance [s/m] (getpet.c lines 73-80)
  z0m   <- 0.123 * 0.12
  ustar <- windspeed * 0.41 / log(10 / z0m)
  raH   <- log(2 / (0.1 * z0m)) / (0.41 * ustar)

  # Vapor pressures [Pa] from specific humidity (getpet.c lines 81-82)
  e    <- ps * humid / (d622 + d378 * humid)
  esat <- 610.78 * exp(17.269 * temp / (237.3 + temp))

  # Slope of saturation vapor pressure curve [Pa/K] (getpet.c line 83)
  s <- 2502936 * exp(17.269 * temp / (237.3 + temp)) / (237.3 + temp)^2

  # Psychrometric constant and latent heat (getpet.c macros)
  gamma_t <- 65.05 + temp * 0.064
  lambda  <- 2.495e6 - temp * 2380

  # Air density [kg/m3] (getpet.c line 84)
  rho <- (ps * Mair + e * Mvap) / rugas / (temp + 273.15)

  # Net radiation [J/m2/day], both terms using full 24 h
  swdown_J <- swdown * 86400
  lwnet_J  <- (lwdown - sigma * (temp + 273.15)^4) * 86400
  Rn       <- swdown_J * (1 - 0.23) + lwnet_J

  # Aerodynamic term [J/m2/day], full day
  aero <- rho * cp * (esat - e) / raH * 86400

  # FAO-56 Penman-Monteith: rs = 70 s/m (getpet.c line 120)
  rs  <- 70
  pet <- (s * Rn + aero) / (s + gamma_t * (1 + rs / raH)) / lambda

  # Clamp at 0 while keeping the input shape: pmax() drops dim, which breaks the
  # cell-vectorised (matrix) callers. pmax is NA-safe; restore dim afterwards.
  d   <- dim(pet)
  pet <- pmax(0, pet)
  if (!is.null(d)) dim(pet) <- d
  return(pet)

}
