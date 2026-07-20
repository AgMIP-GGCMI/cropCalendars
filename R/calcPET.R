#' @title Calculate Potential Evapo-Transpiration (PET)
#'
#' @description Calculate equilibrium (Priestley-Taylor) potential ET as in
#' LPJmL numeric/petpar.c.  When \code{swdown} and \code{lwdown} are supplied,
#' net radiation is computed from observed radiation fluxes:
#'   Rns = (1 - 0.23) * swdown * 86400  [J/m2/day]  (albedo matches FAO-56)
#'   Rnl = (lwdown - sigma * (T+273.15)^4) * 86400  [J/m2/day]
#' Otherwise net radiation is estimated from orbital geometry with a fixed
#' sunshine fraction (legacy behaviour, backward-compatible).
#'
#' @param temp  mean daily temperature (degree Celsius)
#' @param lat   latitude (decimal degrees)
#' @param day   day of the year (DOY)
#' @param swdown  surface downwelling shortwave radiation (W/m2, 24 h mean).
#'   If NULL the legacy orbital-geometry estimate is used.
#' @param lwdown  surface downwelling longwave radiation (W/m2, 24 h mean).
#'   Required when \code{swdown} is provided.
#'
#' @return potential evapotranspiration (mm/day)
#' @export

calcPET <- function(temp,
                    lat = NULL,
                    day = NULL,
                    swdown = NULL,
                    lwdown = NULL
                    ) {

  gamma_t <- 65.05 + temp * 0.064
  lambda  <- 2.495e6 - temp * 2380
  s       <- 2.503e6 * exp(17.269 * temp / (237.3 + temp)) / (237.3 + temp)^2

  if (!is.null(swdown) && !is.null(lwdown)) {

    sigma <- 5.67e-8
    Rns   <- (1 - 0.23) * swdown * 86400
    Rnl   <- (lwdown - sigma * (temp + 273.15)^4) * 86400
    Rn    <- Rns + Rnl
    # pmax() drops the matrix dim, which breaks the cell-vectorised callers
    # (.petDaily -> addYearClimate indexes pet[, dom]); restore it, as
    # calcPET_FAO56 does. pmax is NA-safe.
    eeq   <- pmax(0, s / (s + gamma_t) / lambda * Rn)
    if (!is.null(dim(Rn))) dim(eeq) <- dim(Rn)

  } else {

    # Orbital-geometry net radiation: needs latitude and day-of-year.
    if (is.null(lat) || is.null(day)) {
      stop("calcPET without swdown/lwdown needs 'lat' and 'day' (orbital Rn estimate).")
    }

    ndays_year <- 365
    M_1_PI     <- 0.318309886183790671538
    beta       <- 0.17
    a          <- 107.0
    b          <- 0.2
    qoo        <- 1360.0
    c          <- 0.25
    d          <- 0.5
    k          <- 13750.98708
    sun        <- 0.01

    delta <- deg2rad(-23.4 * cos(2 * pi * (day + 10.0) / ndays_year))
    u <- sin(deg2rad(lat)) * sin(delta)
    v <- cos(deg2rad(lat)) * cos(delta)
    w <- (c + d * sun) * (1 - beta) * qoo *
      (1.0 + 2.0 * 0.01675 * cos(2.0 * pi * day / ndays_year))

    if (u >= v) {
      daylength <- 24
      par <- w / (1 - beta) * u * pi * k
    } else if (u <= -v) {
      daylength <- par <- 0
    } else {
      hh <- acos(-u / v)
      par <- w / (1 - beta) * (u * hh + v * sin(hh)) * k
      daylength <- 24 * hh * M_1_PI
    }

    u <- w * u - (b + (1 - b) * sun) * (a - temp)
    v <- w

    if (u <= -v) {
      eeq <- 0
    } else {
      if (u >= v) {
        eeq <- 2 * (s / (s + gamma_t) / lambda) * u * pi * k
      } else {
        hh  <- acos(-u / v)
        eeq <- 2 * (s / (s + gamma_t) / lambda) * (u * hh + v * sin(hh)) * k
      }
    }

  }

  return(eeq)

}
