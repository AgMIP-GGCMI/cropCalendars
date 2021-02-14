petpar <- function(temp, lat, day) { #(kg H2O m-2 d-1 = mm d-1)
  
  ALPHAM <- 1.391 #Priestley-Taylor coefficient /par/param.par
  
  beta <- 0.17
  a <- 107.0
  b <- 0.2
  qoo <- 1360.0  #/* solar constant (1360 W/m2) */
  c <- 0.25
  d <- 0.5
  k <- 13750.98708  #/* conversion factor from solar angular units to seconds (12/pi*3600) */
  sun <- 0.01
  
  gamma_t <- 65.05+temp*0.064 #psychrometer constant (Pa K-1)
  lambda  <- 2.495e6-temp*2380 #latent heat of vaporization of water (J kg-1)
  
  delta <- deg2rad(-23.4*cos(2*M_PI*(day+10.0)/NDAYYEAR))
  u <- sin(deg2rad(lat))*sin(delta)
  v <- cos(deg2rad(lat))*cos(delta)
  w <- (c+d*sun)*(1-beta)*qoo*(1.0+2.0*0.01675*cos(2.0*M_PI*day/NDAYYEAR))
  
  daylength <- ifelse(u>=v, 24,
                      ifelse(u<=-v, 0,
                             {hh <- acos(-u/v); 24*hh*M_1_PI}))
  
  par <- ifelse(u>=v, w*u*M_PI*k, # (MJ d-1)
                ifelse(u<=-v, 0,
                       {hh <- acos(-u/v); w*(u*hh+v*sin(hh))*k}))
  
  u <- w*u-(b+(1-b)*sun)*(a-temp)
  v <- w
  
  pet <- ifelse(u<=-v, 0,
                ifelse (u>=v, {s <- 2.503e6*exp(17.269*temp/(237.3+temp))/((237.3+temp)*(237.3+temp)); #rate of increase of saturated vapor pressure with temperature (Pa K-1)
                2*(s/(s+gamma_t)/lambda)*u*M_PI*k},
                {s <- 2.503e6*exp(17.269*temp/(237.3+temp))/((237.3+temp)*(237.3+temp));
                hh <- acos(-u/v); 2*(s/(s+gamma_t)/lambda)*(u*hh+v*sin(hh))*k}))
  
  return(list(daylength, par, ALPHAM*pet))
  
} #of petpar