# Units 
doy2month <- function(doy, year = 2015) {strptime(paste(year, doy), format="%Y %j")$mon+1}
k2deg <- function(t_kelvin) {t_kelvin-273.15} #temperature units from K to °C
deg2k <- function(t_deg) {t_deg+273.15} #temperature units from K to °C
ha2kmq <- function(ha) {ha/100}
kmq2ha <- function(kmq) {kmq*100}
deg2rad <- function(deg) {((deg)*M_PI*.00555555555555555555)}


# Area of pixel (km2)
computePixelArea <- function(lat) { (111e3*0.5)*(111e3*0.5)*cos(lat/180*pi)/10000 }

# Yield
g.m2TOkg.ha <- function(g_m2) {kg_ha <- g_m2*10}
g.m2TOt.ha <- function(g_m2) {t_ha <- g_m2*10/1e03}

carbon2biomass <- function(carbon) {biomass <- carbon/0.45}  #0.45 conversion factor from DM biomass to carbon
