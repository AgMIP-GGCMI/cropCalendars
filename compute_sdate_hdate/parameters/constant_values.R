#-----------------------------------------------------------#
# Climate-driven sowing and harvest dates                   #
# R-Code                                                    #
# Written by Sara Minoli                                    #
# Based on Minoli et al. (2019) Global and Planetary Change #
#-----------------------------------------------------------#

#-----------------------------------------------------------#
# CONSTANT VALUES ####
#-----------------------------------------------------------#

NDAYYEAR <- 365
DOYS <- c(1:365)
month_name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
NDAYMONTH <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
NMONTH <- 12

# for daily map legend
cdoys <- cumsum(NDAYMONTH)
doys_endmonth <- DOYS
idx <- DOYS%in%cdoys
doys_endmonth[which(idx==FALSE)] <- NA

M_PI <- 3.14159265358979323846
M_1_PI <- 0.318309886183790671538

#LPJmL parameters called from calc_seasonality.c
TEMPMIN <- 10  #/*minimum temperature of coldest month (deg C) */
#LPJmL parameters called from date.c
midday <- c(15,43,74,104,135,165,196,227,257,288,318,349,380) #midday[NMONTH+1]
diffday <- c(1/28.0, 1/31.0, 1/30.0, 1/31.0, 1/30.0, 1/31.0, 1/31.0, 1/30.0, 1/31.0, 1/30.0, 1/31.0, 1/31.0)

DEFAULT_MONTH <- 0 #NEW: 0 to indicate that default month is used, then the month is assigned depending on the latitude
MINLGP <- 4
daysum <- cumsum(NDAYMONTH[1:NMONTH]) #NEW: monthly cumulative sum of doy

#coords
lons <- seq(-179.75, 179.75, by=0.5)
lats <- seq(-89.75, 89.75, by=0.5)
