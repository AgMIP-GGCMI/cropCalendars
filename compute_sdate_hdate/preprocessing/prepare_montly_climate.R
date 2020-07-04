#-----------------------------------------------------------#
# Climate-driven sowing and harvest dates                   #
# R-Code                                                    #
# Written by Sara Minoli                                    #
# Based on Minoli et al. (2019) Global and Planetary Change #
#-----------------------------------------------------------#

#-----------------------------------------------------------#
# PREPARE MONTHLY CLIMATE #
#-----------------------------------------------------------#
# prepare climate data for sowing and harvest dates estimation
# RUN TIME: 90 min (20 min for monthly stats only, which are done first)

print(Sys.time())

rm(list=ls(all=TRUE))

oncluster <- TRUE

if(oncluster==TRUE) {
  working.dir <- "/home/minoli/crop_calendars_gitlab/crop_phen_paper/compute_sdate_hdate/"
} else {
  working.dir <- "D:/PROJECTS/GROWING_PERIODS_PACKAGE/"
}


# READ CONFIGURATION ----
#___________________________________________________________#
source(paste0(working.dir, "configuration/configuration.R"))

ismip2b.path  <- "/p/projects/ikiimp/ISIMIP2b/input_CLM2/"

# FUNCTIONS ----
#___________________________________________________________#
# units
doy2month <- function(doy, year = 2015) {strptime(paste(year, doy), format="%Y %j")$mon+1}
k2deg <- function(t_kelvin) {t_kelvin-273.15} #temperature units from K to °C
deg2k <- function(t_deg) {t_deg+273.15} #temperature units from K to °C
ha2kmq <- function(ha) {ha/100}
kmq2ha <- function(kmq) {kmq*100}
deg2rad <- function(deg) {((deg)*M_PI*.00555555555555555555)}

# paste path and file name of isimip climate data
paste.ismip2b.clm.fn <- function(path, gcm, scen, var, syear, eyear) {
  paste0(path,
         paste(gcm, scen,
               paste(var, gcm, scen,
                     paste(syear, eyear, sep = "-"), sep = "_"), sep = "/"), ".clm")
}

# read in lpjml climate input file
# fyear=first year file, syear=first year to reand in, eyear=last year to read in, npix=nr of pixels, nands=time step, hd=header size
# NOTE: need to multiply the by the scalar e.g. tas1 <- read.LPJmL.clim.input(tas_fn1, 1861, SY, 2005, 67420, 43, 365)*tas_scalar 
read.LPJmL.clim.input <- function(filename,fyear,syear,eyear,npix,hd=43,nbands=12){
  nyears <- (eyear-syear+1)
  ff <- file(filename,"rb")
  dd <- array(NA,dim=c(npix,nyears*nbands)) #, dimnames = list("pixel" = c(1:npix), "date" = rep(vyear.mon.day, nyears)))
  seek(ff,where=(syear-fyear)*nbands*npix*2+hd,origin="start")
  for(y in syear:eyear){
    dd[,c(1:nbands)+(y-syear)*nbands] <- t(matrix(readBin(ff,integer(),size=2,n=npix*nbands),nrow=nbands,byrow=F))
  }
  close(ff)
  dd
}

# lpjml
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


# LON LAT
lon <- coord.df[,1]
lat <- coord.df[,2]

# import arguments from the job script
options(echo=FALSE) # if want see commands in output file
args <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
print(args)
# args <- 1

# select variable, crop, model
dbatch1 <- subset(dbatch, subset = (step1!=0)) # remove scenarios that are not needed in step1 pre-lpjml
rownames(dbatch1) <- NULL
dbatch2 <- dbatch1[args,]
GCM <- dbatch2$gcm
SCgp <- dbatch2$scenario.growp
SYgp <- dbatch2$syear.growp
EYgp <- dbatch2$eyear.growp
SCclm <- dbatch2$scenario.clm
SYclm <- dbatch2$syear.clm
EYclm <- dbatch2$eyear.clm

tas_fn1 <- paste.ismip2b.clm.fn(ismip2b.path, GCM, "historical", "tas", 1861, 2005)
tas_fn2 <- paste.ismip2b.clm.fn(ismip2b.path, GCM, SCgp, "tas", 2006, 2099)
pr_fn1  <- paste.ismip2b.clm.fn(ismip2b.path, GCM, "historical", "pr", 1861, 2005)
pr_fn2  <- paste.ismip2b.clm.fn(ismip2b.path, GCM, SCgp, "pr", 2006, 2099)

#SYgp <- 1991
#EYgp <- 2010
years <- c(SYgp:EYgp)
nyears <- (EYgp-SYgp+1)


# PARAMETERS
NDAYYEAR <- 365
M_1_PI <- 0.318309886183790671538
M_PI <- 3.14159265358979323846

# vector of full date time line
# get dates from a non-leap year
vdate <- seq(from = strptime("1987-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "CET"),
             to = strptime("1987-12-31 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "CET"),
             by = "day")
length(vdate)
# vectors of time line in weather file 1
vyear <- sprintf("%04d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$year+1900 ))
vmon <- sprintf("%02d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$mon+1 ))
vday <- sprintf("%02d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$mday ))
vdoy <- sprintf("%02d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$yday+1 ))
vyear.mon.day <- paste0(vyear, vmon, vday)

# scalars for multiplying bin files
tas_scalar <- 0.1
pr_scalar <- 0.1

# if SYgp berfore 2005 need to read in two files and cbind them
if (SYgp <= 2005 & EYgp > 2005) {
  # extract mean daily TEMPERATURE from clm files (bin)
  tas1 <- read.LPJmL.clim.input(tas_fn1, 1861, SYgp, 2005, 67420, 43, 365)*tas_scalar
  tas2 <- read.LPJmL.clim.input(tas_fn2, 2006, 2006, EYgp, 67420, 43, 365)*tas_scalar
  tas <- cbind(tas1,tas2)
  dimnames(tas) <- list("pixel" = paste(lon, lat, sep="_"),
                        "date" = paste(rep(years, each = 365), rep(vmon, nyears), rep(vday, nyears), rep(vdoy, nyears), sep ="_"))
  rm(tas1, tas2)
  
  # extract mean daily PRECIPITATION from clm files (bin)
  pr1 <- read.LPJmL.clim.input(pr_fn1, 1861, SYgp, 2005, 67420, 43, 365)*pr_scalar
  pr2 <- read.LPJmL.clim.input(pr_fn2, 2006, 2006, EYgp, 67420, 43, 365)*pr_scalar
  pr <- cbind(pr1,pr2)
  dimnames(pr) <- list("pixel" = paste(lon, lat, sep="_"),
                       "date" = paste(rep(years, each = 365), rep(vmon, nyears), rep(vday, nyears), rep(vdoy, nyears), sep ="_"))
  rm(pr1, pr2)
  
} else if (SYgp <= 2005 & EYgp <= 2005) {
  # extract mean daily TEMPERATURE from clm files (bin)
  tas <- read.LPJmL.clim.input(tas_fn1, 1861, SYgp, EYgp, 67420, 43, 365)*tas_scalar
  dimnames(tas) <- list("pixel" = paste(lon, lat, sep="_"),
                        "date" = paste(rep(years, each = 365), rep(vmon, nyears), rep(vday, nyears), rep(vdoy, nyears), sep ="_"))
  
  # extract mean daily PRECIPITATION from clm files (bin)
  pr <- read.LPJmL.clim.input(pr_fn1, 1861, SYgp, EYgp, 67420, 43, 365)*pr_scalar
  dimnames(pr) <- list("pixel" = paste(lon, lat, sep="_"),
                       "date" = paste(rep(years, each = 365), rep(vmon, nyears), rep(vday, nyears), rep(vdoy, nyears), sep ="_"))

} else {
  
  # extract mean daily TEMPERATURE from clm files (bin)
  tas <- read.LPJmL.clim.input(tas_fn2, 2006, SYgp, EYgp, 67420, 43, 365)*tas_scalar
  dimnames(tas) <- list("pixel" = paste(lon, lat, sep="_"),
                        "date" = paste(rep(years, each = 365), rep(vmon, nyears), rep(vday, nyears), rep(vdoy, nyears), sep ="_"))
  
  # extract mean daily PRECIPITATION from clm files (bin)
  pr <- read.LPJmL.clim.input(pr_fn2, 2006, SYgp, EYgp, 67420, 43, 365)*pr_scalar
  dimnames(pr) <- list("pixel" = paste(lon, lat, sep="_"),
                       "date" = paste(rep(years, each = 365), rep(vmon, nyears), rep(vday, nyears), rep(vdoy, nyears), sep ="_"))
} # if SYgp <= 2005


# MONTHLY CLIMATE STATISTICS ----
# chuncks to subset DT and loop through cuncks instead of single rows
nchunks <- 100
schunks <- round(seq(1, 67420, length.out = nchunks))[-nchunks]
echunks <- c((schunks-1)[-1], 67420)
data.frame(schunks,echunks, echunks-schunks)


DTout <- NULL
for (i in c(1:(nchunks-1))) {
  
  # convert array to data.table
  DTtas <- setDT(melt(tas[schunks[i]:echunks[i], ], value.name = "tas"))
  DTpr <- setDT(melt(pr[schunks[i]:echunks[i], ], value.name = "pr"))
  DT <- merge(DTtas, DTpr)
  rm(DTtas, DTpr)
  
  # split pixel column into lon, lat columns
  DT[,c("lon", "lat") := tstrsplit(pixel, "_", type.convert = TRUE, fixed = TRUE)]
  # split date column
  DT[, c("year", "month", "day", "doy") := tstrsplit(date, "_", type.convert = TRUE, fixed = TRUE)]
  # delete pixel and date columns
  DT[, c("pixel", "date") := NULL]
  # sort DT columns
  DT <- DT[, c("lon", "lat", "year", "month", "day", "doy", "tas", "pr")]
  
  # compute PET and add column
  DT[, c("pet") := round(petpar(temp = tas, lat = lat, day = doy)[[3]], digits = 5)]
  
  # compute P/PET ratio and add column
  DT[, c("ppet") := round(pr/pet, digits = 5)]
  
  # compute monthly statistics for each year
  mDT <- DT[,.(tas = round(mean(tas), digits = 5),
               pr = round(sum(pr), digits = 5),
               pet = round(sum(pet), digits = 5),
               ppet = round(mean(ppet), digits = 5)),
            by = .(lon, lat, year, month)]
  #mDT[,c("mppet") := round(mpr/mpet, digits = 5)]
  
  # compute average monthly statistics across years
  amDT <- mDT[,.(tas = round(mean(tas), digits = 5),
                 pr = round(mean(pr), digits = 5),
                 pet = round(mean(pet), digits = 5),
                 ppet = round(mean(ppet), digits = 5)),
              by = .(lon, lat, month)]
  
  DTout <- rbind(DTout, amDT)
  gc()
}

save(DTout, file = paste0(climate.dir, "DT_average_monthly_climate_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata"))

# ggplot.map.continuous(DTout, "tas") + facet_wrap(~month)
# dev.off()

# DAILY CLIMATE STATISTICS ----
if(F) {
# chuncks to subset DT and loop through cuncks instead of single rows
nchunks <- 100
schunks <- round(seq(1, 67420, length.out = nchunks))[-nchunks]
echunks <- c((schunks-1)[-1], 67420)
data.frame(schunks,echunks, echunks-schunks)


DTout <- NULL
for (i in c(1:(nchunks-1))) {
  
  # convert array to data.table
  DTtas <- setDT(melt(tas[schunks[i]:echunks[i], ], value.name = "tas"))
  DTpr <- setDT(melt(pr[schunks[i]:echunks[i], ], value.name = "pr"))
  DT <- merge(DTtas, DTpr)
  rm(DTtas, DTpr)
  
  # split pixel column into lon, lat columns
  DT[,c("lon", "lat") := tstrsplit(pixel, "_", type.convert = TRUE, fixed = TRUE)]
  # split date column
  DT[, c("year", "month", "day", "doy") := tstrsplit(date, "_", type.convert = TRUE, fixed = TRUE)]
  # delete pixel and date columns
  DT[, c("pixel", "date") := NULL]
  # sort DT columns
  DT <- DT[, c("lon", "lat", "year", "month", "day", "doy", "tas", "pr")]
  
  # compute PET and add column
  DT[, c("pet") := round(petpar(temp = tas, lat = lat, day = doy)[[3]], digits = 5)]
  
  # compute P/PET ratio and add column
  DT[, c("ppet") := round(pr/pet, digits = 5)]
  
  # compute daily statistics for each year
  mDT <- DT[,.(tas = round(mean(tas), digits = 5),
               pr = round(sum(pr), digits = 5),
               pet = round(sum(pet), digits = 5),
               ppet = round(mean(ppet), digits = 5)),
            by = .(lon, lat, year, doy)]
  #mDT[,c("mppet") := round(mpr/mpet, digits = 5)]
  
  # compute daily monthly statistics across years
  amDT <- mDT[,.(tas = round(mean(tas), digits = 5),
                 pr = round(mean(pr), digits = 5),
                 pet = round(mean(pet), digits = 5),
                 ppet = round(mean(ppet), digits = 5)),
              by = .(lon, lat, doy)]
  
  DTout <- rbind(DTout, amDT)
  gc()
}

save(DTout, file = paste0(data.dir, "DT_average_daily_climate_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata"))

# ggplot.map.continuous(DTout, "tas") + facet_wrap(~month)
# dev.off()
} # DAILY STATS


#-------------------------------------------------------------



# args <- 1
# for (args in c(1:5)) {
# source("/p/projects/macmit/users/minoli/PROJECTS/CROP_PHENOLOGY_v01/SCRIPTS/crop_phen_0_master_script.R")
# # select variable, crop, model
# dbatch <- dbatch[args,]
# GCM <- dbatch$gcm
# SC <- dbatch$scenario
# SYgp <- dbatch$syear.series
# EYgp <- dbatch$eyear.series


# PLOT ----
DTclm <- get(load(file = paste0(climate.dir, "DT_average_monthly_climate_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata")))

# plot maps  ----
pdf(paste0(figure.dir, "map_climate_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".pdf"), width = a4w, height = a4h)
p_tas  <- ggplot.map.continuous(DTclm, "tas",  limits = c(-10,30), colors = rev(brewer.pal(11,"Spectral"))) + facet_wrap(~month, nrow = 6)
p_pr   <- ggplot.map.continuous(DTclm, "pr",   limits = c(0, 200), colors = brewer.pal(11,"Spectral")) + facet_wrap(~month, nrow = 6)
p_pet  <- ggplot.map.continuous(DTclm, "pet",  limits = c(0, 100), colors = brewer.pal(11,"Spectral")) + facet_wrap(~month, nrow = 6)
p_ppet <- ggplot.map.continuous(DTclm, "ppet", limits = c(0, 3), colors = brewer.pal(11,"Spectral")) + facet_wrap(~month, nrow = 6)
print(p_tas + labs(subtitle = paste(c("min", "max"), range(DTclm$tas), "(degC)", collapse = " ")) )
print(p_pr + labs(subtitle = paste(c("min", "max"), range(DTclm$pr), "(mm)", collapse = " ")) )
print(p_pet + labs(subtitle = paste(c("min", "max"), range(DTclm$pet), "(mm)", collapse = " ")) )
print(p_ppet + labs(subtitle = paste(c("min", "max"), range(DTclm$ppet), "(-)", collapse = " ")) )
dev.off()
# }
#-------------------------------------------------------------
if(F) {
  # convert DT to arrays
  DTout <- get(load(file = paste0(data.dir, "DT_average_monthly_climate_", SCgp, "_", SYgp, "_", EYgp, ".Rdata")))
  
  DT.tas <- DTout[, c("lon", "lat", "month", "tas")]
  DT.ppet <- DTout[, c("lon", "lat", "month", "ppet")]
  
  lons <- seq(-179.75, 179.75, 0.5)
  lats <- seq(-89.75, 89.75, 0.5)
  AR <- array(NA, dim = c(720, 360, 12), dimnames = list(lon = lons, lat = rev(lats), month = c(1:12)))
  
  # convert full array to datatable
  DTfull <- setDT(melt(AR, value.name = "V1"))
  DTfull[, c("V1") := NULL]
  
  # merge small and full datatables
  DTfull.tas <- merge(DTfull, DT.tas, all.x = TRUE, by = c("lon", "lat", "month"))
  DTfull.ppet <- merge(DTfull, DT.tas, all.x = TRUE, by = c("lon", "lat", "month"))
  
  # order by month, lat, lon
  setorderv(DTfull.tas, cols = c("month", "lat", "lon"), order = c(1, -1, 1))
  DTfull.tas
  # coerce tas vector to array, array is filled with the leftmost subscript moving fastest:
  # x (top-down), y (left-right), z (front-back).
  # Array of lon, lat, time, should be ordered in the reverse order, so that first value is the top-right value in the array.
  ARm <- array(DTfull.tas$tas, dim = c(720, 360, 12), list(lon = lons, lat = rev(lats), month = c(1:12)))
  image(ARm[as.character(lons), as.character(lats), 1])
  dev.off()
  
  # order by month, lat, lon
  setorderv(DTfull.pr, cols = c("month", "lat", "lon"), order = c(1, -1, 1))
  DTfull.pr
  # coerce tas vector to array, array is filled with the leftmost subscript moving fastest:
  # x (top-down), y (left-right), z (front-back).
  # Array of lon, lat, time, should be ordered in the reverse order, so that first value is the top-right value in the array.
  ARm <- array(DTfull.pr$pr, dim = c(720, 360, 12), list(lon = lons, lat = rev(lats), month = c(1:12)))
  image(ARm[as.character(lons), as.character(lats), 1])
  dev.off()
}
