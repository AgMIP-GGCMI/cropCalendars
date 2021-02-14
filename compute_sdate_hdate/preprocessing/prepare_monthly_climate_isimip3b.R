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

working.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate/"


# READ CONFIGURATION ----
#___________________________________________________________#
source(paste0(working.dir, "configuration/configuration.R"))

ismip3b.path  <- "/p/projects/lpjml/input/scenarios/ISIMIP3b/"

# FUNCTIONS ----
#___________________________________________________________#
NDAYYEAR <- 365
M_1_PI <- 0.318309886183790671538
M_PI   <- 3.14159265358979323846

source(paste0(working.dir, "src/units.R"))
source(paste0(working.dir, "src/petpar.R"))
source(paste0(working.dir, "src/read.climate.input.R"))
source(paste0(working.dir, "src/array2dataTable.R"))

# paste path and file name of isimip climate data
paste.ismip3b.clm.fn <- function(path, gcm, scen, var, syear, eyear) {
  paste0(path,
         paste(gcm, scen,
               paste(var, gcm, scen,
                     paste(syear, eyear, sep = "-"), sep = "_"), sep = "/"), ".clm")
}



# import arguments from the job script ----
options(echo=FALSE) # if want see commands in output file
args <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
print(args)
# args <- 1

# select variable, crop, model
batch.df1 <- batch.df[args,]
GCM  <- batch.df1$gcm
SC   <- batch.df1$scenario
SY   <- batch.df1$syear
EY   <- batch.df1$eyear
FY   <- batch.df1$fyear
LY   <- batch.df1$lyear

years  <- c(SY:EY)
nyears <- length(years)

# Grid ----
NCELLS <- nrow(coord.df)
lon <- coord.df[,1]
lat <- coord.df[,2]

# vector of full date time line ----
# get dates from a non-leap year
vdate <- seq(from = strptime("1987-01-01 00:00:00",
                             format = "%Y-%m-%d %H:%M:%S", tz = "CET"),
             to = strptime("1987-12-31 00:00:00",
                           format = "%Y-%m-%d %H:%M:%S", tz = "CET"),
             by = "day")
length(vdate)
# vectors of time line in weather file 1
vyear <- sprintf("%04d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$year+1900 ))
vmon  <- sprintf("%02d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$mon+1 ))
vday  <- sprintf("%02d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$mday ))
vdoy  <- sprintf("%02d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$yday+1 ))
vyear.mon.day <- paste0(vyear, vmon, vday)


# Climate file names ----
tas_fn <- paste.ismip3b.clm.fn(isimip3b.path, GCM, scenario, "tas", FY, LY)
pr_fn  <- paste.ismip3b.clm.fn(isimip3b.path, GCM, scenario, "pr",  FY, LY)

# Read climate ----
tas <- read.climate.input(tas_fn, ncells = NCELLS, ryear = FY,
                          fyear = SY, lyear = EY, header = 43,
                          nbands = 365, dtype = integer, scalar = 0.1)
pr  <- read.climate.input(pr_fn,  ncells = NCELLS, ryear = FY,
                          fyear = SY, lyear = EY, header = 43,
                          nbands = 365, dtype = integer, scalar = 0.1)

# Compute Potential ET (PET)
pet <- array(NA, dim = dim(tas))
for (la in 1:length(lat)) {
  for (dd in 1:365) {
    pet[la,dd,] <- petpar(temp = tas[la,dd,], lat = lat[la], day = dd)[[3]]
  }
}

# dimnames(tas) <- dimnames(pr) <- dimnames(pet) <- list(
#   "pixel" = paste(lon, lat, sep="_"),
#   "date" = paste(vmon, vday, vdoy, sep ="_"),
#   "year" = years)

# MONTHLY CLIMATE STATISTICS ----
# ------------------------------------------------------#
tas.mavg.y <- array(NA, dim = c(NCELLS, 12, nyears))
for (yy in 1:nyears) {
  for (mm in 1:12) {
    
    dom <- which(vmon==sprintf("%02d", mm)) # day of the month
    tas.mavg.y[,mm,yy] <- apply(tas[,dom,yy], 1)
    # pr
    # pet
    # ppet
  }
}

tas.mavg <- round(apply(tas.mavg.y, c(1,2), mean), digits = 5)
dimnames(tas.mavg) <- c("pixelnr" = c(1:NCELLS), "month" = c(1:12))

DTtas <- array2dataTable(tas.mavg)



save(DTout, file = paste0(climate.dir, "DT_average_monthly_climate_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata"))



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

if (F) {
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
}
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
