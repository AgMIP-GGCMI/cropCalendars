# ------------------------------------------------------# ----
# Project: crop_calendar
#
# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
#
# Date Created: 2021-04-08
#
# Description: 
# ------------------------------------------------------#

rm(list=ls(all=T))

library(ncdf4)
library(data.table)
library(ggplot2)
library(zoo)       # for rolling mean

# ----

# Import Job Arguments ----
# ------------------------------------------------------#
options(echo=FALSE) # if want see commands in output file
argr <- commandArgs(trailingOnly = TRUE)
print(argr)
# argr <- c("UKESM1-0-LL", "ssp585", "1991", "2014")

GCM    <- argr[1]
SC     <- argr[2]
CROP   <- argr[3]
IRRI   <- argr[4]

# ------------------------------------------------------#
# Paths ----
working.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/"
project.dir <- "/p/projects/macmit/users/minoli/PROJECTS/GGCMI_ph3_adaptation/"
ncdir       <- paste0(project.dir, "crop_calendars/ncdf/", GCM, "/", SC, "/")

# Functions ----
source(paste0(working.dir, "import.functions.R"))
import.functions(paste0(working.dir, "compute_sdate_hdate/src/"))
import.functions(paste0(working.dir, "compute_phu/src/"))


if (SC == "historical") {
  # Sowing and cultivar to change every 10 years
  SYs <-   seq(1991, 2011, by = 10) # Start of the period
  EYs <- c(seq(2000, 2014, by = 10), 2014) # End of the period
  HYs <- rep(SC, length(SYs))
} else {
  # Sowing and cultivar to change every 10 years
  SYs <- c(2014, seq(2021, 2091, by = 10)) # Start of the period
  EYs <- c(2020, seq(2030, 2100, by = 10)) # End of the period
  nhist <- length(LYs[LYs<2015]) # number of historical time slices
  HYs <- c(rep("historical", nhist), rep(SC, length(SYs)-nhist)) # historical/ssp years
}



# ------------------------------------------------------#
# Get Grid ----
grid.fn <- paste0("/p/projects/lpjml/input/ISIMIP3/grid.bin")
NCELLS <- 67420

grid <- as.data.frame(read.lpjml.grid(
  grid.fn, band_names = c("lon", "lat"), ncells = 67420, header = 43,
  nbands = 2, dtype =  integer(), dsize =  2, scalar = .01))


# ------------------------------------------------------#
# Get Crop Calendar: Sowing and Harvest Dates ----
crop.ls <- list(all_low = c("maize", "rice", "sorghum", "soybean",
                            "spring_wheat", "winter_wheat"),
                rb_cal  = c("Maize", "Rice", "Sorghum", "Soybean",
                            "Spring_Wheat", "Winter_Wheat"),
                ggcmi   = c("mai", "ric", "sor", "soy", "swh", "wwh"),
                vernal  = c("no",  "no",  "no",  "no",  "no",  "yes"))
irri.ls <- list(all_low = c("rainfed", "irrigated"),
                rb_cal  = c("Rainfed", "Irrigated"),
                ggcmi   = c("rf", "ir"))

cr <- which(crop.ls[["ggcmi"]]==CROP)
ir <- which(irri.ls[["ggcmi"]]==IRRI)

ncfname <- paste0(ncdir,
                  crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
                  "_", GCM, "_", SC, "_", min(SYs), "-", max(EYs),
                  "_ggcmi_ph3_rule_based_crop_calendar.nc4")

nc <- nc_open(ncfname)
sdate <- ncvar_get(nc, varid = "plant-day", start = c(1,1,1), count = c(360, 720, nyears))
hdate <- ncvar_get(nc, varid = "maty-day",  start = c(1,1,1), count = c(360, 720, nyears))
lons  <- ncvar_get(nc, varid = "lon")
lats  <- ncvar_get(nc, varid = "lat")
nc_close(nc)

# ------------------------------------------------------#
# Get Crop Parameters ----
croppar.fn <- paste0(working.dir,"compute_phu/parameters/crop_pars_phase3.csv")
crop_parameters_all <- read.csv(croppar.fn, header = T, stringsAsFactors = F)

basetemp <- as.numeric(croppars[rownames(croppars)=="basetemp",names(croppars)==CROP])
max.vern.days <- as.numeric(croppars[rownames(croppars)=="max.vern.days",names(croppars)==CROP])
tv1 <- as.numeric(croppars[rownames(croppars)=="vern.temp.min",names(croppars)==CROP])
tv2 <- as.numeric(croppars[rownames(croppars)=="vern.temp.opt.min",names(croppars)==CROP])
tv3 <- as.numeric(croppars[rownames(croppars)=="vern.temp.opt.max",names(croppars)==CROP])
tv4 <- as.numeric(croppars[rownames(croppars)=="vern.temp.max",names(croppars)==CROP])

# ------------------------------------------------------#

years  <- c(min(SYs):max(EYs))
nyears <- length(years)

phu.annual <- array(NA, c(360, 720))
phu.cube    <- array(NA, c(360, 720, nyears))

# Loop through years ----
for (yy in 1:length(SYs)) {
  
  # ------------------------------------------------------#
  # Get Climate Data ----
  tas <- get.isimip.tas(GCM, SC, SYs[yy], EYy[yy])
  tas_mean_day <- apply(tas, c(1,2), mean)
  
  # ------------------------------------------------------#
  # Loop through grid cells ----
  for (i in 1:NCELLS) {
    
    ilo <- which(lats==grid$lon[i])
    ila <- which(lons==grid$lat[i])
    
    sdate.avg <- mean(sdate[ila, ilo, ])
    hdate.avg <- mean(hdate[ila, ilo, ])
    
    dtemp <- tas_mean_day[i,]
    
    # ------------------------------------------------------#
    # Compute monthly temperature ----
    sday <- c( 1, 32, 60,  91, 121, 152, 182, 213, 244, 274, 305, 335)
    eday <- c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
    mtemp <- rep(NA, 12)
    
    for (m in 1:12) {
      mtemp[m] <- mean(dtemp[sday[m]:eday[m]])
    }
    
    # ------------------------------------------------------#
    # If Vernal-crop:
    
    # Calculate Vernalization Requirements ----
    vd <- calc.vd(temp_mean_month  = mtemp,
                  max.vern.days    = max.vern.days,
                  max.vern.months  = 5,
                  tv2              = tv2,
                  tv3              = tv3)
    
    # Calculate Vernalization Reduction Factors ----
    vrf <- calc.vrf(sdate = sdate.avg,
                    hdate = hdate.avg,
                    mdt   = mtemp,
                    vd    = vd,
                    vd_b  = 0.2,
                    max.vern.days   = max.vern.days,
                    max.vern.months = 5,
                    tv1   = tv1,
                    tv2   = tv2,
                    tv3   = tv3,
                    tv4   = tv4)
    
    # ------------------------------------------------------#
    # Calculate Phenological Heat Unit Requirements ----
    
    phu <- calc.phu(sdate       = sdate.avg,
                    hdate       = hdate.avg,
                    mdt         = mtemp,
                    vern_factor = vrf,
                    basetemp    = basetemp,
                    phen_model  = "tv")
    
    phu.annual[ila, ilo] <- phu
    
  } # i
  
  phu.cube[,,which(SYs[yy]):which(EYy[yy])] <- phu.annual
  
} # yy


# ------------------------------------------------------#
# Write Crop-specific Output File ----

