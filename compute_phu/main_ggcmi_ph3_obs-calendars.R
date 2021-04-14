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

starttime <- Sys.time() # Track run-time
print(starttime)


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
# argr <- c("UKESM1-0-LL", "historical", "wwh", "rf")

GCM    <- argr[1]
SC     <- argr[2]
CROP   <- argr[3]
IRRI   <- argr[4]
SY     <- 1984
EY     <- 2014

# ------------------------------------------------------#
# Paths ----
working.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/"
project.dir <- "/p/projects/macmit/users/minoli/PROJECTS/GGCMI_ph3_adaptation/"
ncdir.in    <- "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar/"
ncdir.out   <- paste0(project.dir, "crop_calendars/ncdf/", GCM, "/", SC, "/")

isimip3b.path  <- "/p/projects/lpjml/input/scenarios/ISIMIP3b/" # .clm climate

# Functions ----
source(paste0(working.dir, "import.functions.R"))
import.functions(paste0(working.dir, "compute_sdate_hdate/src/"))
import.functions(paste0(working.dir, "compute_phu/src/"))


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

ncfname <- paste0(ncdir.in,
                  crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
                  "_ggcmi_crop_calendar_phase3_v1.01.nc4")


# ------------------------------------------------------#
# Get Crop Parameters ----
croppar.fn <- paste0(working.dir,"compute_phu/parameters/crop_pars_ggcmi_ph3.csv")
croppar <- subset(read.csv(croppar.fn, header = T, stringsAsFactors = F), crop==CROP)
basetemp      <- croppar$basetemp
max.vern.days <- croppar$max.vern.days
tv1           <- croppar$vern.temp.min
tv2           <- croppar$vern.temp.opt.min
tv3           <- croppar$vern.temp.opt.max
tv4           <- croppar$vern.temp.max


# ------------------------------------------------------#

phu.cube    <- array(NA, c(720, 360))


# ------------------------------------------------------#
# Get Climate Data ----
tas <- get.isimip.tas(GCM, SC, SY, EY)
tas_mean_day <- apply(tas, c(1,2), mean)

# ------------------------------------------------------#
# Get Sowing and Harvest dates
nc <- nc_open(ncfname)
sdate <- ncvar_get(nc, varid = "planting_day", start = c(1,1),
                   count = c(720, 360))
hdate <- ncvar_get(nc, varid = "maturity_day",  start = c(1,1),
                   count = c(720, 360))
lons  <- ncvar_get(nc, varid = "lon")
lats  <- ncvar_get(nc, varid = "lat")
nc_close(nc)

# ------------------------------------------------------#
# Loop through grid cells ----
for (i in 1:NCELLS) {
  
  ilo <- which(lons==grid$lon[i])
  ila <- which(lats==grid$lat[i])
  
  sdate.avg <- as.integer(mean(sdate[ilo, ila]))
  hdate.avg <- as.integer(mean(hdate[ilo, ila]))
  
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
  if (crop.ls[["vernal"]][cr] == "yes") {
    
    # Calculate Vernalization Requirements ----
    vd <- calc.vd(temp_mean_month  = mtemp,
                  max.vern.days    = max.vern.days,
                  max.vern.months  = 5,
                  tv2              = tv2,
                  tv3              = tv3)
    
    # Calculate Vernalization Reduction Factors ----
    vrf <- calc.vrf(sdate = sdate.avg,
                    hdate = hdate.avg,
                    mdt   = dtemp,
                    vd    = vd,
                    vd_b  = 0.2,
                    max.vern.days   = max.vern.days,
                    max.vern.months = 5,
                    tv1   = tv1,
                    tv2   = tv2,
                    tv3   = tv3,
                    tv4   = tv4)[["vrf"]]
  } else {
    
    vrf <- rep(1, 365)
    
  }
  
  
  # ------------------------------------------------------#
  # Calculate Phenological Heat Unit Requirements ----
  
  phu <- calc.phu(sdate       = sdate.avg,
                  hdate       = hdate.avg,
                  mdt         = dtemp,
                  vern_factor = vrf,
                  basetemp    = basetemp,
                  phen_model  = "tv")
  
  phu.cube[ilo, ila] <- as.integer(phu)
  
} # i

save(phu.cube, file = paste0(working.dir, "compute_phu/tmp/",
                             crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
                             "_", GCM, "_", SC, "_", SY, "-", EY,
                             "_ggcmi_ph3_obs-calendars_phu.Rdata"))



# ------------------------------------------------------#
# Write Crop-specific Output File ----
# Write NCDF file
# ------------------------------------------------------#

ncfname <- paste0(ncdir.out,
                  crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
                  "_", GCM, "_", SC, "_", SY, "-", EY,
                  "_ggcmi_ph3_obs-calendars_phu.nc4")

# Define dimensions
londim <- ncdim_def("lon", "degrees_east",  lons)
latdim <- ncdim_def("lat", "degrees_north", lats)
nc_dimension <- list(londim, latdim)

# Define variables
phu_def  <- ncvar_def(name="phu", units="degree days", dim=nc_dimension,
                      longname = "Phenological Heat Unit Requirements",
                      prec="single", compression = 6)

# Create netCDF file and put arrays
ncout <- nc_create(ncfname, list(phu_def), verbose = F)

# Put variables
ncvar_put(ncout, phu_def, phu.cube)

# Put additional attributes into dimension and data variables
ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"lat","axis","Y")

ncatt_put(ncout, 0, "Crop",
          paste0(crop.ls[["ggcmi"]][cr], "_", irri.ls[[ir]]["ggcmi"]))
ncatt_put(ncout, 0, "Institution",
          "Potsdam Institute for Climate Impact Research (PIK), Germany")
history <- paste("Sara Minoli", date(), sep=", ")
ncatt_put(ncout, 0, "history", history)

# Close the file, writing data to disk
nc_close(ncout)


# ------------------------------------------------------#

cat("\n", paste("Computation PHUs ended!"),
    "-------------------------------------------------------", sep = "\n")

endtime <- Sys.time()
print(endtime)

print(endtime-starttime)


