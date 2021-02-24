# ------------------------------------------------------# ----
# Project: GGCMI phase3 - Adaptation
#
# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
#
# Date Created: 2021-02-24
#
# Description:
# Create NCDF files with sdate and hdate (multi-year and crop-specific)
# For GGCMI-phase3 adaptation runs
# ------------------------------------------------------#

rm(list=ls(all=T))

# ----

working.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate/"

makeplot <- TRUE

# PACKAGES ----
library(ncdf4)
library(data.table)
library(ggplot2)

# FUNCTIONS ----
source(paste0(working.dir, "src/units.R"))
source(paste0(working.dir, "src/ggplot.map.general.R"))
source(paste0(working.dir, "configuration/graphics.R"))
source(paste0(working.dir, "configuration/configuration_ggcmi_ph3.R"))
source(paste0(working.dir, "postprocessing/functions_lpjml_input_output.R"))
source(paste0(working.dir, "postprocessing/ncdfs.R"))

# PATHS ----
dtdir <- paste0(project.dir, "crop_calendars/DT/")
ncdir <- paste0(project.dir, "crop_calendars/ncdf/")

# Crop Names: ----
# rb_cal = rule-based calendar, ggcmi = ggcmi ph3
crop.ls <- list(all_low = c("maize", "rice", "sorghum", "spring_wheat", "winter_wheat"),
                rb_cal  = c("Maize", "Rice", "Sorghum", "Spring_Wheat", "Winter_Wheat"),
                ggcmi   = c("mai", "ric", "sor", "swh", "wwh"))
irri.ls <- list(all_low = c("rainfed", "irrigated"),
                rb_cal  = c("Rainfed", "Irrigated"),
                ggcmi   = c("rf", "ir"))

# SELECT SCENARIO ----
#___________________________________________________________#

# import argument from bash script
options(echo=FALSE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
# args <- c("Maize", "UKESM1-0-LL", "ssp585")
print(args)

CROP <- args[1]
GCM  <- args[2]
SC   <- args[3]

SYs <- c(2005, 2015, seq(2021, 2091, by = 10))    # Start of the period
EYs <- c(2014, 2020, seq(2030, 2100, by = 10))    # End of the period
# MYs <- SYs+(EYs-SYs+1)/2                          # Mid of the period
# LYs <- length(SYs:EYs)                            # Length of the time period
HYs <- c("historical", rep(SC, length(SYs)-1))    # historical/ssp years

print(SYs); print(EYs)

# Initialize array for ncdf ----
# ------------------------------------------------------#

lons  <- seq(-179.75, 179.75, by = 0.5)
lats  <- seq(-89.75,  89.75,  by = 0.5)
years <- c(min(SYs):max(EYs))

AR <- array(NA, dim = c(length(lons), length(lats), length(years)),
            dimnames = list(lon = lons, lat = lats, year = years))
str(AR)

# Read DT crop calendar and fill-in array ----
# ------------------------------------------------------#

cr <- which(crop.ls[["rb_cal"]]==CROP) # crop index
cat("\n------\n", CROP, "\n------\n")

#tt <- 1; ir <- 1
for (ir in 1:2) { # irrigation index
  
  cat("Doing", irri.ls[["rb_cal"]][ir], "\n----------------\n")
  
  # Initialize arrays for each variable
  ARsd <- ARhd <- ARgp <- AR
  
  for (tt in 1:length(SYs)) { # time-slice index
    
    iyears <- which(years==SYs[tt]):which(years==EYs[tt])
    cat("Doing", years[iyears], "\n")
    
    # Crop calendar DT.Rdata file
    fname <- paste0(output.dir, "DT_output_crop_calendars_", CROP, "_", GCM,
                    "_", HYs[tt], "_", SYs[tt], "_", EYs[tt], ".Rdata")
    DT <- get(load(fname))[irrigation==irri.ls[["rb_cal"]][ir]] # subset irrig
    print(dim(DT))
    
    # Loop through pixels and extract values from crop calendar DT
    for (i in 1:nrow(DT)){ # = pixel index
      
      if(i%%1e4==0) cat(i, "\t")
      
      ilat <- which(lats==DT$lat[i])
      ilon <- which(lons==DT$lon[i])
      
      # Repeat average dates each year in the time-slice tt
      for (j in iyears) { # iyears index of years in time slice tt
        
        ARsd[ilon, ilat, j] <- DT$sowing_doy[i]
        ARhd[ilon, ilat, j] <- DT$maturity_doy[i]
        ARgp[ilon, ilat, j] <- DT$growing_period[i]
        
      } # j
      
    } # i
    cat("\n")
    
  } # tt
  
  # Write NCDF file
  ncfname <- paste0(ncdir,
                    crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
                    "_", GCM, "_", min(SYs), "-", max(EYs),
                    "_ggcmi_ph3_rule_based_crop_calendar.nc4")
  
  # Define dimansions
  londim <- ncdim_def("lon", "degrees_east",  lons)
  latdim <- ncdim_def("lat", "degrees_north", lats)
  timdim <- ncdim_def("year", "year", years)
  nc_dimension <- list(londim, latdim, timdim)
  
  # Define variables
  sdate_def  <- ncvar_def(name="plant-day", units="DOY", dim=nc_dimension,
                          longname = "Rule-based sowing date",
                          prec="single", compression = 6)
  hdate_def  <- ncvar_def(name="maty-day", units="DOY", dim=nc_dimension,
                          longname = "Rule-based harvest date",
                          prec="single", compression = 6)
  growp_def <- ncvar_def(name="grow-period", units="days", dim=nc_dimension,
                         longname = "Rule-based growing period duration",
                         prec="single", compression = 6)
  # Create netCDF file and put arrays
  ncout <- nc_create(ncfname, list(sdate_def, hdate_def, growp_def), verbose = F)
  
  # Put variables
  ncvar_put(ncout, sdate_def, ARsd)
  ncvar_put(ncout, hdate_def, ARhd)
  ncvar_put(ncout, growp_def, ARgp)
  
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
  
} # ir


cat("\n------\nDone !\n------\n")
