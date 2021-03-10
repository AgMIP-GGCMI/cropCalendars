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
stime <- Sys.time()

# ----

working.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate/"

makeplot <- TRUE

# PACKAGES ----
library(ncdf4)
library(data.table)
library(ggplot2)
library(zoo)       # for rolling mean

# FUNCTIONS ----
source(paste0(working.dir, "src/units.R"))
source(paste0(working.dir, "src/ggplot.map.general.R"))
source(paste0(working.dir, "src/replace.jumps.R"))
source(paste0(working.dir, "configuration/graphics.R"))
source(paste0(working.dir, "configuration/configuration_ggcmi_ph3.R"))
source(paste0(working.dir, "postprocessing/functions_lpjml_input_output.R"))
source(paste0(working.dir, "postprocessing/ncdfs.R"))

# PATHS ----
dtdir <- paste0(project.dir, "crop_calendars/DT/")
ncdir <- paste0(project.dir, "crop_calendars/ncdf/")

if (!dir.exists(ncdir)) dir.create(ncdir, recursive = T)

# Crop Names: ----
# rb_cal = rule-based calendar, ggcmi = ggcmi ph3
crop.ls <- list(all_low = c("maize", "rice", "sorghum", "soybean", "spring_wheat", "winter_wheat"),
                rb_cal  = c("Maize", "Rice", "Sorghum", "Soybean", "Spring_Wheat", "Winter_Wheat"),
                ggcmi   = c("mai", "ric", "sor", "soy", "swh", "wwh"))
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

SYs <- seq(1981, 2091, by = 10) # Start of the period
EYs <- seq(1990, 2100, by = 10) # End of the period
FYs <- seq(1961, 2071, by = 10) # First year DT file
LYs <- seq(1990, 2100, by = 10) # Last year DT file

nhist <- length(EYs[EYs<2015]) # number of historical time slices
HYs <- c(rep("historical", nhist), rep(SC, length(SYs)-nhist)) # historical/ssp years

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
  ARsd <- ARhd <- ARst <- ARhr <- ARss <- AR # ARgp
  ARsd.r <- ARhd.r <- ARsd.m <- ARhd.m <- AR # s/hdate replacing and marking
  ARsd.a <- ARhd.a <- AR # rolling average
  
  for (tt in 1:length(SYs)) { # time-slice index
    
    iyears <- which(years==SYs[tt]):which(years==EYs[tt])
    cat("Doing", years[iyears], "\n")
    
    # Crop calendar DT.Rdata file
    fname <- paste0(output.dir, "DT_output_crop_calendars_", CROP, "_", GCM,
                    "_", HYs[tt], "_", FYs[tt], "_", LYs[tt], ".Rdata")
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
        # ARgp[ilon, ilat, j] <- DT$growing_period[i]
        ARst[ilon, ilat, j] <- DT$seasonality_type[i]
        ARhr[ilon, ilat, j] <- DT$harvest_reason[i]
        ARss[ilon, ilat, j] <- DT$sowing_season[i]
        
      } # j
      
      # Filter sdate and hdates:
      # --------------------------# 
      # Check for temporary changes in seasonality type, replace and mark
      ARsd.r[ilon, ilat,] <- replace.jumps(ARst[ilon, ilat,], ARsd[ilon, ilat,])
      ARsd.m[ilon, ilat,] <- replace.jumps(ARst[ilon, ilat,], ARsd[ilon, ilat,], marking = T)
      
      # Check for temporary changes in harvest reason, replace and mark
      ARhd.r[ilon, ilat,] <- replace.jumps(ARhr[ilon, ilat,], ARhd[ilon, ilat,])
      ARhd.m[ilon, ilat,] <- replace.jumps(ARhr[ilon, ilat,], ARhd[ilon, ilat,], marking = T)
      
      # Interpolated sdate:
      # --------------------------# 
      # Compute annual sdate by 10-years rolling average (on "replaced" sdate)
      kk <- 10 # years for rollmean (k parameter) 
      t1 <- 1; t2 <- 4; t3 <- length(years)-4; t4 <- length(years) # t2=(kk/2)-1
      # concatenate first and last values, as rolling average trims edges
      ARsd.tmp <- c(ARsd.r[ilon, ilat,t1:t2],
                    ARsd.r[ilon, ilat,],
                    ARsd.r[ilon, ilat,t3:t4])
      ARsd.a[ilon, ilat,] <- rollmean(ARsd.tmp, k = 10)
      # concatenate first and last values, as rolling average trims edges
      ARhd.tmp <- c(ARhd.r[ilon, ilat,t1:t2],
                    ARhd.r[ilon, ilat,],
                    ARhd.r[ilon, ilat,t3:t4])
      ARhd.a[ilon, ilat,] <- rollmean(ARhd.tmp, k = 10)
      
    } # i
    cat("\n")
    
  } # tt
  
  # Write NCDF file: ----
  # ------------------------------------------------------#
  
  ncfname <- paste0(ncdir,
                    crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
                    "_", GCM, "_", min(SYs), "-", max(EYs),
                    "_ggcmi_ph3_rule_based_crop_calendar.nc4")
  
  # Define dimensions
  londim <- ncdim_def("lon", "degrees_east",  lons)
  latdim <- ncdim_def("lat", "degrees_north", lats)
  timdim <- ncdim_def("year", "year", years)
  nc_dimension <- list(londim, latdim, timdim)
  
  # Define variables
  sdate_def  <- ncvar_def(name="plant-day", units="day of year", dim=nc_dimension,
                          longname = "Rule-based sowing date (filtered & interpolated)",
                          prec="single", compression = 6)
  hdate_def  <- ncvar_def(name="maty-day", units="day of year", dim=nc_dimension,
                          longname = "Rule-based harvest date (filtered & interpolated)",
                          prec="single", compression = 6)
  # growp_def <- ncvar_def(name="grow-period", units="days", dim=nc_dimension,
  #                        longname = "Rule-based growing period duration",
  #                        prec="single", compression = 6)
  seast_def <- ncvar_def(name="seasonality", units="-", dim=nc_dimension,
                         longname = paste("Climate seasonality type,
                                          (1=No Seas; 2=Prec; 3=PrecTemp;",
                                          "4=Temp; 5=TempPrec)"),
                         prec="single", compression = 6)
  harvr_def <- ncvar_def(name="harv-reason", units="-", dim=nc_dimension,
                         longname = paste("Rule triggering harvest",
                                          "(1=GPmin; 2=GPmed; 3=GPmax; 4=Wstress;",
                                          "5=Topt, 6=Thigh)"),
                         prec="single", compression = 6)
  sowse_def <- ncvar_def(name="plant-season", units="days", dim=nc_dimension,
                         longname = "Sowing season (Spring / Winter)",
                         prec="single", compression = 6)
  sdrep_def  <- ncvar_def(name="plant-day-replaced", units="boolean", dim=nc_dimension,
                          longname = "Mark of replaced sowing dates to avoid jumps",
                          prec="single", compression = 6)
  hdrep_def  <- ncvar_def(name="maty-day-replaced", units="boolean", dim=nc_dimension,
                          longname = "Mark of replaced harvest dates to avoid jumps",
                          prec="single", compression = 6)
  
  # Create netCDF file and put arrays
  ncout <- nc_create(ncfname, list(sdate_def, hdate_def, #growp_def,
                                   seast_def, harvr_def, sowse_def,
                                   sdrep_def, hdrep_def),verbose = F)
  
  # Put variables
  ncvar_put(ncout, sdate_def, ARsd.a) # sdate interpolated by rolling mean
  ncvar_put(ncout, hdate_def, ARhd.a) # hdate with replaced values (not interpolated)
  # ncvar_put(ncout, growp_def, ARgp)
  ncvar_put(ncout, seast_def, ARst)
  ncvar_put(ncout, harvr_def, ARhr)
  ncvar_put(ncout, sowse_def, ARss)
  ncvar_put(ncout, sdrep_def, ARsd.m)
  ncvar_put(ncout, hdrep_def, ARhd.m)
  
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

etime <- Sys.time()
cat("Run time:\n--------\n")
print(etime-stime)

cat("\n------\nDone !\n------\n")
