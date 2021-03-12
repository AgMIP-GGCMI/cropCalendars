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
pldir <- paste0(project.dir, "crop_calendars/plots/")
ggdir <- "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar/"

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
# ------------------------------------------------------#

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
  
  # Get GGCMI phase 3 sowing and harvest dates (to replace default dates) ----
  # ------------------------------------------------------#
  fn <- paste0(ggdir, crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
               "_ggcmi_crop_calendar_phase3_v1.01.nc4")
  nf <- nc_open(fn)
  sdggcmi <- ncvar_get(nf, "planting_day", c(1,1), c(720,360))[,360:1]
  hdggcmi <- ncvar_get(nf, "maturity_day", c(1,1), c(720,360))[,360:1]
  nc_close(nf)
  
  
  # Initialize arrays for each variable
  # ------------------------------------------------------#
  ARsd <- ARhd <- ARst <- ARhr <- ARss <- AR # ARgp
  ARdd <- AR # default sdate

  # Loop through time-slice-specific DT and fill in arrays
  # ------------------------------------------------------#
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
        ARdd[ilon, ilat, j] <- ifelse(DT$sowing_month[i]==0, 0, 1) # mask of default month
        
      } # j
    } # i
    cat("\n")
    
  } # tt
  
  # Loop through pixels and filter + interpolate time series
  # ------------------------------------------------------#
  
  # Initialize arrays for each variable
  # --------------------------#
  ARsd.r <- ARhd.r <- ARsd.m <- ARhd.m <- AR # s/hdate replacing and marking
  ARst.r <- ARhr.r <- AR                     # seasonality/harv.reas. replacing
  ARsd.a <- ARhd.a <- AR                     # rolling average
  
  count <- 0                                 # for plotting only some pixels
  
  for (i in 1:nrow(DT)) {
    
    if(i%%1e4==0) cat(i, "\t")
    
    ilat <- which(lats==DT$lat[i])
    ilon <- which(lons==DT$lon[i])
    
    # Filter sdate and hdates: ----
    # ------------------------------------------------------#
    
    # Check for temporary changes in Seasonality Type, replace and mark
    ARsd.r[ilon, ilat,] <- replace.jumps(ARst[ilon, ilat,], ARsd[ilon, ilat,])
    ARst.r[ilon, ilat,] <- replace.jumps(ARst[ilon, ilat,], ARst[ilon, ilat,],
                                         replacing.value = "previous")
    ARsd.m[ilon, ilat,] <- replace.jumps(ARst[ilon, ilat,], ARsd[ilon, ilat,],
                                         marking = T, mark.value = 1L)
    
    # Check for temporary changes in Harvest Reason, replace and mark
    ARhd.r[ilon, ilat,] <- replace.jumps(ARhr[ilon, ilat,], ARhd[ilon, ilat,])
    ARhr.r[ilon, ilat,] <- replace.jumps(ARhr[ilon, ilat,], ARhr[ilon, ilat,],
                                         replacing.value = "next")
    ARhd.m[ilon, ilat,] <- replace.jumps(ARhr[ilon, ilat,], ARhd[ilon, ilat,],
                                         marking = T, mark.value = 1L)
    
    # --------------------------#
    
    ARsd.r2 <- ARsd.r; ARhd.r2 <- ARhd.r; ARsd.m2 <- ARsd.m; ARhd.m2 <- ARhd.m
    
    # Replace default dates with GGCMI phase3 dates
    ARsd.r2[ilon, ilat, which(ARdd[ilon, ilat,]==0)] <- sdggcmi[ilon, ilat]
    ARhd.r2[ilon, ilat, which(ARdd[ilon, ilat,]==0)] <- hdggcmi[ilon, ilat]
    
    # --------------------------#
    
    ARsd.r3 <- ARsd.r2; ARhd.r3 <- ARhd.r2; ARsd.m3 <- ARsd.m2; ARhd.m3 <- ARhd.m2
    
    # Check for temporary Default Sdate, replace sdate and mark
    ARsd.r3[ilon, ilat,] <- replace.jumps(ARdd[ilon, ilat,], ARsd.r2[ilon, ilat,])
    ARsd.m3[ilon, ilat,] <- replace.jumps(ARdd[ilon, ilat,], ARsd.r2[ilon, ilat,],
                                         marking = T, mark.value = 2L)
    
    # Check for temporary Default Sdate, replace hdate and mark
    ARhd.r3[ilon, ilat,] <- replace.jumps(ARdd[ilon, ilat,], ARhd.r2[ilon, ilat,])
    ARhd.m3[ilon, ilat,] <- replace.jumps(ARdd[ilon, ilat,], ARhd.r2[ilon, ilat,],
                                         marking = T, mark.value = 2L)
    
    # --------------------------#
    
    ARsd.m4 <- ARsd.m3; ARhd.m4 <- ARhd.m3
    
    # Add Seasonality type and Default date vector to identify all jumps to be
    #  considered as windows for rolling mean
    ARsd.m4[ilon, ilat,] <- ARst.r[ilon, ilat,] + ARdd[ilon, ilat,]
    ARhd.m4[ilon, ilat,] <- ARhr.r[ilon, ilat,] + ARdd[ilon, ilat,]
    
    
    # Interpolated sdate: ----
    # ------------------------------------------------------#
    ARsd.a.m <- ARhd.a.m <- AR
    
    # Compute annual sdate by 10-years rolling average (on "replaced" s/hdates)
    ARsd.a[ilon, ilat,] <- rollmean.in.steps(ARsd.m4[ilon, ilat,],
                                             ARsd.r3[ilon, ilat,], kk = 10)
    ARhd.a[ilon, ilat,] <- rollmean.in.steps(ARsd.m4[ilon, ilat,],
                                             ARhd.r3[ilon, ilat,], kk = 10)
    ARsd.a.m[ilon, ilat,] <- rollmean.in.steps(ARsd.m4[ilon, ilat,],
                                             ARsd.r3[ilon, ilat,], kk = 10, marking = T)
    ARhd.a.m[ilon, ilat,] <- rollmean.in.steps(ARsd.m4[ilon, ilat,],
                                             ARhd.r3[ilon, ilat,], kk = 10, marking = T)
    
    
    # Plot time series for testing ----
    # ------------------------------------------------------#
    if ( makeplot==T && count%%1000==0 &&
         ( any(diff(ARsd.a.m[ilon,ilat,])!=0) |
          any(ARsd.m[ilon, ilat,]==1L) | any(ARhd.m[ilon, ilat,]==1L) |
          any(ARsd.m3[ilon, ilat,]==2L) | any(ARhd.m3[ilon, ilat,]==2L) )) {
      
      count <- count + 1
      
      pfile <- paste(lons[ilon], lats[ilat], crop.ls[["ggcmi"]][cr],
                     irri.ls[["ggcmi"]][ir], GCM, sep="_")
      pdf(paste0(pldir, pfile, ".pdf"), width = 7, height = 4)
      
      layout(matrix(1:4, nrow = 2, byrow = T), heights = c(.55, .45))
      par(cex.lab=0.7, cex.axis=0.7, cex.main=1)
      par(mar = c(2,4,2,1)) #c(bottom, left, top, right)
      
      # sdate
      plot(years, ARsd[ilon, ilat,], type = "l", ylim = c(1,365), xlab = "", ylab = "sdate")
      lines(years, ARsd.r3[ilon, ilat,], type = "l", col = "blue")
      lines(years, ARsd.a[ilon, ilat,], type = "l", col = "red4", lty=2, lwd=2)
      legend("topleft", lty=1, cex = .6, seg.len=.5, horiz=TRUE,
             legend = c("sdate.original", "sdate.replaced", "sdate.averaged"),
             col = c("black", "blue", "red4"))
      
      # hdate
      plot(years, ARhd[ilon, ilat,],  type = "l", ylim = c(1,365), xlab = "", ylab = "hdate")
      lines(years, ARhd.r3[ilon, ilat,], type = "l", col = "blue")
      lines(years, ARhd.a[ilon, ilat,], type = "l", col = "red4", lty=2, lwd=2)
      legend("topleft", lty=1, cex = .6, seg.len=.5, horiz=TRUE,
             legend = c("hdate.original", "hdate.replaced", "hdate.averaged"),
             col = c("black", "blue", "red4"))
      
      # sdate classes
      plot(years, ARst[ilon, ilat,], type = "l", ylim = c(0,8), ylab = "sdate factors")
      lines(years, ARdd[ilon, ilat,], type = "l", col = "orange")
      lines(years, ARsd.m4[ilon, ilat,], type = "l", col = "deeppink4")
      legend("topleft", lty=1, cex = .6, seg.len=.5, horiz=TRUE,
             legend = c("seasonalty.original", "default.sdate", "seasonality.replaced"),
             col = c("black", "orange", "deeppink4"))
      
      # hdate classes
      plot(years, ARhr[ilon, ilat,], type = "l", ylim = c(0,8), ylab = "hdate factors")
      lines(years, ARdd[ilon, ilat,], type = "l", col = "orange")
      lines(years, ARhd.m4[ilon, ilat,], type = "l", col = "deeppink4")
      legend("topleft", lty=1, cex = .6, seg.len=.5, horiz=TRUE,
             legend = c("hreason.original", "default.sdate", "hreason.replaced"),
             col = c("black", "orange", "deeppink4"))
      
      dev.off()
      
    } # plot
    
    
  } #i
  
  cat("\nNr. of pixels for which sdate or hdate have been replaced: ",count,"\n")
  
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
  # sdrep_def  <- ncvar_def(name="plant-day-replaced", units="boolean", dim=nc_dimension,
  #                         longname = "Mark of replaced sowing dates to avoid jumps",
  #                         prec="single", compression = 6)
  # hdrep_def  <- ncvar_def(name="maty-day-replaced", units="boolean", dim=nc_dimension,
  #                         longname = "Mark of replaced harvest dates to avoid jumps",
  #                         prec="single", compression = 6)
  sroll_def <- ncvar_def(name="plant-day-rollmean-window", units="sequential nr.", dim=nc_dimension,
                          longname = "Mark of roll-averaged sowing dates after filtering for jumps",
                          prec="single", compression = 6)
  hroll_def <- ncvar_def(name="maty-day-rollmean-window", units="sequential nr.", dim=nc_dimension,
                          longname = "Mark of roll-averaged maturity dates after filtering for jumps",
                          prec="single", compression = 6)
  
  # Create netCDF file and put arrays
  ncout <- nc_create(ncfname, list(sdate_def, hdate_def, #growp_def,
                                   seast_def, harvr_def, sowse_def,
                                   sroll_def, hroll_def),verbose = F)
  
  # Put variables
  ncvar_put(ncout, sdate_def, ARsd.a) # sdate interpolated by rolling mean
  ncvar_put(ncout, hdate_def, ARhd.a) # hdate with replaced values (not interpolated)
  # ncvar_put(ncout, growp_def, ARgp)
  ncvar_put(ncout, seast_def, ARst)
  ncvar_put(ncout, harvr_def, ARhr)
  ncvar_put(ncout, sowse_def, ARss)
  ncvar_put(ncout, sroll_def, ARsd.a.m)
  ncvar_put(ncout, hroll_def, ARhd.a.m)
  
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
