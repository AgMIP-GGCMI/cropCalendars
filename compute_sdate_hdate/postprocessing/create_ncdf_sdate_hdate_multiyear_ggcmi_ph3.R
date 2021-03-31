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
source(paste0(working.dir, "src/rollmean.in.steps.R"))
source(paste0(working.dir, "configuration/graphics.R"))
source(paste0(working.dir, "configuration/configuration_ggcmi_ph3.R"))
source(paste0(working.dir, "postprocessing/functions_lpjml_input_output.R"))
source(paste0(working.dir, "postprocessing/ncdfs.R"))

# SELECT SCENARIO ----
# ------------------------------------------------------#

# import argument from bash script
options(echo=FALSE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
# args <- c("Maize", "Rainfed", "UKESM1-0-LL", "historical")
print(args)

CROP <- args[1]
IRRI <- args[2]
GCM  <- args[3]
SC   <- args[4]

if (SC == "historical") {
  # Sowing and cultivar to change every 10 years
  SYs <-   seq(1991, 2011, by = 10) # Start of the period
  EYs <- c(seq(2000, 2014, by = 10), 2014) # End of the period
  # Computing sowing and harvest dates based on preceding 30-years climate
  FYs <-   seq(1961, 1981, by = 10) # First year DT file (output of main.R)
  LYs <-   seq(1990, 2010, by = 10) # Last  year DT file (output of main.R)
  
  HYs <- rep(SC, length(SYs))
} else {
  # Sowing and cultivar to change every 10 years
  SYs <- c(2014, seq(2021, 2091, by = 10)) # Start of the period
  EYs <- c(2020, seq(2030, 2100, by = 10)) # End of the period
  # Computing sowing and harvest dates based on preceding 30-years climate
  FYs <- seq(1981, 2061, by = 10) # First year DT file (output of main.R)
  LYs <- seq(2010, 2090, by = 10) # Last  year DT file (output of main.R)
  
  nhist <- length(LYs[LYs<2015]) # number of historical time slices
  HYs <- c(rep("historical", nhist), rep(SC, length(SYs)-nhist)) # historical/ssp years
}

print(data.frame(SYs, EYs, FYs, LYs, HYs))


# PATHS ----
dtdir <- paste0(project.dir, "crop_calendars/DT/")
ncdir <- paste0(project.dir, "crop_calendars/ncdf/", GCM, "/", SC, "/")
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
ir <- which(irri.ls[["rb_cal"]]==IRRI) # irrigation index
cat("\n------------------\n", CROP, "\t", IRRI, "\n------------------\n")

#tt <- 1

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
  fname <- paste0(paste0(output.dir, HYs[tt], "/"),
                  "DT_output_crop_calendars_", CROP, "_", GCM,
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
      ARdd[ilon, ilat, j] <- ifelse(DT$sowing_month[i]==0, 0, 1) # mask of default sdate
      
    } # j
  } # i
  cat("\n")
  
} # tt

print(Sys.time()-stime)

# Loop through pixels and filter + interpolate time series
# ------------------------------------------------------#

# Initialize arrays for each variable
# --------------------------#
ARsd.r <- ARhd.r <- ARsd.m <- ARhd.m <- AR # s/hdate replacing and marking
ARst.r <- ARhr.r <- AR                     # seasonality/harv.reas. replacing
ARsd.a <- ARhd.a <- AR                     # rolling average
ARsd.a.m <- ARhd.a.m <- AR


# Keep track of replaced values
# --------------------------#
count <- 0                                 # for plotting only some pixels
count.dt <- data.frame(lon            = numeric(),
                       lat            = numeric(),
                       sdate.smoothed = logical(), # jumps removed
                       hdate.smoothed = logical(), # jumps removed
                       ddate.smoothed = logical(), # jumps removed
                       ddate.replaced = logical()) # default date replaced with ggcmi

# --------------------------#
for (i in 1:nrow(DT)) {
  #for (i in 1:1000) { 
  if(i%%1e3==0) cat(i, "\t") #else cat(".")
  
  ilat <- which(lats==DT$lat[i])
  ilon <- which(lons==DT$lon[i])
  
  sdate <- ARsd[ilon, ilat,]   # sowing date
  seast <- ARst[ilon, ilat,]   # seasonality type
  hdate <- ARhd[ilon, ilat,]   # harvest date
  hreas <- ARhr[ilon, ilat,]   # harvest reason
  ddate <- ARdd[ilon, ilat,]   # default date == 0
  sggcm <- sdggcmi[ilon, ilat] # sowing date ggcmi
  hggcm <- hdggcmi[ilon, ilat] # harvest date ggcmi
  
  # Filter sdate and hdates: ----
  # ------------------------------------------------------#
  
  # Check for temporary changes in Seasonality Type, replace and mark
  sdate.r <- replace.jumps(seast, sdate)
  seast.r <- replace.jumps(seast, seast, replacing.value = "previous")
  sdate.m <- replace.jumps(seast, sdate, marking = T, mark.value = 1L)
  
  # Check for temporary changes in Harvest Reason, replace and mark
  hdate.r <- replace.jumps(hreas, hdate)
  hreas.r <- replace.jumps(hreas, hreas, replacing.value = "previous")
  hdate.m <- replace.jumps(hreas, hdate, marking = T, mark.value = 1L)
  
  # --------------------------#
  
  sdate.r2 <- sdate.r; hdate.r2 <- hdate.r;
  sdate.m2 <- sdate.m; hdate.m2 <- hdate.m
  
  # Replace default dates with GGCMI phase3 dates
  sdate.r2[which(ddate==0)] <- sggcm
  hdate.r2[which(ddate==0)] <- hggcm
  
  # --------------------------#
  
  # Check for temporary Default Sdate, replace sdate and mark
  sdate.r3 <- replace.jumps(ddate, sdate.r2)
  sdate.m3 <- replace.jumps(ddate, sdate.r2, marking = T, mark.value = 2L)
  ddate.r  <- replace.jumps(ddate, ddate, replacing.value = "previous")
  
  # Check for temporary Default Sdate, replace hdate and mark
  hdate.r3 <- replace.jumps(ddate, hdate.r2)
  hdate.m3 <- replace.jumps(ddate, hdate.r2, marking = T, mark.value = 2L)
  
  # --------------------------#
  
  # Add Seasonality type and Default date vector to identify all jumps to be
  #  considered as windows for rolling mean
  sdate.m4 <- seast.r + ddate
  hdate.m4 <- hreas.r + ddate
  
  
  # Interpolated sdate: ----
  # ------------------------------------------------------#
  
  # Compute annual sdate by 10-years rolling average (on "replaced" s/hdates)
  sdate.a   <- rollmean.in.steps(sdate.m4, sdate.r3, kk = 30)
  hdate.a   <- rollmean.in.steps(hdate.m4, hdate.r3, kk = 30)
  sdate.a.m <- rollmean.in.steps(sdate.m4, sdate.r3, kk = 30, marking = T)
  hdate.a.m <- rollmean.in.steps(hdate.m4, hdate.r3, kk = 30, marking = T)
  
  # Fill in new arrays with filtered values
  ARst.r[ilon, ilat,]   <- seast.r
  ARhr.r[ilon, ilat,]   <- hreas.r
  ARsd.a[ilon, ilat,]   <- sdate.a
  ARhd.a[ilon, ilat,]   <- hdate.a
  ARsd.a.m[ilon, ilat,] <- sdate.a.m
  ARhd.a.m[ilon, ilat,] <- hdate.a.m
  
  count.dt <- rbind(count.dt,
                    data.frame(pixelnr = i,
                               lon            = DT$lon[i],
                               lat            = DT$lat[i],
                               sdate.smoothed = any(sdate.m==1L), # jumps removed
                               hdate.smoothed = any(hdate.m==1L), # jumps removed
                               ddate.smoothed = any(sdate.m3==2L), # jumps removed
                               ddate.replaced = any(ddate.r==0)) # default date replaced with ggcmi
  )
  
  # Plot time series for testing ----
  # ------------------------------------------------------#
  if ( any(count.dt[i,]) ) {
    
    count <- count + 1
    
    if ( makeplot==T && count%%1000==0) {
      
      # File name
      pfile <- paste(lons[ilon], lats[ilat], crop.ls[["ggcmi"]][cr],
                     irri.ls[["ggcmi"]][ir], GCM, sep="_")
      pdf(paste0(pldir, pfile, ".pdf"), width = 7, height = 4)
      
      # Split panels
      layout(matrix(1:4, nrow = 2, byrow = T), heights = c(.55, .45))
      par(cex.lab=0.7, cex.axis=0.7, cex.main=1)
      par(mar = c(2,4,2,1)) #c(bottom, left, top, right)
      
      # sdate
      plot(years,  sdate, type = "l", ylim = c(1,365), xlab = "", ylab = "sdate")
      text(x = 2040, y = 300,
           labels = paste0(crop.ls[["all_low"]][cr], " ", irri.ls[["all_low"]][ir], "; ",
                           "lon/lat: ", lons[ilon], "/", lats[ilat]), cex = .7)
      lines(years, sdate.r3, type = "l", col = "blue")
      lines(years, sdate.a, type = "l", col = "red4", lty=2)#, lwd=2)
      legend("topleft", lty=1, cex = .6, seg.len=.5, horiz=TRUE,
             legend = c("sdate.original", "sdate.replaced", "sdate.averaged"),
             col = c("black", "blue", "red4"))
      
      # hdate
      plot(years,  hdate,  type = "l", ylim = c(1,365), xlab = "", ylab = "hdate")
      lines(years, hdate.r3, type = "l", col = "blue")
      lines(years, hdate.a, type = "l", col = "red4", lty=2)#, lwd=2)
      legend("topleft", lty=1, cex = .6, seg.len=.5, horiz=TRUE,
             legend = c("hdate.original", "hdate.replaced", "hdate.averaged"),
             col = c("black", "blue", "red4"))
      
      # sdate classes
      plot(years,  seast, type = "l", ylim = c(0,8), ylab = "sdate factors")
      lines(years, seast.r, type = "l", col = "blue")
      lines(years, ddate, type = "l", col = "orange")
      lines(years, sdate.a.m+.5, type = "l", col = "red4", lty=2)#, lwd=2)
      legend("topleft", lty=1, cex = .5, seg.len=.5, horiz=TRUE,
             legend = c("seasonalty.orig", "seasonalty.repl",
                        "default.sdate", "avg.window"),
             col = c("black", "blue", "orange", "red4"))
      
      # hdate classes
      plot(years,  hreas, type = "l", ylim = c(0,8), ylab = "hdate factors")
      lines(years, hreas.r, type = "l", col = "blue")
      lines(years, ddate, type = "l", col = "orange")
      lines(years, hdate.a.m+.5, type = "l", col = "red4", lty=2)#, lwd=2)
      legend("topleft", lty=1, cex = .5, seg.len=.5, horiz=TRUE,
             legend = c("hreason.orig", "hreason.repl",
                        "default.sdate", "avg.window"),
             col = c("black", "blue", "orange", "red4"))
      
      dev.off()
      
    } # plot
    
    
  } # makeplot
} #i

cat("\nNr. of pixels for which sdate or hdate have been replaced: ",count,"\n")

# Write count.dt in a csv file to record which pixels have replaced values ----
write.csv(count.dt, paste0(ncdir,
                           crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
                           "_", GCM, "_", SC, "_", min(SYs), "-", max(EYs),
                           "_pixels_with_filtered_values.csv"))

print(Sys.time()-stime)

# Write NCDF file: ----
# ------------------------------------------------------#

ncfname <- paste0(ncdir,
                  crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
                  "_", GCM, "_", SC, "_", min(SYs), "-", max(EYs),
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
ncvar_put(ncout, seast_def, ARst.r)
ncvar_put(ncout, harvr_def, ARhr.r)
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


# --------------------------# 


etime <- Sys.time()
cat("Run time:\n--------\n")
print(etime-stime)

cat("\n------\nDone !\n------\n")
