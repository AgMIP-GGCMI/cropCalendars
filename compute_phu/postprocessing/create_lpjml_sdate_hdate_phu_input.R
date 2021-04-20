# ------------------------------------------------------# ----
# Project: crop_calendar
#
# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
#
# Date Created: 2021-04-19
#
# Description: Generate sdate and phu .bin inputs for LPJmL
# ------------------------------------------------------#

rm(list=ls(all=T))

library(ncdf4)

# ----

# ------------------------------------------------------#
# Import Job Arguments ----
options(echo=FALSE) # if want see commands in output file
argr <- commandArgs(trailingOnly = TRUE)
print(argr)
# argr <- c("UKESM1-0-LL", "ssp585")

GCM    <- argr[1]
SC     <- argr[2]
# CROP   <- argr[3]
# IRRI   <- argr[4]

if (SC == "historical") {
  syear <- 1991
  eyear <- 2014
} else {
  syear <- 2014
  eyear <- 2100
}

years  <- syear:eyear
NYEARS <- length(years)

# ------------------------------------------------------#
# Paths ----
working.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/"
project.dir <- "/p/projects/macmit/users/minoli/PROJECTS/GGCMI_ph3_adaptation/"
ncdir_rbcal <- paste0(project.dir, "crop_calendars/ncdf/", GCM, "/", SC, "/")
ncdir_ggcmi <- "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar/"
clmdir      <- paste0(project.dir, "crop_calendars/clm/", GCM, "/", SC, "/")

# ------------------------------------------------------#
# Functions ----
source(paste0(working.dir, "import.functions.R"))
import.functions(paste0(working.dir, "compute_sdate_hdate/src/"))
import.functions(paste0(working.dir, "compute_phu/src/"))


# ------------------------------------------------------#
# Crop Names: ----
crop.ls <- list(all_low = c("winter_wheat", "spring_wheat", "maize", "rice1", "rice2",
                            "soybean", "millet", "sorghum","peas","sugar_beat",
                            "cassava","rape_seed","sunflower","nuts","sugarcane"),
                rb_cal  = c("Winter_Wheat", "Spring_Wheat", "Maize", "Rice", NA,
                            "Soybean", NA, "Sorghum", NA, NA,
                            NA, NA, NA, NA, NA),
                ggcmi   = c("wwh","swh","mai","ri1","ri2",
                            "soy","mil","sor","pea","sgb",
                            "cas","rap","sun","nut","sgc"),
                vernal  = c("yes","no","no","no","no",
                            "no","no","no","no","no",
                            "no","yes","no","no","no"))

irri.ls <- list(all_low = c("rainfed", "irrigated"),
                rb_cal  = c("Rainfed", "Irrigated"),
                ggcmi   = c("rf", "ir"))

bands  <- rep(crop.ls[["ggcmi"]], 2)
NBANDS <- length(bands)
irris  <- rep(irri.ls[["ggcmi"]], each = length(crop.ls[["ggcmi"]]))


# ------------------------------------------------------#
# Get Grid ----
grid.fn <- paste0("/p/projects/lpjml/input/ISIMIP3/grid.bin")
NCELLS <- 67420

grid <- as.data.frame(read.lpjml.grid(
  grid.fn, band_names = c("lon", "lat"), ncells = 67420, header = 43,
  nbands = 2, dtype =  integer(), dsize =  2, scalar = .01))


# ------------------------------------------------------#
# Get sdate, hdate & phu ----

xsd <- array(0, dim = c(NCELLS, NBANDS, NYEARS))
xhd <- array(0, dim = c(NCELLS, NBANDS, NYEARS))
xph <- array(0, dim = c(NCELLS, NBANDS, NYEARS))

for (yy in 1:length(years)) {
  #yy <- 1
  cat("\nYear", years[yy], "\n")
  
  for (bb in 1:NBANDS) {
    # yy <- 1; i <- 1; bb <- 1
    cat("\nBand", bands[bb], "--", irris[bb], "\n")
    
    cr <- which(crop.ls[["ggcmi"]]==bands[bb])
    ir <- which(irri.ls[["ggcmi"]]==irris[bb])
    
    # --------------------------# 
    nc1 <- paste0(ncdir_rbcal,
                  crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
                  "_", GCM, "_", SC, "_", syear, "-", eyear,
                  "_ggcmi_ph3_rule_based_crop_calendar.nc4")
    
    nc <- nc_open(nc1)
    sdate <- ncvar_get(nc, varid = "plant-day", start = c(1, 1, yy),
                       count = c(720, 360, 1))
    hdate <- ncvar_get(nc, varid = "maty-day",  start = c(1, 1, yy),
                       count = c(720, 360, 1))
    lons  <- ncvar_get(nc, varid = "lon")
    lats  <- ncvar_get(nc, varid = "lat")
    nc_close(nc)
    
    # --------------------------# 
    nc2 <- paste0(ncdir_rbcal,
                  crop.ls[["ggcmi"]][cr], "_", irri.ls[["ggcmi"]][ir],
                  "_", GCM, "_", SC, "_", syear, "-", eyear,
                  "_ggcmi_ph3_rule_based_phu.nc4")
    
    nc <- nc_open(nc2)
    phu   <- ncvar_get(nc, varid = "phu",  start = c(1, 1, yy),
                       count = c(720, 360, 1))
    nc_close(nc)
    
    # --------------------------# 
    
    for (i in 1:NCELLS) {
      #i <- 1
      if(i%%1e3==0) cat(i, "\t")
      
      # --------------------------# 
      
      ilo <- which(lons==grid[i,1])
      ila <- which(lats==grid[i,2])
      
      # --------------------------# 
      
      xsd[i, bb ,yy] <- sdate[ilo, ila]
      xhd[i, bb ,yy] <- hdate[ilo, ila]
      xph[i, bb ,yy] <-   phu[ilo, ila]
      
      
    } # i
    
  } # bb
  
} # yy


# ------------------------------------------------------#

y <- NULL
for (j in 1:NYEARS) {
  y <- c(y, c(t(xsd[,,j])))
}

FNAME.SD <- paste0(clmdir,"sdate_",GCM, "_", SC, "_", syear, "_", eyear,
                   "_ggcmi_ph3_rule_based_crop_calendar.clm")
sdfile <- file(FNAME.SD, "wb")
fwriteheader(sdfile, "LPJSOWD", 2, NBANDS, FYEAR, NYEARS, NCELLS, SCALAR)
writeBin(as.integer(y/SCALAR), sdfile, size=2, endian=.Platform$endian)
close(sdfile)

# --------------------------# 

y <- NULL
for (j in 1:NYEARS) {
  y <- c(y, c(t(xhd[,,j])))
}

FNAME.HD <- paste0(clmdir,"hdate_",GCM, "_", SC, "_", syear, "_", eyear,
                   "_ggcmi_ph3_rule_based_crop_calendar.clm")
hdfile <- file(FNAME.HD, "wb")
fwriteheader(hdfile, "LPJSOWD", 2, NBANDS, FYEAR, NYEARS, NCELLS, SCALAR)
writeBin(as.integer(y/SCALAR), hdfile, size=2, endian=.Platform$endian)
close(hdfile)

# --------------------------# 

y <- NULL
for (j in 1:NYEARS) {
  y <- c(y, c(t(xph[,,j])))
}

FNAME.HD <- paste0(clmdir,"phu_",GCM, "_", SC, "_", syear, "_", eyear,
                   "_ggcmi_ph3_rule_based_crop_calendar.clm")
hdfile <- file(FNAME.HD, "wb")
fwriteheader(hdfile, "LPJSOWD", 2, NBANDS, FYEAR, NYEARS, NCELLS, SCALAR)
writeBin(as.integer(y/SCALAR), hdfile, size=2, endian=.Platform$endian)
close(hdfile)






# 
# #--- OLD ---#
# 
# xsd <- array(default_sdate, dim = c(NCELLS, NBANDS, NYEARS))
# xhd <- array(default_hdate, dim = c(NCELLS, NBANDS, NYEARS))
# for (j in 1:NYEARS) {
#   for (i in bands) {
#     
#     DTcft <- DT[crop==cfts[i] & irrigation==irrs[i], c("pixelnr", "sowing_doy", "maturity_doy"), with=F]
#     DTcft <- DTcft[order(pixelnr)]
#     print(dim(DTcft))
#     xsd[,i,j] <-  DTcft[["sowing_doy"]]
#     xhd[,i,j] <-  DTcft[["maturity_doy"]]
#     
#   } # i
# } #j
# 
# y <- NULL
# for (j in 1:NYEARS) {
#   y <- c(y, c(t(xsd[,,j])))
# }
# 
# FNAME.SD <- paste0(cropcal.dir,"sdate_",GCM, "_", SCgp, "_", SYgp, "_", EYgp,".bin")
# sdfile <- file(FNAME.SD, "wb")
# fwriteheader(sdfile, "LPJSOWD", 2, NBANDS, FYEAR, NYEARS, NCELLS, SCALAR)
# writeBin(as.integer(y/SCALAR), sdfile, size=2, endian=.Platform$endian)
# close(sdfile)
# 
# 
# y <- NULL
# for (j in 1:NYEARS) {
#   y <- c(y, c(t(xhd[,,j])))
# }
# 
# FNAME.HD <- paste0(cropcal.dir,"hdate_",GCM, "_", SCgp, "_", SYgp, "_", EYgp,".bin")
# hdfile <- file(FNAME.HD, "wb")
# fwriteheader(hdfile, "LPJSOWD", 2, NBANDS, FYEAR, NYEARS, NCELLS, SCALAR)
# writeBin(as.integer(y/SCALAR), hdfile, size=2, endian=.Platform$endian)
# close(hdfile)
# 
