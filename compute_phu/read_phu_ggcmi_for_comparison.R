# ------------------------------------------------------# ----
# Project: lpjmlToolKit
#
# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
#
# Date Created: 2021-04-08
#
# Description:
# Read PHUs from ggcmi_ph3 input data and compare with
#   results from my scripts
# ------------------------------------------------------#

rm(list=ls(all=T))

library(ncdf4)

# ------------------------------------------------------#
# Functions ----
# sowing date and harvest date input (also multiple years)
read.crop.dates.input <- function(fname, ncells = NCELLS,
                                  ryear = RYEAR, fyear = FYEAR, lyear = LYEAR,
                                  header = HEADER, nbands = NBANDS,
                                  dtype = DTYPE, dsize = DSIZE, scalar = SCALAR) {
  print(paste("Reading crop dates input:", fname))
  
  #number of years
  nyears <- lyear-fyear+1
  
  # check file size
  check.fs <- (file.size(fname)-header)/ncells/dsize/nyears==nbands
  print(paste("File size as expected = ", check.fs))
  
  fcon <- file(fname, open = "rb")
  seek(fcon, header, origin = "start")
  
  x <- array(NA, dim = c(ncells, nbands, nyears))
  
  for (j in 1:nyears) {
    
    year <- fyear+(j-1)
    seek(fcon, where = header+((year-ryear)*ncells*nbands*dsize), origin = "start")
    
    for (i in 1:ncells) {
      x[i,,j] <- readBin(fcon, what = dtype, size = dsize , n = nbands)*scalar
    }
  }
  close.connection(fcon)
  print(str(x))
  return(x)
}

fpath <- paste0("/home/minoli/git_lpjmlToolKit/development/functions/")
fsources <- paste0(fpath, list.files(fpath, pattern = ".R", recursive = T))
invisible(lapply(fsources, source))
cat("\nImporting functions:\n", "-----------------",
    list.files(fpath, pattern = ".R", recursive = T), sep = "\n")      

# ------------------------------------------------------#
# Paths ----
project.dir <- "/p/projects/macmit/users/minoli/PROJECTS/GGCMI_ph3_adaptation/"
ggcmi.dir <- "/p/projects/lpjml/input/crop_calendar/"
grid.fn  <- "/p/projects/lpjml/input/historical/input_VERSION2/grid.bin"

# ------------------------------------------------------#
# Crop and irrigation naming ----
ggcmi.crops <- c("wwh","swh","mai","ri1","ri2","soy","mil","sor",
                 "pea","sgb","cas","rap","sun","nut","sgc")

crop.ls <- list(all_low = c("maize", "rice", "sorghum", "soybean",
                            "spring_wheat", "winter_wheat"),
                rb_cal  = c("Maize", "Rice", "Sorghum", "Soybean",
                            "Spring_Wheat", "Winter_Wheat"),
                ggcmi   = c("mai", "ri1", "sor", "soy", "swh", "wwh"),
                vernal  = c("no",  "no",  "no",  "no",  "no",  "yes"))
irri.ls <- list(all_low = c("rainfed", "irrigated"),
                rb_cal  = c("Rainfed", "Irrigated"),
                ggcmi   = c("rf", "ir"))

# ------------------------------------------------------#
# Grid ----
grid.in <- read.lpjml.grid(grid.fn)
NCELLS <- nrow(grid.in)

# ------------------------------------------------------#
# PHU files computed from observed crop calendars ----
#   used in GGCMI ph3 first round (w/o adaptation)
sdate.fn <- paste0(ggcmi.dir, "sdates_ggcmi_phase3_v1.01_67420.clm")
hdate.fn <- paste0(ggcmi.dir, "mdates_ggcmi_phase3_v1.01_67420.clm")
phu.fn   <- paste0(ggcmi.dir, "phu_ukesm1-0-ll_historical_1984-2014_ggcmi_phase3_v1.01_67420.clm")

read.lpjml.longheader(sdate.fn)
read.lpjml.longheader(hdate.fn)
read.lpjml.longheader(phu.fn)

NBANDS <- 30

sdate <- read.crop.dates.input(sdate.fn, 67420, ryear = 2000, fyear = 2000, lyear = 2000, header = 43, nbands = 30, dtype = integer(), dsize = 2, scalar = 1)

hdate <- read.crop.dates.input(hdate.fn, 67420, ryear = 2000, fyear = 2000, lyear = 2000, header = 43, nbands = 30, dtype = integer(), dsize = 2, scalar = 1)

phu <- read.crop.dates.input(phu.fn, 67420, ryear = 2000, fyear = 2000, lyear = 2000, header = 43, nbands = 30, dtype = integer(), dsize = 2, scalar = 1)

# ------------------------------------------------------#
# PHU files computed from observed crop calendars
#   using the updated scripts for multi-years phu computation ----

GCM <- "UKESM1-0-LL"
SC  <- "historical"
# CROP <- "swh"
# IRRI <- "rf"

myphu <- array(NA, c(NCELLS, NBANDS, 1))

for (cc in 1:length(ggcmi.crops)) {
  
  CROP <- ggcmi.crops[cc]
  cat("\nDoing", CROP)
  
  cr <- which(crop.ls[["ggcmi"]]==CROP)
  # ir <- which(irri.ls[["ggcmi"]]==IRRI)
  
  if (length(cr)>0) {
    ncdir <- paste0(project.dir, "crop_calendars/ncdf/", GCM, "/", SC, "/")
    fn.rf <- paste0(ncdir,
                    crop.ls[["ggcmi"]][cr], "_", "rf",
                    "_", GCM, "_", SC, "_", 1984, "-", 2014,
                    "_ggcmi_ph3_obs-calendars_phu.nc4")
    fn.ir <- paste0(ncdir,
                    crop.ls[["ggcmi"]][cr], "_", "ir",
                    "_", GCM, "_", SC, "_", 1984, "-", 2014,
                    "_ggcmi_ph3_obs-calendars_phu.nc4")
    
    nc <- nc_open(fn.rf)
    lons <- ncvar_get(nc, "lon")
    lats <- ncvar_get(nc, "lat")
    nc_close(nc)
    
    nc <- nc_open(fn.rf)
    rf <- ncvar_get(nc, "phu")
    nc_close(nc)
    
    nc <- nc_open(fn.ir)
    ir <- ncvar_get(nc, "phu")
    nc_close(nc)
    
    # Convert nc array to lpjml vector
    for (i in 1:nrow(grid.in)) {
      ilo <- which(lons==grid.in[i,1])
      ila <- which(lats==grid.in[i,2])
      
      myphu[i,cc,1]                     <- rf[ilo, ila]
      myphu[i,cc+length(ggcmi.crops),1] <- ir[ilo, ila]
    } # i
  } # if
  
} # cc

str(phu)
str(myphu)

summary(myphu-abs(phu))

for (i in which(ggcmi.crops%in%crop.ls[["ggcmi"]])) {
  #i <- 1
  cat("\nPlotting", ggcmi.crops[i], "\n")
  
  plot.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_phu/tmp/"
  phu.i <- abs(round(phu[,i,1]))
  p <- plot.lpjml.1D.array2(phu.i, grid.ar = grid.in)
  ggsave(paste0(plot.dir, "phu.ggcmi_", ggcmi.crops[i], ".png"), p)
  
  myphu.i <- abs(round(myphu[,i,1]))
  p <- plot.lpjml.1D.array2(myphu.i, grid.ar = grid.in)
  ggsave(paste0(plot.dir, "phu.ruleb_", ggcmi.crops[i], ".png"), p)
  
  diff.i <- round(phu.i-myphu.i)
  p <- plot.lpjml.1D.array2(diff.i, grid.ar = grid.in)
  ggsave(paste0(plot.dir, "phu.diff_", ggcmi.crops[i], ".png"), p)
  
}

# gdf <- data.frame(lon=grid.in[,1], lat=grid.in[,2])
# subset(gdf, lon==-100.25 & lat==60.25)
# pix <- 7236
