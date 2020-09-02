###############################################
#### CROP PHENOLOGY
#### Sara Minoli 2020

# CREATE LPJmL INPUT SOWING and HARVEST

# RUN TIME: 2 min
###############################################



rm(list=ls(all=TRUE))

oncluster <- TRUE

if(oncluster==TRUE) {
  working.dir <- "/home/minoli/crop_calendars_gitlab/crop_phen_paper/compute_sdate_hdate/"
} else {
  working.dir <- "D:/PROJECTS/GROWING_PERIODS_PACKAGE/"
}

makeplot <- TRUE

# PACKAGES ----
library(ncdf4)
library(data.table)
library(ggplot2)

# FUNCTIONS ----
source(paste0(working.dir, "src/units.R"))
source(paste0(working.dir, "src/ggplot.map.general.R"))
source(paste0(working.dir, "configuration/graphics.R"))
source(paste0(working.dir, "configuration/configuration.R"))
source(paste0(working.dir, "postprocessing/functions_lpjml_input_output.R"))

cropcal.dir   <- paste0(project.dir, "DATA/CROP_CALENDARS/LPJML_INPUT/")
#cropcal.dir   <- paste0(project.dir, "DATA/CROP_CALENDARS/LPJML_INPUT_swheat_only/") # for sims with spring wheat only everywhere

NCFTS <- 12
CFTBANDS <- 24
#e.g. crops <- c("Maize", "Rice", "Sorghum", "Soybean", "Spring_Wheat", "Winter_Wheat")
crops <- c("Maize", "Rice", "Sorghum", "Soybean", "Wheat")
#crops <- c("Maize", "Rice", "Sorghum", "Soybean", "Spring_Wheat")                    # for sims with spring wheat only everywhere

# import arguments from the job script ----
options(echo=FALSE) # if want see commands in output file
arg <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
print(arg)
# arg <- 1

# select variable, crop, model
dbatch1 <- subset(dbatch, subset = (step1!=0)) # remove scenarios that are not needed in step1 pre-lpjml
rownames(dbatch1) <- NULL
dbatch2 <- dbatch1[arg,]
GCM   <- dbatch2$gcm
SCgp  <- dbatch2$scenario.growp
SYgp  <- dbatch2$syear.growp
EYgp  <- dbatch2$eyear.growp
SCclm <- dbatch2$scenario.clm
SYclm <- dbatch2$syear.clm
EYclm <- dbatch2$eyear.clm

# Bind DT of all crops together ----
DTallcrops <- NULL
for (cc in 1:length(crops)) {
  CROP  <- crops[cc]
  DT    <- get(load(file = paste0(output.dir, "DT_output_crop_calendars_", CROP, "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata")))
  print(dim(DT)/2)
  print(CROP)
  DTallcrops <- rbind(DTallcrops, DT)
} # cc
DT <- DTallcrops # renaming DT, just because incompatible with get(load(DT))
rm(DTallcrops)


# create arrays sowing and harvest day ----

# empty array (3D)
AR3d <- array(NA, dim = c(720, 360, CFTBANDS), dimnames = list(lon = lons, lat = rev(lats), cft_id = c(1:CFTBANDS)))
# empty array (2D)
AR2d <- array(NA, dim = c(720, 360), dimnames = list(lon = lons, lat = rev(lats)))
# convert full array to datatable
DTfull <- setDT(melt(AR2d, value.name = "V1"))
DTfull[, c("V1") := NULL]

vars <- c("sowing_doy", "maturity_doy") # , "harvest_reason"

#vr <- 2; cr <- 1

for (vr in c(1:length(vars))) {
  for (cr in c(1:CFTBANDS)){ # loop over cft_id
    # subset by cft
    DTcft <- DT[cft_id==cr, c("lon", "lat", vars[vr]), with=F]
    # merge small and full datatables
    DTfull.cft <- merge(DTfull, DTcft, all.x = TRUE, by = c("lon", "lat"))
    
    # order by lat, lon
    setorderv(DTfull.cft, cols = c("lat", "lon"), order = c(-1, 1))
    DTfull.cft
    # coerce vector to array, array is filled with the leftmost subscript moving fastest:
    # x (top-down), y (left-right), z (front-back).
    # Array of lon, lat, time, should be ordered in the reverse order, so that first value is the top-right value in the array.
    ARcr <- array(DTfull.cft[, get(vars[vr])], dim = c(720, 360), list(lon = lons, lat = rev(lats)))
    
    AR3d[,,cr] <- ARcr
  }
  str(AR3d)
  save(AR3d, file = paste0(cropcal.dir, "AR_", vars[vr], "_lpjml_cft_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata"))
}


# Create LPJmL binary inputs of sdate and hdate ----
# Write multi-year sdate input file ----
NCELLS <- 67420
HEADER <- 43 # although in input_VERSION3 directory, TO CHECK!
NBANDS <- NCFTS*2
DTYPE <- integer()
DSIZE <- 2
SCALAR <- 1
RYEAR <- 1900
FYEAR <- SYgp+10 # mid of the period
LYEAR <- SYgp+10
NYEARS <- LYEAR-FYEAR+1

cfts <- rep(c("Wheat","Rice","Maize","Sorghum","Pulses","Temperate_Roots",
              "Tropical_Roots","Sunflower","Soybean","Groundnut","Rapeseed","Sugarcane"), 2)
#cfts <- rep(c("Spring_Wheat","Rice","Maize","Sorghum","Pulses","Temperate_Roots",
 #             "Tropical_Roots","Sunflower","Soybean","Groundnut","Rapeseed","Sugarcane"), 2)      # for sims with spring wheat only everywhere
irrs <- rep(c("Rainfed", "Irrigated"), each = NCFTS)
bands <- which(cfts%in%crops)

default_sdate <- 1
default_hdate <- 365

xsd <- array(default_sdate, dim = c(NCELLS, NBANDS, NYEARS))
xhd <- array(default_hdate, dim = c(NCELLS, NBANDS, NYEARS))
for (j in 1:NYEARS) {
  for (i in bands) {
    
    DTcft <- DT[crop==cfts[i] & irrigation==irrs[i], c("pixelnr", "sowing_doy", "maturity_doy"), with=F]
    DTcft <- DTcft[order(pixelnr)]
    print(dim(DTcft))
    xsd[,i,j] <-  DTcft[["sowing_doy"]]
    xhd[,i,j] <-  DTcft[["maturity_doy"]]
    
  } # i
} #j

y <- NULL
for (j in 1:NYEARS) {
  y <- c(y, c(t(xsd[,,j])))
}

FNAME.SD <- paste0(cropcal.dir,"sdate_",GCM, "_", SCgp, "_", SYgp, "_", EYgp,".bin")
sdfile <- file(FNAME.SD, "wb")
fwriteheader(sdfile, "LPJSOWD", 2, NBANDS, FYEAR, NYEARS, NCELLS, SCALAR)
writeBin(as.integer(y/SCALAR), sdfile, size=2, endian=.Platform$endian)
close(sdfile)


y <- NULL
for (j in 1:NYEARS) {
  y <- c(y, c(t(xhd[,,j])))
}

FNAME.HD <- paste0(cropcal.dir,"hdate_",GCM, "_", SCgp, "_", SYgp, "_", EYgp,".bin")
hdfile <- file(FNAME.HD, "wb")
fwriteheader(hdfile, "LPJSOWD", 2, NBANDS, FYEAR, NYEARS, NCELLS, SCALAR)
writeBin(as.integer(y/SCALAR), hdfile, size=2, endian=.Platform$endian)
close(hdfile)


if (makeplot==T) {
  sdate.ar <- read.crop.dates.input(FNAME.SD, NCELLS, FYEAR, FYEAR, LYEAR, HEADER, NBANDS, DTYPE, DSIZE, SCALAR)
  
  FNAME.GRID <- paste0(input.dir, "grid.bin")
  grid.ar <- read.grid(FNAME.GRID, NCELLS, 43, 2, integer(), 2, 0.01)
  
  
  sdate.df <- cbind(data.frame(grid.ar), data.frame(sdate.ar))
  colnames(sdate.df) <- c("lon", "lat", cfts)
  
  pdf(paste0(cropcal.dir,"sdate_",GCM, "_", SCgp, "_", SYgp, "_", EYgp,".pdf"))
  for (cr in cfts) {
    p <- ggplot.map.general(sdate.df, cr, colors = brewer.pal(11,"BrBG"))
    plot(p)
  }
  dev.off()
  
  
  hdate.ar <- read.crop.dates.input(FNAME.HD, NCELLS, FYEAR, FYEAR, LYEAR, HEADER, NBANDS, DTYPE, DSIZE, SCALAR)
  
  FNAME.GRID <- paste0(input.dir, "grid.bin")
  grid.ar <- read.grid(FNAME.GRID, NCELLS, 43, 2, integer(), 2, 0.01)
  
  
  hdate.df <- cbind(data.frame(grid.ar), data.frame(hdate.ar))
  colnames(hdate.df) <- c("lon", "lat", cfts)
  
  pdf(paste0(cropcal.dir,"hdate_",GCM, "_", SCgp, "_", SYgp, "_", EYgp,".pdf"))
  for (cr in cfts) {
    p <- ggplot.map.general(hdate.df, cr, colors = brewer.pal(11,"BrBG"))
    plot(p)
  }
  dev.off()
  
}
