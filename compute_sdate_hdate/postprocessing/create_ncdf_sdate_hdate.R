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
source(paste0(working.dir, "postprocessing/ncdfs.R"))

cropcal.dir   <- paste0(project.dir, "DATA/CROP_CALENDARS/LPJML_INPUT/")
#cropcal.dir   <- paste0(project.dir, "DATA/CROP_CALENDARS/LPJML_INPUT_swheat_only/") # for sims with spring wheat only everywhere
ncdir <- paste0(project.dir, "DATA/CROP_CALENDARS/NCDF/")

NCFTS <- 12
CFTBANDS <- 24
#e.g. crops <- c("Maize", "Rice", "Sorghum", "Soybean", "Spring_Wheat", "Winter_Wheat")
crops <- c("Maize", "Rice", "Sorghum", "Soybean", "Wheat")
#crops <- c("Maize", "Rice", "Sorghum", "Soybean", "Spring_Wheat")                    # for sims with spring wheat only everywhere
irrigs <- c("Rainfed", "Irrigated")

# # import arguments from the job script ----
# options(echo=FALSE) # if want see commands in output file
# arg <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
# print(arg)
arg <- 13

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


# cc <- 1; ir <- 1
for (cc in 1:length(crops)) {
  for (ir in 1:length(irrigs)) {
    
    DTgrid <- DT[crop==crops[cc] & irrigation==irrigs[ir], c("lon", "lat")]
    DTcr <- DT[crop==crops[cc] & irrigation==irrigs[ir]]
    
    sdate <- round(DTcr$sowing_doy)
   
    ARnc <- lpjml.array.to.nccube(ARin = sdate, ARgrid = DTgrid, years = 2000)
    FNAME <- paste0(ncdir, "nc_output_crop_calendars_", crops[cc], "_", irrigs[ir], "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp)
    write.ncdf.1cube(cube = ARnc, fname = FNAME, cropname = CROP, varshort = "sdate", varlong = "sowing date", years = 2000, unit = "DOY", precision = "float")
    
#    cube = ARnc; fname = FNAME; cropname = CROP; varshort = "sdate"; varlong = "sowing date"; years = 2000; unit = "DOY"; precision = "float"
    
    
    hdate <- round(DTcr$maturity_doy)
    ARnc <- lpjml.array.to.nccube(ARin = hdate, ARgrid = DTgrid, years = 2000)
    FNAME <- paste0(ncdir, "nc_output_crop_calendars_", crops[cc], "_", irrigs[ir], "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp)
    write.ncdf.1cube(cube = ARnc, fname = FNAME, cropname = CROP, varshort = "hdate", varlong = "harvest date", years = 2000, unit = "DOY", precision = "float")
    
    print(paste(crops[cc], irrigs[ir]))
    
  } #ir
} #cr
