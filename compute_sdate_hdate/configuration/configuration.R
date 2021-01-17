#-----------------------------------------------------------#
# Climate-driven sowing and harvest dates                   #
# R-Code                                                    #
# Written by Sara Minoli                                    #
# Based on Minoli et al. (2019) Global and Planetary Change #
#-----------------------------------------------------------#

#-----------------------------------------------------------#
# CONFIGURATION SCRIPT #
#-----------------------------------------------------------#

# PACKAGES ----
#__________________________________________________________
library(data.table) # more userfriendly dataframes
library(ggplot2)
library(fields)
library(hexbin) # to plot geom_hex
library(RColorBrewer)
library(viridis)
library(scales) # adds some functionalities to ggplot2 plot scales
# library(ncdf4)
# library(plyr)
# library(foreach) # to parallelize loops

# DEFINE ----
#___________________________________________________________#

# PATHS ----
project.dir   <- "/p/projects/macmit/users/minoli/PROJECTS/CROP_PHENOLOGY_v01/"
# project.dir   <- "C:/Users/minoli/Documents/Work/PROJECTS/CROP_PHENOLOGY_v01/"
climate.dir   <- paste0(project.dir, "DATA/CLIMATE/")
input.dir     <- paste0(project.dir, "DATA/INPUT/")
output.dir    <- paste0(project.dir, "DATA/CROP_CALENDARS/DT/")
figure.dir    <- paste0(project.dir, "DATA/CROP_CALENDARS/DT/")

# Grid file ----
grid.fn <- paste0(input.dir, "grid.bin")
NCELLS <- 67420

# Parameters file ----
croppar.fn <- paste0(working.dir, "parameters/", "crop_parameters.txt")
#croppar.fn <- paste0(working.dir, "parameters/", "crop_parameters_original_Minoli_et_al_2019.txt")

# Simulation climate settings file ----
simsetting.fn <- paste0(working.dir, "configuration/", "climate_scenarios_to_simulate.txt")

# Crops
crops <- c("Maize", "Rice", "Sorghum", "Soybean", "Spring_Wheat", "Winter_Wheat")



# IMPORT ----
#___________________________________________________________#
# grid input
read.grid <- function(fname, ncells = NCELLS, header = HEADER, nbands = NBANDS, dtype = DTYPE, dsize = DSIZE, scalar = SCALAR) {
  
  print(paste("Reading grid.bin input:", fname))
  
  # check file size
  check.fs <- (file.size(fname)-header)/ncells/dsize==nbands
  print(paste("File size as expected = ", check.fs))
  
  # read out data
  fcon <- file(fname, open = "rb")
  seek(con = fcon, where = header, origin = "start")
  x <- array(NA, c(ncells, nbands))
  for (i in 1:ncells){
    x[i,] <- readBin(fcon, what = dtype, size = dsize , n = nbands)*scalar
  }
  close.connection(fcon)
  print(str(x))
  return(x)
}
# Grid ----
# FNAME <- grid.fn
# HEADER <- 43 # clm2
# NBANDS <- 2  # lon, lat
# DTYPE <- integer()
# DSIZE <- 2
# SCALAR <- 0.01

coord.df <- as.data.frame(read.grid(fname = grid.fn, 67420, 43, 2, integer(), 2, 0.01))
names(coord.df) <- c("lon", "lat")


# Constants
source(paste0(working.dir, "parameters/", "constant_values.R"))

# Parameters
cat("Importing parameter table for all crops ...",
    "-------------------------------------------",
    sep = "\n")
crop_parameters_all <- subset(read.table(croppar.fn, header = TRUE, stringsAsFactors = FALSE),
                              crop_name%in%crops)
print(crop_parameters_all[,c("crop_id", "crop_name")])

# old version param file
# cfts <- c(1, 2, 3, 4, 9) #LPJmL cft to be simulated
# crop_parameters <- cft_par[cfts, ]
#ncft <- length(crop_parameters$cft_id)

# Simulation settings
dbatch <- read.table(simsetting.fn, header = TRUE, stringsAsFactors = FALSE)

# Graphics ----
cat("Importing graphics for plotting (labelling and color schemes) ...",
    "-----------------------------------------------------------------",
    sep = "\n")
source(paste0(working.dir, "configuration/", "graphics.R"))

