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
working.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate/"
project.dir <- "/p/projects/macmit/users/minoli/PROJECTS/GGCMI_ph3/"
climate.dir <- paste0(project.dir, "monthly_climate/")
input.dir   <- paste0(project.dir, "inputs/")
output.dir  <- paste0(project.dir, "crop_calendars/DT/")
figure.dir  <- paste0(project.dir, "crop_calendars/DT/")

# Grid file ----
grid.fn <- paste0("/p/projects/lpjml/input/ISIMIP3/grid.bin")
NCELLS <- 67420

# Parameters file ----
croppar.fn <- paste0(working.dir,"parameters/","crop_parameters.csv")

# Simulation climate settings file ----
simsetting.fn <- paste0(working.dir,"configuration/","scenarios_ggcmi_ph3.csv")

# Crops
crops <- c("Maize","Rice","Sorghum","Soybean","Spring_Wheat","Winter_Wheat")



# IMPORT ----
#___________________________________________________________#
# Read grid input ----
read.lpjml.grid <- function(fname = "grid/path/grid.bin",
                            band_names = c("lon", "lat"),
                            ncells = 67420,
                            header = 43,
                            nbands = 2,
                            dtype =  integer(),
                            dsize =  2,
                            scalar = .01) {
  
  cat(paste("\nReading grid.bin (input/output):\n-------------------------------\n", fname))
  
  # check file size
  check.fs <- (file.size(fname)-header)/ncells/dsize==nbands
  cat(paste("\nFile size as expected = ", check.fs, "\n"))
  
  # read out data
  fcon <- file(fname, open = "rb")
  seek(con = fcon, where = header, origin = "start")
  x <- array(NA, c(ncells, nbands))
  for (i in 1:ncells){
    x[i,] <- readBin(fcon, what = dtype, size = dsize , n = nbands)*scalar
  }
  close.connection(fcon)
  colnames(x) <- band_names
  cat(str(x))
  return(x)
}




# Grid ----
coord.df <- as.data.frame(read.lpjml.grid(
  grid.fn, band_names = c("lon", "lat"), ncells = 67420, header = 43,
  nbands = 2, dtype =  integer(), dsize =  2, scalar = .01))

# Constants
source(paste0(working.dir, "parameters/", "constant_values.R"))

# Parameters
cat("Importing parameter table for all crops ...",
    "-------------------------------------------",
    sep = "\n")
crop_parameters_all <- read.csv(croppar.fn, header = T, stringsAsFactors = F)
crop_parameters_all <- subset(crop_parameters_all, crop_name%in%crops)
print(crop_parameters_all[,c("crop_id", "crop_name")])

# Simulation settings
dfbatch <- read.csv(simsetting.fn, header = TRUE, stringsAsFactors = FALSE)

# Graphics ----
cat("Importing graphics for plotting (labelling and color schemes) ...",
    "-----------------------------------------------------------------",
    sep = "\n")
source(paste0(working.dir, "configuration/", "graphics.R"))

