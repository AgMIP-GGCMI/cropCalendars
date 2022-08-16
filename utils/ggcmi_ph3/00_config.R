# ---------------------------------------------------------------------------- #
# Configuration for GGCMI phase3 (ISIMIP3b climate)

# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
# ---------------------------------------------------------------------------- #

library(devtools)
library(ncdf4)
library(abind)
library(data.table)
library(foreach)
library(cropCalendars)
library(pryr)          # for tracking memory usage
library(zoo)           # for rolling mean
#library(unix)

# ------------------------------------ #
# General Settings

# Output directory: where output data are going to be saved
output_dir <- paste0("/p/projects/macmit/users/minoli/PROJECTS/",
                   "GGCMI_ph3_adaptation_test_220811/ISIMIP3b/")

parallel     <- TRUE
cluster_job  <- TRUE
plot_results <- TRUE

# Years for which crop calendars should be computed
ccal_years    <- seq(1601, 2091, by = 10)
# Number of years for average climate
clm_avg_years <- 30

climate_dir <- paste0("/p/projects/macmit/data/GGCMI/AgMIP.input/",
                      "phase3/climate_land_only/")

# Climate input files
gcms <- c(
  "GFDL-ESM4",
  "IPSL-CM6A-LR",
  "MPI-ESM1-2-HR",
  "MRI-ESM2-0",
  "UKESM1-0-LL"
)
enms <- c(
    "GFDL-ESM4"     = "r1i1p1f1",
    "IPSL-CM6A-LR"  = "r1i1p1f1",
    "MPI-ESM1-2-HR" = "r1i1p1f1",
    "MRI-ESM2-0"    = "r1i1p1f1",
    "UKESM1-0-LL"   = "r1i1p1f2"
)
scens <- c(
  "picontrol",
  "historical",
  "ssp126",
  "ssp585",
  "ssp370"
)
syears <- list(
  "picontrol"  = seq(1601, 2091, by = 10),
  "historical" = seq(1851, 2011, by = 10),
  "ssp126"     = c(2015, seq(2021, 2091, by = 10)),
  "ssp585"     = c(2015, seq(2021, 2091, by = 10)),
  "ssp370"     = c(2015, seq(2021, 2091, by = 10))
)
eyears <- list(
  "picontrol"  = seq(1610, 2100, by = 10),
  "historical" = c(seq(1860, 2014, by = 10), 2014),
  "ssp126"     = seq(2020, 2100, by = 10),
  "ssp585"     = seq(2020, 2100, by = 10),
  "ssp370"     = seq(2020, 2100, by = 10)
)

# ------------------------------------ #
# Read grid file
readGridLPJmL <- function(
  fname  = "/p/projects/lpjml/input/historical/input_VERSION2/grid.bin",
  ncells = NULL,
  header = NULL,
  nbands = NULL,
  dtype  = NULL,
  dsize  = NULL,
  scalar = NULL
  ) {

  print(paste("Reading grid.bin input:", fname))

  # check file size
  check.fs <- (file.size(fname) - header)/ncells/dsize == nbands
  print(paste("File size as expected = ", check.fs))

  # read out data
  fcon <- file(fname, open = "rb")
  seek(con = fcon, where = header, origin = "start")
  x <- array(NA, c(ncells, nbands))
  for (i in seq_len(ncells)) {
    x[i,] <- readBin(fcon, what = dtype, size = dsize , n = nbands)*scalar
  }
  close.connection(fcon)
  print(str(x))
  return(x)
}

grid_df <- as.data.frame(
  readGridLPJmL(
    ncells = 67420, header = 43, nbands = 2, dtype = integer(),
    dsize = 2, scalar = 0.01)
  )
names(grid_df) <- c("lon", "lat")

# ------------------------------------ #
cat("\nConfigs imported.\n")