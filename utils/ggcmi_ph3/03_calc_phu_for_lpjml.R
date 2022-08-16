# ---------------------------------------------------------------------------- #
# Calculate PHUs for LPJmL and create NCDF files (multi-year and crop-specific)
#  for GGCMI-phase3 adaptation runs

# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
# ---------------------------------------------------------------------------- #

rm(list = ls(all = TRUE))

starttime <- Sys.time() # Track run-time
print(starttime)

# ------------------------------------ #
# General settings
work_dir <- setwd(paste(
  "/home/minoli/crop_calendars_gitlab/r_package/cropCalendars/utils/ggcmi_ph3/"
))
source(paste0(work_dir, "/00_config.R"))

makeplot <- TRUE
NCELLS   <- 67420

# ------------------------------------ #
# Individual-run settings
if (cluster_job == TRUE) {
  # import argument from bash script
  options(echo = FALSE) # if you want see commands in output file
  args <- commandArgs(trailingOnly = TRUE)
} else {
  args <- c("GFDL-ESM4", "ssp585", "mai", "rf")
}
print(args)

# ------------------------------------ #
# Select variable, crop, model, year
gcm    <- args[1]
scen   <- args[2]
cro    <- args[3]
irri   <- args[4]

if (scen == "2015gs") {

  FYnc <- 2015 # first year ncdf output file
  LYnc <- 2100 # last year ncdf output file

  # Sowing and cultivar to change every 10 years
  SYs <- 2015 # Start of the period
  EYs <- 2020 # End of the period
  # Computing sowing and harvest dates based on preceding 30-years climate
  FYs <- 1981 # First year DT file (output of main.R)
  LYs <- 2010 # Last  year DT file (output of main.R)

  nhist <- length(EYs[EYs < 2015]) # number of historical time slices
  HYs <- c(rep("historical", nhist), rep("ssp126", length(SYs)-nhist)) # historical/ssp years

} else if (scen == "historical") {

  # first and last year of ncdf files
  FYnc <- 1991
  LYnc <- 2014

  # Sowing and cultivar to change every 10 years
  SYs <-   seq(1991, 2011, by = 10) # Start of the period
  EYs <- c(seq(2000, 2014, by = 10), 2014) # End of the period
  # Computing sowing and harvest dates based on preceding 30-years climate
  FYs <-   seq(1961, 1981, by = 10) # First year DT file (output of main.R)
  LYs <-   seq(1990, 2010, by = 10) # Last  year DT file (output of main.R)

  HYs <- rep(scen, length(SYs))

} else {

  FYnc <- 2015 # first year ncdf output file
  LYnc <- 2100 # last year ncdf output file

  # Sowing and cultivar to change every 10 years
  SYs <- c(2015, seq(2021, 2091, by = 10)) # Start of the period
  EYs <- c(2020, seq(2030, 2100, by = 10)) # End of the period
  # Computing sowing and harvest dates based on preceding 30-years climate
  FYs <- seq(1981, 2061, by = 10) # First year DT file (output of main.R)
  LYs <- seq(2010, 2090, by = 10) # Last  year DT file (output of main.R)

  nhist <- length(EYs[EYs < 2015]) # number of historical time slices
  HYs <- c(rep("historical", nhist), rep(scen, length(SYs)-nhist)) # historical/ssp years

}

print(data.frame(SYs, EYs, FYs, LYs, HYs, FYnc, LYnc))

years  <- FYnc:LYnc
nyears <- length(years)

ncdir  <- paste0(output_dir, "crop_calendars/ncdf/", gcm, "/", scen, "/")

# ------------------------------------------------------#
# Compute PHUs and Write ncdfs

generatePHUTserie_isimip3(
    ncdir         = ncdir,
    gcm           = gcm,
    scen          = scen,
    cro           = cro,
    irri          = irri,
    SYs           = SYs,
    EYs           = EYs,
    FYnc          = FYnc,
    LYnc          = LYnc,
    crop_par_file = NULL
)

# ------------------------------------------------------#

cat("\n", paste("Computation PHUs ended!"),
    "-------------------------------------------------------", sep = "\n")

endtime <- Sys.time()
print(endtime)

print(endtime-starttime)
