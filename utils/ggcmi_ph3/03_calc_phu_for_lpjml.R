# ---------------------------------------------------------------------------- #
# Calculate PHUs for LPJmL and create NCDF files (multi-year and crop-specific)
#  for GGCMI-phase3 adaptation runs

# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
# ---------------------------------------------------------------------------- #

rm(list = ls(all.names = TRUE))

starttime <- Sys.time() # Track run-time
print(starttime)

# ------------------------------------ #
# General settings
# Run from the pipeline dir: sbatch passes --chdir=$WD, interactive runs cd there.
work_dir <- getwd()
source(file.path(work_dir, "00_config.R"))

makeplot <- TRUE

# Read the LPJmL grid here (the only stage that needs it, for the .clm output).
# lpjmlkit::read_io auto-detects the header; round to the grid's native 0.01-degree
# resolution. grid_df is passed explicitly to generatePHUTserie_isimip3().
library(lpjmlkit)
grid_io <- suppressWarnings(read_io(grid_file, silent = TRUE))
grid_df <- data.frame(
  lon = round(as.numeric(grid_io$data[, 1, 1]), 2),
  lat = round(as.numeric(grid_io$data[, 1, 2]), 2)
)

# ------------------------------------ #
# Individual-run settings
if (cluster_job == TRUE) {
  # import argument from bash script
  options(echo = FALSE) # if you want see commands in output file
  args <- commandArgs(trailingOnly = TRUE)
} else {
  args <- c("GFDL-ESM4", "ssp585")
}
print(args)

# ------------------------------------ #
# One compact job per gcm x scenario: all crops x irrigations are processed in a single
# call so each year's daily temperature is read once and reused across crops.
gcm    <- args[1]
scen   <- args[2]
ncores <- if (length(args) >= 3) as.integer(args[3]) else as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
# CROPS env (ggcmi tokens, comma- or space-separated) restricts the crop set; default all.
crops_env <- trimws(unlist(strsplit(Sys.getenv("CROPS", ""), "[, ]+")))
crops_env <- crops_env[nzchar(crops_env)]
cros  <- if (length(crops_env) > 0) crops_env else crop_ls[["ggcmi"]]
irris <- c("rf", "ir")

# DRS crop-calendar directory + soc token (output of stage 02). ISIMIP3a observational
# scenarios (obsclim/spinclim -> histsoc, counterclim -> countersoc) -> ISIMIP3a; ESMs ->
# ISIMIP3b per gcm x soc. MUST mirror stage 02 (02_assemble_annual_ncdf.R) exactly.
if (scen %in% isimip3a_scenarios) {
  soc_file <- if (scen == "counterclim") "countersoc" else "histsoc"
  cal_dir  <- paste0(output_dir, "ISIMIP3a/InputData/socioeconomic/crop_calendar/", soc_file, "/")
} else {
  soc_dir  <- if (scen == "historical") "historical" else paste0(scen, "soc-adapt")
  soc_file <- if (scen == "historical") "histsoc" else scen
  cal_dir  <- paste0(output_dir, "ISIMIP3b/InputData/socioeconomic/crop_calendar/", gcm, "/", soc_dir, "/")
}

# Derive the product year range from any stage-02 file (all crops share it): stage 02
# names the file from the discovered emit_years (min/max), so reading it back keeps the
# two stages on a single source of truth.
hits <- Sys.glob(paste0(cal_dir, "ggcmi-crop-calendar_", tolower(gcm), "_", soc_file, "_*_annual_*.nc"))
if (length(hits) == 0)
  stop("Stage-02 crop-calendar files not found (run stage 02 first): ", cal_dir)
m <- regmatches(basename(hits[1]),
                regexec("_annual_([0-9]{4})_([0-9]{4})\\.nc$", basename(hits[1])))[[1]]
if (length(m) != 3)
  stop("Cannot parse the product year range from: ", basename(hits[1]))
FYnc <- as.integer(m[2]); LYnc <- as.integer(m[3])

ncdir  <- paste0(output_dir, "crop_calendars/ncdf/", gcm, "/", scen, "/")  # PHU .nc4 output
if (!dir.exists(ncdir)) dir.create(ncdir, recursive = TRUE)

# ------------------------------------------------------#
# Compute decadal PHUs and write one netCDF per crop x irrigation.

generatePHUTserie_isimip3(
    ncdir    = ncdir,
    gcm      = gcm,
    scen     = scen,
    cros     = cros,
    irris    = irris,
    FYnc     = FYnc,
    LYnc     = LYnc,
    grid_df  = grid_df,
    cal_dir  = cal_dir,
    soc_file = soc_file,
    ncores   = ncores
)

# ------------------------------------------------------#

cat("\n", paste("Computation PHUs ended!"),
    "-------------------------------------------------------", sep = "\n")

endtime <- Sys.time()
print(endtime)

print(endtime-starttime)
