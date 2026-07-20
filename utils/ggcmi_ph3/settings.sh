# ---------------------------------------------------------------------------- #
# Deployment settings — single source of truth for this pipeline copy.
#
# Sourced by the *.sh job scripts (native bash) and parsed by 00_config.R for
# the R side. Use plain KEY=VALUE with no spaces around "=".
#
# Keys read by R (OUTPUT_DIR, CLIMATE_DIR, ISIMIP3B_PATH, AGMIP_DIR) must be
# LITERAL paths — the R parser does not expand shell variables. Bash-only keys
# further down (NCDF_DIR, PUBLISH_DIR) may reference earlier keys; they are only
# `source`d by bash, where the expansion happens.
#
# When moving this pipeline to a new location, edit THIS file only.
# ---------------------------------------------------------------------------- #

# Working directory: where these pipeline scripts live (no trailing slash).
WD=/p/projects/macmit/users/heinke/crop_calendars/cropCalendars/utils/ggcmi_ph3

# SLURM account for sbatch (-A).
ACCOUNT=landuse

# Output root: where results are written (keep trailing slash).
OUTPUT_DIR=/p/projects/macmit/users/heinke/crop_calendars/output/

# Climate input roots, read-only. Colon-separated SEARCH LIST (no trailing slash):
# the annual driver discovers each <root>/<scenario>/<gcm>/*_<var>_global_daily_*.nc
# and uses the first root that has the files. Official ISIMIP roots only:
#   ISIMIP3b primary + secondary  -> the ESMs (all SSPs, historical, picontrol);
#   ISIMIP3a obsclim + spinclim   -> the observational forcings (GSWP3-W5E5 etc.):
#     scenarios are named directly "obsclim"/"spinclim" (files under "historical").
# The GGCMI land mask (ggcmi_landcells.csv, 67420 cells) is applied uniformly, so the
# full-grid official files are subset to the same cells (the ISIMIP no-ant mask is a
# subset of these, and GSWP3-W5E5 covers all 67420).
CLIMATE_DIR=/p/projects/isimip/isimip/ISIMIP3b/InputData/climate/atmosphere/bias-adjusted/global/daily:/p/projects/isimip/isimip/ISIMIP3b/SecondaryInputData/climate/atmosphere/bias-adjusted/global/daily:/p/projects/isimip/isimip/ISIMIP3a/InputData/climate/atmosphere/obsclim/global/daily:/p/projects/isimip/isimip/ISIMIP3a/InputData/climate/atmosphere/spinclim/global/daily

# ISIMIP3b .clm climate path used by generatePHUTserie_isimip3 (keep trailing slash).
ISIMIP3B_PATH=/p/projects/lpjml/input/scenarios/ISIMIP3bv2/

# ISIMIP3a .clm climate path (obsclim/spinclim/counterclim forcings; keep trailing slash).
ISIMIP3A_PATH=/p/projects/lpjml/input/historical/ISIMIP3av2/

# LPJmL grid file (defines the 67420 land cells / their lon-lat), read by 00_config.R.
GRID_BIN=/p/projects/lpjml/input/historical/input_VERSION2/grid.bin

# AgMIP reference crop-calendar input, read-only (keep trailing slash). Stage 02
# (02_assemble_annual_ncdf.R) reads it for the observed GGCMI default sowing/harvest
# dates used to fill cells without a rule-based calendar.
AGMIP_DIR=/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar/

# --- Post-processing trees (04–07), derived from OUTPUT_DIR; bash-only ---------
# ncdf working tree written by step 2/3 (source for 04).
NCDF_DIR=${OUTPUT_DIR}crop_calendars/ncdf
# Published ISIMIP3b output tree: 04 renames into it; 05/06/07 fix files in it.
PUBLISH_DIR=${OUTPUT_DIR}ISIMIP3b/InputData/socioeconomic/crop_calendar
