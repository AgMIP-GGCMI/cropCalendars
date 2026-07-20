# ---------------------------------------------------------------------------- #
# Configuration for GGCMI phase3 (ISIMIP3b climate)

# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
# ---------------------------------------------------------------------------- #

library(ncdf4)
library(abind)
library(data.table)
library(foreach)
library(cropCalendars)
library(zoo)           # for rolling mean
#library(unix)

# ------------------------------------ #
# General Settings

# Deployment paths and SLURM account live in settings.sh, so the .sh job scripts
# and this config share a single source of truth. Parse the KEY=VALUE lines here.
settings_file <- if (exists("work_dir")) file.path(work_dir, "settings.sh") else "settings.sh"
.settings <- local({
  lines <- readLines(settings_file)
  lines <- lines[grepl("^[A-Za-z_][A-Za-z0-9_]*=", lines)]   # keep KEY=VALUE lines
  keys  <- sub("=.*$", "", lines)
  raw   <- sub("^[^=]*=", "", lines)
  raw   <- sub("[[:space:]]+#.*$", "", raw)                  # strip trailing inline comment
  vals  <- gsub("(^[\"']|[\"']$)", "", trimws(raw))
  setNames(as.list(vals), keys)
})

# Fail loudly if a required deployment key is absent: a missing key would otherwise
# be NULL and either crash (strsplit on CLIMATE_DIR) or silently collapse in paste0
# to a root/relative path. Validate here, at the single config source of truth.
.required_settings <- c("OUTPUT_DIR", "CLIMATE_DIR", "ISIMIP3B_PATH",
                        "AGMIP_DIR", "GRID_BIN")
.missing_settings  <- setdiff(.required_settings, names(.settings))
if (length(.missing_settings) > 0)
  stop("settings.sh (", settings_file, ") is missing required key(s): ",
       paste(.missing_settings, collapse = ", "))

# Output directory: where output data are going to be saved
output_dir <- .settings$OUTPUT_DIR

parallel     <- TRUE
cluster_job  <- TRUE
plot_results <- TRUE

# ------------------------------------ #
# Tunable parameters (annual sliding-window pipeline). The env vars EMIT_STEP and
# PHU_SMOOTH_WINDOW override clm_emit_step / phu_smooth_window for ad-hoc runs.
clm_avg_years     <- 30       # climate-averaging window length (years)
clm_emit_step     <- 1        # compute calendars every N years (1 = fully annual)
pet_method        <- "fao56"  # PET: "fao56" (Penman-Monteith) or "pt" (Priestley-Taylor)
phu_smooth_window <- 1        # PHU temperature-averaging window (years; 1 = period-exact)
# Threshold-crossing noise filters (a matched pair). They attack the year-to-year
# sowing/harvest oscillation at its source: the per-DOY daily climatology
# (dtemp/dprec/dpet) carries ~1 degC / spiky day-to-day jitter, and the point
# detectors (calcDoyCrossThreshold: spring/fall temperature + wet-season-end
# crossings) otherwise latch onto a single-day blip -> a spurious crossing ~130 days
# off (e.g. a 1-day dip through temp_spring=14 in the autumn descent). The two knobs:
#   smooth_window      : the SINGLE global day-window the daily climatologies are read
#       through -- both the crossing detectors AND the window/extremum reductions
#       (warmest/coldest-month temperature, driest-month P/PET).
#       Set to 31 (~1 month, ODD) so the reductions stay monthly-equivalent AND the
#       crossings share the same smoothing (previously crossings used 15, reductions a
#       structural 30; unified here). ODD makes the centred smoothing/argmax exactly
#       symmetric -- an EVEN window centres half a day low (see .circRoll's caveat).
#       The 120-day wettest window (calcDoyWetMonth) keeps its own fixed window.
#   cross_min_duration : a crossing must stay on the new side of the threshold for
#       at least this many days to count (1 = off).
smooth_window       <- 31L    # global daily-climatology smoothing window (days; odd = symmetric)
cross_min_duration  <- 5L     # min sustained-excursion days for the hot-day temperature crossings (1 = off;
                              # the doy_wet1 LEVEL crossing is now guarded by cross_min_area alone)
cross_min_area      <- c(2,6) # wet-end LEVEL crossing deficit-days budget, HYSTERETIC c(lo,hi): prev-found uses
                              # lo (stay found), prev-absent uses hi (become found) -> damps existence flicker.
                              # Widened lo/hi band 2/3 -> 2/6 (strict hi=6 to flip INTO found; lenient lo=2 to stay):
                              # trades some decadal trend-tracking for less wet-end existence flicker (accepted).
temp_cross_min_area <- c(2,8) # reproductive hot-day crossing (hd_temp_opt) degree-days budget, HYSTERETIC c(lo,hi),
                              # temperature analogue of cross_min_area. Damps hd_temp_opt EXISTENCE flicker where the
                              # warm plateau grazes temp_opt_rphase (subtropical/continental WW: Botswana, Mediterranean,
                              # Central Asia, N/NE China). 2/8 (strict hi=8 to flip INTO found; lenient lo=2 to stay) --
                              # genuinely-warm cells (area >> 8) unaffected. Rice's spring-crop down-crossing (large warm
                              # season) does not respond -- that residual was the floor tier, not topt. 0 = off.

# Wettest-window hysteresis (distance-weighted, max-normalised selection). For the
# ~57% of PREC/PRECTEMP cells with a near-tied second 120-day P/PET peak, the plain
# argmax flips between far-apart peaks year to year. The new rule picks
# argmax( (ws/max ws) * (1 - eps*dist_to_last_year/182.5) ): near peaks essentially
# free (tracks drift), far peaks must be decisively better to win. eps=0.5 cut the
# wet-cell mean year-to-year jump 1.59 -> 0.13 and cells-ever-flipping 0.257 -> 0.077.
wet_window_eps    <- 0.5      # wettest-window weight FLOOR = 1-eps (0 = plain argmax); far peak must be >1/(1-eps)x wetter to win
wet_window_decay  <- 0.3      # Gaussian decay scale of the distance weight (units of half-year; smaller = stickier)

# WET-NEAR GATE hysteretic Schmitt window (days after sowing scanned for the wet-season decision; the
# package option default is 10/20). 20/40 is the validated config -- locked here so no launch path can
# silently fall back to 10/20. Applied as the default in 01b (WET_WINDOW_LO/HI env still overrides for A/B).
wet_window_lo     <- 20L
wet_window_hi     <- 40L

# Seasonality-classifier hysteresis (threshold deadband). ~18% of cells flip their
# seasonality CLASS year to year (grazing the CV_prec/CV_temp/min_temp thresholds),
# which swaps the whole sowing rule. seas_eps relaxes the relative CV thresholds toward
# keeping last year's class (thermostat deadband); seas_mtemp_margin is the absolute
# deadband (deg C) on the min-temp threshold. seas_eps=0.25 cut class flips 18.3% -> 2.8%.
seas_eps          <- 0.25     # seasonality CV-threshold deadband (rel.; 0 = off)
seas_mtemp_margin <- 1        # seasonality min-temp threshold deadband (deg C)

# Harvest-rule hysteresis. Harvest dates flip more than sowing because several thresholds in
# the harvest rule are grazed by the sliding-window climatology. The wet-season decision is now a
# TWO-TIER rule organised by state at sowing (calcHarvestDateVector), with all its de-flickering
# built into the package and driven by two Schmitt mechanisms, NOT by the old deadbands:
#   * Wet-end ESCAPE wrap oscillating around sowing (the old dominant ~50% of flips): now self-healed
#     by the always-on WET-NEAR GATE (an end just before/after sowing both land on hd_first), window
#     WET_WINDOW_LO/HI (10/20).
#   * Wet-end EXISTENCE flicker (doy_wet1 FOUND <-> ABSENT): now damped by the cross_min_area Schmitt
#     c(lo,hi) above (a deficit-days budget hysteresis -- monotonic in dip depth, so unlike the old
#     level-threshold nudge it cannot erase the crossing it keeps). This SUPERSEDES harv_exist_eps
#     (the cc_regime path) as the default existence de-flicker.
#   * The old always-wet test (min_ppet vs ppet_min) and its harv_ppet_eps deadband are RETIRED: the
#     no-wet-end hd_last/hd_first decision is now the persistence-guarded two-tier rule (wet-near gate,
#     then the ppet_min floor tier with the same machinery). harv_ppet_eps is ignored.
#   * harv_tmax_margin (thermal-class deadband) gave ~0% historically (masked by moisture flicker).
#     With the moisture flicker removed (doy_wet2 retired, two-tier rule), the thermal-class boundary
#     is now a leading flicker source: warmest_t grazing temp_base/temp_opt flips the t-low/mid/high
#     class -> the whole harvest formula (hd_first<->hd_temp_base<->hd_temp_opt, ~16k cell-years). The
#     1 deg C deadband (mirroring seas_mtemp_margin) stabilises the class hysteretically; a replay
#     hotspot (Winter_Wheat 32.25/-19.75, warmest_t ~25 = temp_opt) drops 6->3 big jumps (the residual
#     is a separate within-rule crossing-date wobble). Enabled.
harv_ppet_eps     <- 0         # RETIRED/ignored (was always-wet test deadband)
harv_exist_eps    <- 0         # superseded by the cross_min_area Schmitt (cc_regime path off)
harv_tmax_margin  <- 1         # harvest-rule temp_max threshold deadband (deg C; 0 = off) -- thermal-class thermostat
wet_near_min_area <- c(2,5)    # wet-near gate INTEGRATED budget c(lo,hi) HYSTERETIC (0 = off): the gate fires when
                               # sum(daily_ppet[window]-ppet_ratio) over above-threshold days >= budget, instead of
                               # any-day-above (a single-day max that flips on the peak grazing the threshold). Reads a
                               # thin touch (area~0) as NOT wet -> latches the TIER1<->TIER2 selector. Big harvest win:
                               # NE-China Maize monsoon -84%, neutral on the broad population; complements (does NOT
                               # replace) the 20/40 window-WIDTH hysteresis (tested -- fixed window regresses the monsoon).
                               # Keyed on prev_wet_near (no extra state). 2/5 paired with the ppet_min floor Schmitt.

# Winter-regime sowing thermostat (winter-type / Winter_Wheat only). The winter regime is classified
# warm / mild / cold by two coldest_t thresholds -- the warm boundary (basetemp.low) and the cold
# boundary (-10 C) -- and each regime picks a different autumn-sowing anchor. When coldest_t grazes
# either boundary the sowing flips ~half a year between adjacent years (the dominant winter-wheat
# sowing-flicker mode; a replay cell at Quebec -72.25/45.25 toggles 264<->107 on a 0.2 deg C wobble at
# the -10 boundary). The 1 deg C deadband (mirroring seas_mtemp_margin / harv_tmax_margin) carries last
# year's regime and relaxes BOTH boundaries toward it (2*margin = 2 deg C sticky band each), latching
# the cell into one regime. Enabled.
winter_margin      <- 1        # WARM winter boundary (basetemp.low) deadband (deg C; 0 = off)
winter_cold_margin <- 2        # COLD winter boundary (-10 C) deadband (deg C; 0 = off); decoupled, set to 2 to fully latch the Russia continental autumn<->spring WW flip (measurement suggested ~2)

# Spring up-crossing depth gate REMOVED (was spring_offset/spring_margin). The temp_spring up-crossing is
# now always scanned forward from the centroid coldest_doy; when the trough never dips below temp_spring
# there is no crossing and the mean-anchored fallback (mean above temp_spring -> coldest_doy ~Jan; below
# -> warmest_doy) resolves the placeholder -- the same DOY a gated-off cell landed on, so the gate was a
# no-op once the fallback became mean-based. See calcSowingDate.R.

climate_dirs  <- sub("/+$", "", strsplit(.settings$CLIMATE_DIR, ":")[[1]])  # search list
climate_dir   <- climate_dirs[1]                                            # legacy (stage 01a)
isimip3b.path <- .settings$ISIMIP3B_PATH # .clm climate (ESM scenarios)
isimip3a.path <- .settings$ISIMIP3A_PATH # .clm climate (obsclim/spinclim/counterclim)
agmip_dir     <- .settings$AGMIP_DIR     # AgMIP reference crop calendars (used in 02)
grid_file     <- .settings$GRID_BIN      # LPJmL grid path; read only in 03 to map product cells
                                         # onto the LPJmL grid order (03 writes netCDF, not .clm)

# Climate input forcings (ESMs + the GSWP3-W5E5 observational forcing).
gcms <- c(
  "GSWP3-W5E5",
  "GFDL-ESM4",
  "IPSL-CM6A-LR",
  "MPI-ESM1-2-HR",
  "MRI-ESM2-0",
  "UKESM1-0-LL"
)

# Scenarios available per forcing -- the (GCM x scenario) processing matrix.
# ssp534-over (SSP5-3.4 overshoot, 2040-2100) is available for IPSL/MRI/UKESM only; its
# pre-2040 sliding window is gap-filled from ssp585 (see 01a_climatology_annual.R).
# NB: UKESM1-0-LL ssp119 lacks bias-adjusted huss + ps, so the launcher SC matrices run
# ssp119 for IPSL/MRI only. It is kept here because it WOULD run under Priestley-Taylor PET
# (pet_method = "pt", which needs only temperature + radiation); the default FAO-56
# Penman-Monteith needs huss/ps/wind, so UKESM ssp119 is excluded under the default.
scenarios <- list(
  "GSWP3-W5E5"    = c("spinclim", "obsclim"),
  "GFDL-ESM4"     = c("historical", "ssp126", "ssp245", "ssp370", "ssp585"),
  "IPSL-CM6A-LR"  = c("historical", "ssp119", "ssp126", "ssp245", "ssp370", "ssp460", "ssp585", "ssp534-over"),
  "MPI-ESM1-2-HR" = c("historical", "ssp126", "ssp245", "ssp370", "ssp585"),
  "MRI-ESM2-0"    = c("historical", "ssp119", "ssp126", "ssp245", "ssp370", "ssp460", "ssp585", "ssp534-over"),
  "UKESM1-0-LL"   = c("historical", "ssp119", "ssp126", "ssp245", "ssp370", "ssp585", "ssp534-over")  # ssp119: PT-PET only
)
# Union of all scenarios, used to build the file-window lists below.
scens <- sort(unique(unlist(scenarios, use.names = FALSE)))

# ISIMIP3a observational-climate scenarios (vs ISIMIP3b ESM scenarios). Stage 02/03
# branch the DRS publish path on these: any forcing run under one of these scenarios
# (GSWP3-W5E5, 20CRv3-W5E5, ... -- the gcm stays a variable token) publishes under
# ISIMIP3a/.../crop_calendar/<soc>/ with soc = histsoc (obsclim/spinclim) or
# countersoc (counterclim), rather than the ISIMIP3b <gcm>/<soc> layout.
isimip3a_scenarios <- c("obsclim", "spinclim", "counterclim")

# NB: ensemble members and the per-scenario climate-file year windows are no longer
# hardcoded. The annual pipeline (01_compute_annual_calendars.R) discovers the
# climate files by globbing <CLIMATE_DIR>/<scenario>/<gcm>/ and reads the ensemble
# member, scenario tag and year range straight from the file names, so new datasets
# or scenarios need no config edits. (The legacy stage 01a -- now superseded by
# 01_compute_annual_calendars.R -- was the only consumer of enms/syears/eyears.)

# NB: the LPJmL grid is NOT read here. 01a/01b/02 work on the climate land mask; only
# stage 03 (writing the .clm files) needs the LPJmL grid, and reads it there.

# ------------------------------------------------------#
# Crop Names: ----
crop_ls <- list(all_low = c("winter_wheat", "spring_wheat", "maize", "rice1", "rice2",
                            "soybean", "millet", "sorghum","peas","sugar_beat",
                            "cassava","rape_seed","sunflower","nuts","sugarcane"),
                rb_cal  = c("Winter_Wheat", "Spring_Wheat", "Maize", "Rice", NA,
                            "Soybean", "Millet", "Sorghum", NA, NA,
                            NA, NA, NA, NA, NA),
                ggcmi   = c("wwh","swh","mai","ri1","ri2",
                            "soy","mil","sor","pea","sgb",
                            "cas","rap","sun","nut","sgc"),
                # vernal: yes_all = vern. forced in all grid cells;
                #         yes = only if conditions are met, see wintercrop()
                vernal  = c("yes_all","no","no","no","no",
                            "no","no","no","no","no",
                            "no","yes","no","no","no"))

irri_ls <- list(all_low = c("rainfed", "irrigated"),
                rb_cal  = c("Rainfed", "Irrigated"),
                ggcmi   = c("rf", "ir"))

# ------------------------------------ #
cat("\nConfigs imported.\n")