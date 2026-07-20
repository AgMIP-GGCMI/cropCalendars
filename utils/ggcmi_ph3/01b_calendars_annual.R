# ---------------------------------------------------------------------------- #
# Step 01b (annual): rule-based crop calendars from the cached per-year
# climatology written by 01a. For each year, loads that year's climatology
# (~0.6 GB), runs calcCropCalendars for every crop x cell via mclapply, and
# writes the per-crop annual calendars (same format as the old unified stage 01).
#
# Because the heavy ring buffer is NOT live here (only one year's climatology is
# in the parent at fork time), the 64-worker fork is cheap (~tens of GB, no OOM),
# and this stage is re-runnable in minutes on every sowing/harvest RULE change
# WITHOUT re-reading the raw climate.
#
# Args: GCM SCENARIO [NCORES]
# Env : YEARS subset for dev iteration, e.g. YEARS="2000:2014" (default: every
#       year for which 01a wrote a climatology file). NB: a sparse subset breaks
#       the per-cell wettest-window hysteresis continuity (only relevant when
#       wet_window_eps > 0).
# ---------------------------------------------------------------------------- #

rm(list = ls(all.names = TRUE))
suppressMessages(library(parallel))
starttime <- Sys.time(); print(starttime)

work_dir <- getwd()
source(file.path(work_dir, "00_config.R"))

if (cluster_job == TRUE) { options(echo = FALSE); args <- commandArgs(trailingOnly = TRUE) } else {
  args <- c("GFDL-ESM4", "historical", "8")
}
print(args)
gcm    <- args[1]
scen   <- args[2]
ncores <- if (length(args) >= 3) as.integer(args[3]) else
          as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

if (!exists("wet_window_eps"))        wet_window_eps        <- 0
if (!exists("wet_window_decay"))      wet_window_decay      <- 0.3
if (!exists("cross_min_duration"))    cross_min_duration    <- 1L
if (!exists("cross_min_area"))        cross_min_area        <- 0
if (!exists("temp_cross_min_area"))   temp_cross_min_area   <- 0
if (!exists("smooth_window"))         smooth_window         <- 31L
if (!exists("seas_eps"))              seas_eps              <- 0
if (!exists("seas_mtemp_margin"))     seas_mtemp_margin     <- 1
if (!exists("harv_ppet_eps"))         harv_ppet_eps         <- 0
if (!exists("harv_exist_eps"))        harv_exist_eps        <- 0
if (!exists("harv_tmax_margin"))      harv_tmax_margin      <- 0
if (!exists("wet_near_min_area"))     wet_near_min_area     <- 0
if (!exists("winter_margin"))         winter_margin         <- 0
if (!exists("winter_cold_margin"))    winter_cold_margin    <- 0

# Directional wet-end EXISTENCE deadband (harv_exist_eps on the ppet_ratio doy_wet1 crossing)
# lives on the gated cc_regime code path; enable it iff the knob is on.
options(cc_regime = (harv_exist_eps > 0))

parse_years <- function(s) {
  if (is.null(s) || s == "") return(NULL)
  sort(unique(unlist(lapply(strsplit(s, ",")[[1]], function(p) {
    p <- trimws(p)
    if (grepl(":", p)) { ab <- as.integer(strsplit(p, ":")[[1]]); ab[1]:ab[2] } else as.integer(p)
  }))))
}
years_env <- parse_years(Sys.getenv("YEARS", ""))

# Optional env overrides for A/B validation: HARV_EXIST_EPS forces the existence-deadband
# knob (and re-derives cc_regime); OUT_SUFFIX writes to a sibling output dir so a baseline
# run does not clobber the production GCM dir.
if (Sys.getenv("HARV_EXIST_EPS") != "") {
  harv_exist_eps <- as.numeric(Sys.getenv("HARV_EXIST_EPS"))
  options(cc_regime = (harv_exist_eps > 0))
}
# Harvest threshold-crossing scans (wet-end + hot-day) are always anchored at sowing (the Jan-1
# scan is a bug that lets a spurious early-DOY crossing pre-empt the genuine first-after-sowing
# one); no toggle.
# WET_WINDOW_LO/HI set the always-on WET-NEAR GATE's hysteretic Schmitt window (defaults 10/20): the
# wet-season decision is taken only when the cell is wet within that window after the (temperature-set)
# sowing, else it falls to the ppet_min floor tier and then hd_first. (WET_NEAR_SUPPRESS is obsolete --
# the gate is now the default rule, not a toggle -- and is ignored if set.)
# USE_WET2 is obsolete -- the doy_wet2 TREND wet-end estimate was retired (see NEWS) -- and is ignored.
options(cc_wet_window_lo = if (Sys.getenv("WET_WINDOW_LO") != "") as.integer(Sys.getenv("WET_WINDOW_LO"))
                           else if (exists("wet_window_lo")) as.integer(wet_window_lo) else 10L)
options(cc_wet_window_hi = if (Sys.getenv("WET_WINDOW_HI") != "") as.integer(Sys.getenv("WET_WINDOW_HI"))
                           else if (exists("wet_window_hi")) as.integer(wet_window_hi) else 20L)
# CROSS_MIN_AREA: integrated-deficit (deficit-days) budget for the wet-end LEVEL crossing -- a
# grazing P/PET crossing must accumulate sum(ppet_ratio - daily_ppet) >= this to count (default 0).
# A HYSTERETIC (Schmitt) pair "lo,hi" damps wet-end existence flicker (prev-found uses lo, prev-absent
# uses hi); a single value means lo == hi (no hysteresis). e.g. CROSS_MIN_AREA="2,3".
if (Sys.getenv("CROSS_MIN_AREA") != "")
  cross_min_area <- as.numeric(strsplit(Sys.getenv("CROSS_MIN_AREA"), ",")[[1]])
# CROSS_MIN_DUR: min sustained-excursion days for the hot-day temperature crossings (the wet-end
# doy_wet1 is guarded by cross_min_area alone). Default 1 = off.
if (Sys.getenv("CROSS_MIN_DUR") != "") cross_min_duration <- as.integer(Sys.getenv("CROSS_MIN_DUR"))
# WET_NEAR_MIN_AREA: integrated-area budget for the wet-near gate (c(lo,hi) "lo,hi"); overrides config.
if (Sys.getenv("WET_NEAR_MIN_AREA") != "")
  wet_near_min_area <- as.numeric(strsplit(Sys.getenv("WET_NEAR_MIN_AREA"), ",")[[1]])
out_suffix <- Sys.getenv("OUT_SUFFIX", "")

clim_dir <- paste0(output_dir, "/crop_calendars/annual_climatology/", scen, "/", gcm, "/")
out_dir  <- paste0(output_dir, "/crop_calendars/annual/", scen, "/", gcm, out_suffix, "/")
if (!dir.exists(clim_dir)) stop("No climatology dir (run 01a first): ", clim_dir)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Discover the cached climatology years; intersect with the YEARS subset.
cfiles <- Sys.glob(file.path(clim_dir, sprintf("climatology_%s_%s_*.Rdata", gcm, scen)))
cyears <- as.integer(sub(".*_(\\d{4})\\.Rdata$", "\\1", cfiles))
o <- order(cyears); cfiles <- cfiles[o]; cyears <- cyears[o]
if (!is.null(years_env)) { keep <- cyears %in% years_env; cfiles <- cfiles[keep]; cyears <- cyears[keep] }
if (length(cyears) == 0) stop("No climatology files match (run 01a; check YEARS).")
emit_years <- cyears
nE <- length(emit_years)
cat(sprintf("\n%s %s | %d climatology years (%d..%d) cores=%d | wet_eps=%g wet_decay=%g cross_min_dur=%d cross_min_area=%s temp_cross_min_area=%s smooth_window=%d seas_eps=%g harv_exist_eps=%g harv_tmax_margin=%g wet_near_min_area=%s winter_margin=%g winter_cold_margin=%g wet_window=%d/%d\n",
            gcm, scen, nE, min(emit_years), max(emit_years), ncores, wet_window_eps, wet_window_decay, cross_min_duration, paste(cross_min_area, collapse="/"), paste(temp_cross_min_area, collapse="/"), smooth_window, seas_eps, harv_exist_eps, harv_tmax_margin, paste(wet_near_min_area, collapse="/"), winter_margin, winter_cold_margin,
            as.integer(getOption("cc_wet_window_lo", 10L)), as.integer(getOption("cc_wet_window_hi", 20L))))

# Crops + pre-extracted parameters. CROPS env (rb_cal names, comma-separated, e.g.
# "Maize") restricts the crop set for fast dev/validation runs (default: all).
crops <- as.character(stats::na.omit(crop_ls[["rb_cal"]]))
crops_env <- trimws(strsplit(Sys.getenv("CROPS", ""), ",")[[1]])
crops_env <- crops_env[nzchar(crops_env)]
if (length(crops_env) > 0) {
  miss <- setdiff(crops_env, crops); if (length(miss)) stop("Unknown CROPS: ", paste(miss, collapse=", "))
  crops <- crops_env
}
cat("crops:", paste(crops, collapse=", "), "\n")
cpfile  <- system.file("extdata", "crop_parameters.csv", package = "cropCalendars", mustWork = TRUE)
cparams <- lapply(crops, function(cr) getCropParam(crops = cr, cropparam_file = cpfile))
names(cparams) <- crops

SEAS_LEV <- c("NO_SEASONALITY", "PREC", "PRECTEMP", "TEMP", "TEMPPREC")
HARV_LEV <- c("hd_first", "hd_maxrp", "hd_last", "hd_wetseas", "hd_temp_base", "hd_temp_opt")
FLDS     <- c("sow", "ss", "seas", "dflag", "maty_rf", "maty_ir",
              "gp_rf", "gp_ir", "hr_rf", "hr_ir")

# One year of calendars for all cells x crops (identical to the unified driver).
computeYear <- function(clim, prev_wet, prev_seas, prev_harv, prev_winter) {
  res <- mclapply(seq_len(NCELLS), function(j) {
    mcl <- list(dtemp = clim$dtemp[j, ], dprec = clim$dprec[j, ], dpet = clim$dpet[j, ])
    M <- matrix(NA_real_, length(crops), length(FLDS), dimnames = list(NULL, FLDS))
    wd <- NA_integer_; st <- NA_character_; hv <- rep(NA_integer_, length(crops)); wc <- rep(NA_integer_, length(crops))
    for (ci in seq_along(crops)) {
      r <- calcCropCalendars(lon = land_lon[j], lat = land_lat[j], mclimate = mcl,
                             crop_parameters = cparams[[ci]],
                             prev_wet_doy = prev_wet[j], wet_window_eps = wet_window_eps,
                             wet_window_decay = wet_window_decay,
                             cross_min_duration = cross_min_duration,
                             cross_min_area = cross_min_area,
                             temp_cross_min_area = temp_cross_min_area,
                             wet_near_min_area = wet_near_min_area,
                             smooth_window = smooth_window,
                             prev_seas = prev_seas[j], seas_eps = seas_eps,
                             seas_mtemp_margin = seas_mtemp_margin,
                             prev_harv = prev_harv[j, ci], harv_ppet_eps = harv_ppet_eps,
                             harv_exist_eps = harv_exist_eps,
                             harv_tmax_margin = harv_tmax_margin,
                             prev_winter = prev_winter[j, ci],
                             winter_margin = winter_margin,
                             winter_cold_margin = winter_cold_margin)
      if (ci == 1L) { wd <- attr(r, "wet_doy"); st <- attr(r, "seas_type") }
      hv[ci] <- attr(r, "harv_state")     # crop-DEPENDENT harvest hysteresis state
      wc[ci] <- attr(r, "winter_regime")  # crop-DEPENDENT winter-regime hysteresis state
      M[ci, ] <- c(r$sowing_doy[1],
                   ifelse(r$sowing_season[1] == "winter", 1, 2),
                   match(r$seasonality_type[1], SEAS_LEV),
                   ifelse(r$sowing_month[1] == 0, 0, 1),
                   r$maturity_doy[1], r$maturity_doy[2],
                   r$growing_period[1], r$growing_period[2],
                   match(r$harvest_reason[1], HARV_LEV),
                   match(r$harvest_reason[2], HARV_LEV))
    }
    list(M = M, wd = wd, st = st, hv = hv, wc = wc)
  }, mc.cores = ncores)
  if (any(vapply(res, function(x) !is.list(x) || !is.matrix(x$M), logical(1))))
    stop("computeYear: a worker failed (retry with ncores = 1 to see the error).")
  list(arr = simplify2array(lapply(res, `[[`, "M")),
       wet = vapply(res, function(x) x$wd, integer(1)),
       seas = vapply(res, function(x) x$st, character(1)),
       harv = t(vapply(res, `[[`, integer(length(crops)), "hv")),      # [cells x crops]
       winter = t(vapply(res, `[[`, integer(length(crops)), "wc")))    # [cells x crops]
}

# ------------------------------------ #
# Load grid from the first climatology file to define the cell set.
e1 <- new.env(); load(cfiles[1], envir = e1)
grid_clm <- e1$grid_clm; land_lon <- grid_clm$lon; land_lat <- grid_clm$lat
NCELLS   <- nrow(grid_clm); rm(e1)
cat("GGCMI land cells:", NCELLS, "\n")

OUT <- lapply(crops, function(.) { z <- lapply(FLDS, function(.) matrix(NA_real_, NCELLS, nE)); names(z) <- FLDS; z })
names(OUT) <- crops
store <- function(e, arr) for (ci in seq_along(crops)) for (fi in seq_along(FLDS))
  OUT[[ci]][[FLDS[fi]]][, e] <<- arr[ci, fi, ]

prev_wet    <- rep(NA_integer_, NCELLS)
prev_seas   <- rep(NA_character_, NCELLS)
prev_harv   <- matrix(NA_integer_, NCELLS, length(crops))   # crop-dependent harvest state
prev_winter <- matrix(NA_integer_, NCELLS, length(crops))   # crop-dependent winter-regime state
t_all <- Sys.time()
for (e in seq_len(nE)) {
  t0 <- Sys.time()
  ce <- new.env(); load(cfiles[e], envir = ce); clim <- ce$clim; rm(ce)
  cy <- computeYear(clim, prev_wet, prev_seas, prev_harv, prev_winter)
  prev_wet <- cy$wet; prev_seas <- cy$seas; prev_harv <- cy$harv; prev_winter <- cy$winter
  store(e, cy$arr)
  rm(clim)
  tot <- as.numeric(Sys.time() - t_all, units = "mins")
  cat(sprintf("[%s]  year %d/%d (%d): %.1fs  (rss %.1f GB, elapsed %.1f min, eta %.1f min)\n",
              format(Sys.time(), "%H:%M:%S"), e, nE, emit_years[e],
              as.numeric(Sys.time() - t0, units = "secs"), as.numeric(gc()[2, 2]) / 1024,
              tot, tot / e * (nE - e)))
}

# ------------------------------------ #
for (ci in seq_along(crops)) {
  cal  <- OUT[[ci]]
  crop <- crops[ci]
  fn <- paste0(out_dir, "annual_calendar_", crop, "_", gcm, "_", scen, "_",
               min(emit_years), "_", max(emit_years), ".Rdata")
  save(grid_clm, emit_years, cal, gcm, scen, crop, file = fn)
  cat("saved", fn, "\n")
}
endtime <- Sys.time(); print(endtime); print(endtime - starttime)
