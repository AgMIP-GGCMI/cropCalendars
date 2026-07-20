# ---------------------------------------------------------------------------- #
# Step 01: annual sliding-window crop calendars (replaces 01a + 01b)
#
# Streams the raw daily climate ONE year at a time into a `window`-year ring
# buffer (initClimateRing/pushClimateYear), and for each target year T emits the
# 30-yr climatology of [T-window, T-1] (anchored to the first full block for the
# early years) and computes the rule-based calendars for every crop. Each raw
# year is read exactly once.
#
# Output: per crop, an annual calendar (per-cell x per-year fields) consumed by
# the simplified stage 02. No 10-year tiling, jump-filtering or output moving
# average — the annual series is smooth by construction and rule-consistent.
#
# Args: GCM SCENARIO [NCORES]
# Env : EMIT_STEP (default from config), PROBE_NEMIT (compute only the first N
#       emit years, for cost/correctness probing)
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

# Window length (config) and emit step (env overrides config default of 1).
W         <- clm_avg_years
emit_step <- as.integer(Sys.getenv("EMIT_STEP", as.character(clm_emit_step)))  # config default
probe_n   <- as.integer(Sys.getenv("PROBE_NEMIT", "0"))   # 0 = full run
# Wettest-window hysteresis (config). wet_window_eps (0 = off) is the distance-weighted
# selection strength that suppresses near-tie argmax flips in semi-arid / monsoon-fringe
# PREC/PRECTEMP cells (see ?calcDoyWetMonth).
# Source-level oscillation fix (config): smooth the daily climatology and require
# threshold crossings to persist before they count (see 00_config.R).
if (!exists("wet_window_eps"))        wet_window_eps        <- 0
if (!exists("wet_window_decay"))      wet_window_decay      <- 0.3
if (!exists("smooth_window"))         smooth_window         <- 31L
if (!exists("cross_min_duration"))    cross_min_duration    <- 1L
if (!exists("seas_eps"))              seas_eps              <- 0
if (!exists("seas_mtemp_margin"))     seas_mtemp_margin     <- 1
if (!exists("harv_ppet_eps"))         harv_ppet_eps         <- 0
if (!exists("harv_exist_eps"))        harv_exist_eps        <- 0
if (!exists("harv_tmax_margin"))      harv_tmax_margin      <- 0
if (!exists("cross_min_area"))        cross_min_area        <- 0
if (!exists("temp_cross_min_area"))   temp_cross_min_area   <- 0
if (!exists("wet_near_min_area"))     wet_near_min_area     <- 0
if (!exists("winter_margin"))         winter_margin         <- 0
if (!exists("winter_cold_margin"))    winter_cold_margin    <- 0

# Wet-near gate Schmitt window: adopt the locked config 20/40 (the package default is 10/20) by
# setting the options the rule reads, so this streaming driver matches the cached 01b driver.
options(cc_wet_window_lo = if (exists("wet_window_lo")) as.integer(wet_window_lo) else 10L,
        cc_wet_window_hi = if (exists("wet_window_hi")) as.integer(wet_window_hi) else 20L)

out_dir <- paste0(output_dir, "/crop_calendars/annual/", scen, "/", gcm, "/")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------ #
# Climate file list — discovered by globbing the dataset roots; the file names encode
# the ensemble member, scenario tag and year range, so nothing is hardcoded. Handles
# the ISIMIP3b ESM layout <root>/<scenario>/<gcm>/ AND the ISIMIP3a observational
# layout: the obsclim/spinclim roots store files under a "historical" subdir, and the
# scenario is named directly ("obsclim"/"spinclim"), so it is matched to its root.
vars <- c("tas", "pr", "rsds", "rlds", "huss", "sfcwind", "ps")
file_year_range <- function(fnames) {
  m <- regmatches(fnames, regexpr("[0-9]{4}_[0-9]{4}\\.nc$", fnames))
  list(fy = as.integer(substr(m, 1, 4)), ly = as.integer(substr(m, 6, 9)))
}
discover_clm <- function(scenario, var) {       # first climate root that has the files
  for (root in climate_dirs) {
    subdir <- if (grepl("/obsclim/", root)) {
                if (scenario == "obsclim") "historical" else next
              } else if (grepl("/spinclim/", root)) {
                if (scenario == "spinclim") "historical" else next
              } else scenario                                   # standard ISIMIP3b
    f <- Sys.glob(file.path(root, subdir, gcm, paste0("*_", var, "_global_daily_*.nc")))
    if (length(f) > 0) return(sort(f))
  }
  character(0)
}

# Scenario-own files define the product period; SSPs additionally prepend the
# historical files (the 30-yr seed window precedes the 2015 scenario start).
seeded <- grepl("^ssp", scen)
clm_file_list <- list()
for (v in vars) {
  fv <- discover_clm(scen, v)
  if (length(fv) == 0) stop("No climate files for ", gcm, " / ", scen, " / ", v)
  clm_file_list[[v]] <- if (seeded) c(discover_clm("historical", v), fv) else fv
}
scen_rng <- file_year_range(discover_clm(scen, "tas"))   # product period (scenario only)
Y0 <- min(scen_rng$fy); Yend <- max(scen_rng$ly)
cat(sprintf("\n%s %s | window=%d emit_step=%d cores=%d | product %d-%d%s\n",
            gcm, scen, W, emit_step, ncores, Y0, Yend,
            if (seeded) " (seeded from historical)" else ""))
read_year_cells <- function(fnames, yr, conv = identity) {
  rng <- file_year_range(fnames); wi <- which(rng$fy <= yr & rng$ly >= yr)[1]
  if (is.na(wi)) stop("No climate file covering year ", yr)
  dates_f <- seqDates(paste0(rng$fy[wi], "-01-01"), paste0(rng$ly[wi], "-12-31"), "day")
  pos <- which(as.integer(substr(dates_f, 1, 4)) == yr)
  arr <- cropCalendars::readNcdf(fnames[wi], dim_subset = list(time = pos[1]:pos[length(pos)]),
                                 index_dims = "time")
  conv(matrix(arr, nrow = dim(arr)[1] * dim(arr)[2])[cell_lin, , drop = FALSE])
}

# Cell set = the fixed GGCMI 67420-cell land mask (pipeline CSV). Mapping it to grid
# indices works for the pre-masked land-only copy AND the full-grid official files,
# so the product covers the same cells regardless of which climate root is used.
arr0     <- cropCalendars::readNcdf(clm_file_list[["tas"]][1], dim_subset = list(time = 1L),
                                    index_dims = "time")
lon_axis <- as.numeric(dimnames(arr0)[[1]]); lat_axis <- as.numeric(dimnames(arr0)[[2]])
nlon     <- length(lon_axis); rm(arr0)
lc   <- read.csv(file.path(work_dir, "ggcmi_landcells.csv"))
ilon <- match(round(lc$lon, 2), round(lon_axis, 2))
ilat <- match(round(lc$lat, 2), round(lat_axis, 2))
if (anyNA(ilon) || anyNA(ilat)) stop("Land-cell lon/lat not present on the climate grid.")
cell_lin <- ilon + (ilat - 1L) * nlon
NCELLS   <- length(cell_lin)
land_lon <- lc$lon; land_lat <- lc$lat
cat("GGCMI land cells:", NCELLS, "\n")

# ------------------------------------ #
# Crops + pre-extracted parameters (read the CSV ONCE per crop, not per cell).
crops <- as.character(stats::na.omit(crop_ls[["rb_cal"]]))   # rule-based crops, from config
cpfile  <- system.file("extdata", "crop_parameters.csv", package = "cropCalendars", mustWork = TRUE)
cparams <- lapply(crops, function(cr) getCropParam(crops = cr, cropparam_file = cpfile))
names(cparams) <- crops

SEAS_LEV <- c("NO_SEASONALITY", "PREC", "PRECTEMP", "TEMP", "TEMPPREC")
HARV_LEV <- c("hd_first", "hd_maxrp", "hd_last", "hd_wetseas", "hd_temp_base", "hd_temp_opt")
FLDS     <- c("sow", "ss", "seas", "dflag", "maty_rf", "maty_ir",
              "gp_rf", "gp_ir", "hr_rf", "hr_ir")

# One year of calendars for all cells x crops. `prev_wet` is the per-cell
# wettest-window DOY from the previously computed year (NA on the first call),
# carrying the eps-hysteresis state (the wettest-window argmax is crop-independent,
# so the state is per-cell, not per-crop). Returns a list:
#   arr = field array [ncrops x nflds x ncells]
#   wet = per-cell resolved wettest-window DOY to feed in as next year's prev_wet.
computeYear <- function(clim, prev_wet, prev_seas, prev_harv, prev_winter) {
  # NB: do NOT call gc() here. A full collection right before the mclapply fork,
  # with the large parent heap live (ring buffer + preallocated OUT), doubled the
  # per-worker copy-on-write footprint (168 GB -> 340 GB at 64 cores) and OOM'd.
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
      if (ci == 1L) { wd <- attr(r, "wet_doy"); st <- attr(r, "seas_type") }   # crop-independent
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
  list(arr = simplify2array(lapply(res, `[[`, "M")),               # [ncrops x nflds x ncells]
       wet = vapply(res, function(x) x$wd, integer(1)),
       seas = vapply(res, function(x) x$st, character(1)),
       harv = t(vapply(res, `[[`, integer(length(crops)), "hv")),    # [cells x crops]
       winter = t(vapply(res, `[[`, integer(length(crops)), "wc")))  # [cells x crops]
}

# ------------------------------------ #
# Emit years (anchored block uses the first full window). PROBE truncates.
emit_years <- sort(unique(c(seq(Y0, Yend, by = emit_step), Yend)))
if (probe_n > 0) emit_years <- head(emit_years, probe_n)
nE <- length(emit_years)
OUT <- lapply(crops, function(.) { z <- lapply(FLDS, function(.) matrix(NA_real_, NCELLS, nE)); names(z) <- FLDS; z })
names(OUT) <- crops
store <- function(e, arr) for (ci in seq_along(crops)) for (fi in seq_along(FLDS))
  OUT[[ci]][[FLDS[fi]]][, e] <<- arr[ci, fi, ]

read_push <- function(ring, yr) {
  pushClimateYear(ring,
    temp = read_year_cells(clm_file_list[["tas"]], yr, k2deg),
    prec = read_year_cells(clm_file_list[["pr"]], yr, function(x) 86400 * x),
    dates = seqDates(paste0(yr, "-01-01"), paste0(yr, "-12-31"), "day"),
    swdown = read_year_cells(clm_file_list[["rsds"]], yr),
    lwdown = read_year_cells(clm_file_list[["rlds"]], yr),
    windspeed = read_year_cells(clm_file_list[["sfcwind"]], yr),
    humid = read_year_cells(clm_file_list[["huss"]], yr),
    ps = read_year_cells(clm_file_list[["ps"]], yr), lat = land_lat)
}

# Window for product year T is [T-W, T-1]. data_start = earliest available climate
# year (historical reaches back to 1850 / picontrol to its first file). Future
# scenarios therefore SEED the ring from historical climate (e.g. 1985-2014 for a
# 2015 start) and switch to scenario climate as the window advances. Anchor to the
# first full block only when [T-W] precedes data_start (the record start itself).
data_start <- min(file_year_range(clm_file_list[["tas"]])$fy)
fill_lo    <- max(data_start, Y0 - W); fill_hi <- fill_lo + W - 1L
serve_hi   <- fill_hi + 1L   # highest product year the initial ring serves

seed_dir  <- paste0(output_dir, "crop_calendars/seed_rings/")
seed_file <- function(end_year) paste0(seed_dir, "seed_ring_", gcm, "_w", W, "_end", end_year, ".rds")
save_seed <- Sys.getenv("SAVE_SEED", "0") == "1"

# Initial ring = [fill_lo, fill_hi]. For a future scenario this is the pure-historical
# seed [Y0-W, Y0-1]; if a saved seed ring from the historical run of this GCM exists,
# load it and skip re-reading those 30 years (otherwise read them normally).
sf <- seed_file(Y0 - 1L)
if (Y0 - W >= data_start && file.exists(sf)) {
  cat("Loading seed ring:", sf, "\n")
  ring <- readRDS(sf)
  if (ring$ncells != NCELLS || ring$window != W) stop("Seed ring grid/window mismatch with this run.")
} else {
  ring <- initClimateRing(NCELLS, W, pet_method)
  cat(sprintf("Seeding ring with %d-%d (data_start=%d; serves product years %d-%d)...\n",
              fill_lo, fill_hi, data_start, Y0, serve_hi))
  for (yr in fill_lo:fill_hi) { cat(yr, ""); ring <- read_push(ring, yr) }; cat("\n")
}

# Per-cell wettest-window hysteresis state (NA = no prior year). Carried across
# the slide so each year's PREC/PRECTEMP sowing sticks to the previous window
# unless beaten by more than wet_window_eps. The anchored seed block is the first
# computed climatology (prev_wet = NA -> plain argmax) and seeds the state.
prev_wet  <- rep(NA_integer_, NCELLS)
prev_seas <- rep(NA_character_, NCELLS)   # per-cell seasonality-class hysteresis state
prev_harv <- matrix(NA_integer_, NCELLS, length(crops))   # crop-dependent harvest state
prev_winter <- matrix(NA_integer_, NCELLS, length(crops)) # crop-dependent winter-regime state

# Product years served by the initial ring (the anchored block at the record start,
# or just Y0 otherwise) share one climatology — compute once and replicate.
blk <- which(emit_years >= Y0 & emit_years <= serve_hi)
if (length(blk) > 0) { t0 <- Sys.time()
  cy <- computeYear(ringClimatology(ring), prev_wet, prev_seas, prev_harv, prev_winter)
  arr0 <- cy$arr; prev_wet <- cy$wet; prev_seas <- cy$seas; prev_harv <- cy$harv; prev_winter <- cy$winter
  for (e in blk) store(e, arr0)
  cat(sprintf("  block %d-%d (1 climatology): %.1fs\n", min(emit_years[blk]), max(emit_years[blk]),
              as.numeric(Sys.time() - t0, units = "secs"))) }

# Slide: push year P (scenario climate once P >= scenario start), ring serves T=P+1.
last_emit <- max(emit_years)
if (last_emit - 1L >= serve_hi) for (P in serve_hi:(last_emit - 1L)) {
  ring <- read_push(ring, P)
  Tn <- P + 1L; e <- match(Tn, emit_years)
  if (!is.na(e)) { t0 <- Sys.time()
    cy <- computeYear(ringClimatology(ring), prev_wet, prev_seas, prev_harv, prev_winter)
    prev_wet <- cy$wet; prev_seas <- cy$seas; prev_harv <- cy$harv; prev_winter <- cy$winter
    store(e, cy$arr)
    cat(sprintf("  year %d: %.1fs  (rss %.1f GB)\n", Tn, as.numeric(Sys.time() - t0, units = "secs"),
                as.numeric(gc()[2, 2]) / 1024)) }
}

# Optionally save the end-state ring as a seed for future-scenario runs (opt-in via
# SAVE_SEED=1): push the last data year so the ring is [Yend-W+1, Yend] — the exact
# seed window for an (Yend+1)-start scenario of this GCM.
if (save_seed) {
  if (!dir.exists(seed_dir)) dir.create(seed_dir, recursive = TRUE)
  ring <- read_push(ring, Yend)
  saveRDS(ring, seed_file(Yend), compress = FALSE)
  cat("saved seed ring:", seed_file(Yend), "(window [", Yend - W + 1L, ",", Yend, "])\n")
}

# ------------------------------------ #
grid_clm <- data.frame(lon = land_lon, lat = land_lat)
for (ci in seq_along(crops)) {
  cal  <- OUT[[ci]]
  crop <- crops[ci]
  fn <- paste0(out_dir, "annual_calendar_", crop, "_", gcm, "_", scen, "_",
               min(emit_years), "_", max(emit_years), ".Rdata")
  save(grid_clm, emit_years, cal, gcm, scen, crop, file = fn)
  cat("saved", fn, "\n")
}
endtime <- Sys.time(); print(endtime); print(endtime - starttime)
