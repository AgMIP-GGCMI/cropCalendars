# ---------------------------------------------------------------------------- #
# Step 01a (annual): build the per-year 30-yr sliding-window climatology.
#
# Streams the raw daily climate ONE year at a time into a `window`-year ring
# buffer and, for each target year T, writes the (optionally smoothed) climatology
# of [T-window, T-1] to a per-year .Rdata file. This is the I/O- and RAM-heavy half
# of the old unified stage 01, split off so the cheap rule processing (01b) can be
# re-run on the cached climatology WITHOUT re-reading the raw climate, and without
# the ring buffer being live in the parent during the mclapply fork (the cause of
# the 64-worker copy-on-write OOM). No crops, no fork here.
#
# Args: GCM SCENARIO
# Env : EMIT_STEP (default from config); YEARS subset for dev iteration, e.g.
#       YEARS="1990:2014" or "2000,2005,2010" (default: all emit years). Only the
#       requested years are written; the ring is still streamed up to the latest
#       requested year so each window is correct.
# ---------------------------------------------------------------------------- #

rm(list = ls(all.names = TRUE))
starttime <- Sys.time(); print(starttime)

work_dir <- getwd()
source(file.path(work_dir, "00_config.R"))

if (cluster_job == TRUE) { options(echo = FALSE); args <- commandArgs(trailingOnly = TRUE) } else {
  args <- c("GFDL-ESM4", "historical")
}
print(args)
gcm  <- args[1]
scen <- args[2]

W         <- clm_avg_years
emit_step <- as.integer(Sys.getenv("EMIT_STEP", as.character(clm_emit_step)))
# NB: 01a writes the RAW daily climatology. The global daily-climatology smoothing
# (smooth_window) is applied at rule time in 01b, so it is tunable there
# without recomputing this climatology cache.

# YEARS subset (dev iteration). "" = all emit years.
parse_years <- function(s) {
  if (is.null(s) || s == "") return(NULL)
  sort(unique(unlist(lapply(strsplit(s, ",")[[1]], function(p) {
    p <- trimws(p)
    if (grepl(":", p)) { ab <- as.integer(strsplit(p, ":")[[1]]); ab[1]:ab[2] } else as.integer(p)
  }))))
}
years_env <- parse_years(Sys.getenv("YEARS", ""))

clim_dir <- paste0(output_dir, "/crop_calendars/annual_climatology/", scen, "/", gcm, "/")
if (!dir.exists(clim_dir)) dir.create(clim_dir, recursive = TRUE)

# ------------------------------------ #
# Climate file discovery (identical to the unified driver).
vars <- c("tas", "pr", "rsds", "rlds", "huss", "sfcwind", "ps")
file_year_range <- function(fnames) {
  m <- regmatches(fnames, regexpr("[0-9]{4}_[0-9]{4}\\.nc$", fnames))
  list(fy = as.integer(substr(m, 1, 4)), ly = as.integer(substr(m, 6, 9)))
}
discover_clm <- function(scenario, var) {
  for (root in climate_dirs) {
    subdir <- if (grepl("/obsclim/", root)) {
                if (scenario == "obsclim") "historical" else next
              } else if (grepl("/spinclim/", root)) {
                if (scenario == "spinclim") "historical" else next
              } else scenario
    f <- Sys.glob(file.path(root, subdir, gcm, paste0("*_", var, "_global_daily_*.nc")))
    if (length(f) > 0) return(sort(f))
  }
  character(0)
}
seeded <- grepl("^ssp", scen)
# ssp534-over (SSP5-3.4 overshoot) only has its own climate from 2040 on; per the
# ISIMIP3b overshoot convention (climate_nc2clm_ISIMIP3B_overshoot.sh: scen_name_pre
# = ssp585, skipyear = 2040), the run FOLLOWS ssp585 THROUGH 2040 and uses the
# overshoot's own data only from 2041 on. So the early sliding window is filled from
# ssp585 up to AND INCLUDING the branch year 2040 (historical covers <= 2014), and
# those ssp585 fillers PRECEDE the ssp534-over files: the duplicated 2040 (ssp585's
# 2031_2040 chunk vs ssp534-over's 2040_2040 file) then resolves to ssp585 -- mirroring
# the reference's -skipyear 2040 -- while 2041+ falls through to ssp534-over (the
# ssp585 fillers are truncated at <= 2040). read_year_cells takes the FIRST file in
# the list covering a given year, so list order encodes this precedence.
gap_scen    <- if (scen == "ssp534-over") "ssp585" else NA_character_
branch_year <- 2040L
clm_file_list <- list()
for (v in vars) {
  fv <- discover_clm(scen, v)
  if (length(fv) == 0) stop("No climate files for ", gcm, " / ", scen, " / ", v)
  fill <- character(0)
  if (!is.na(gap_scen)) {
    ff <- discover_clm(gap_scen, v)                       # ssp585 chunks, sorted
    fill <- ff[file_year_range(ff)$fy <= branch_year]     # ... truncated to <= branch year (2015-2040)
  }
  clm_file_list[[v]] <- if (seeded) c(discover_clm("historical", v), fill, fv) else fv
}
# Product year range. A gap scenario (ssp534-over) is written in FULL as an ordinary
# continuous ssp: it STARTS at the filler scenario's first year (ssp585 2015) and ends
# at its own last year (2100). Seeded from the existing end-2014 historical ring and fed
# identical ssp585 climate through 2040, the 2015-2040 calendars come out identical to
# ssp585 (no cold-start discontinuity); 2041+ diverges. Writing the full series lets the
# downstream stages treat ssp534-over like any other ssp -- no special handling, and no
# need to bank a post-2040 seed ring.
own_rng <- file_year_range(discover_clm(scen, "tas"))
Yend <- max(own_rng$ly)
Y0   <- if (!is.na(gap_scen)) min(file_year_range(discover_clm(gap_scen, "tas"))$fy) else min(own_rng$fy)
read_year_cells <- function(fnames, yr, conv = identity) {
  rng <- file_year_range(fnames); wi <- which(rng$fy <= yr & rng$ly >= yr)[1]
  if (is.na(wi)) stop("No climate file covering year ", yr)
  dates_f <- seqDates(paste0(rng$fy[wi], "-01-01"), paste0(rng$ly[wi], "-12-31"), "day")
  pos <- which(as.integer(substr(dates_f, 1, 4)) == yr)
  arr <- cropCalendars::readNcdf(fnames[wi], dim_subset = list(time = pos[1]:pos[length(pos)]),
                                 index_dims = "time")
  conv(matrix(arr, nrow = dim(arr)[1] * dim(arr)[2])[cell_lin, , drop = FALSE])
}

# Cell set = the fixed GGCMI 67420-cell land mask.
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
grid_clm <- data.frame(lon = land_lon, lat = land_lat)

# Emit years + the subset actually written.
emit_years <- sort(unique(c(seq(Y0, Yend, by = emit_step), Yend)))
years_write <- if (is.null(years_env)) emit_years else intersect(emit_years, years_env)
if (length(years_write) == 0) stop("YEARS subset does not intersect the emit years ", Y0, "-", Yend)
last_needed <- max(years_write)
cat(sprintf("\n%s %s | window=%d emit_step=%d | product %d-%d%s\n  writing %d climatology years (%d..%d)\n",
            gcm, scen, W, emit_step, Y0, Yend, if (seeded) " (seeded from historical)" else "",
            length(years_write), min(years_write), max(years_write)))

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

# The ring serves only the daily climatology (dtemp/dprec/dpet); the monthly
# seasonality stats are reconstructed downstream in calcCropCalendars (01b).
save_clim <- function(yr, clim) {
  fn <- file.path(clim_dir, sprintf("climatology_%s_%s_%d.Rdata", gcm, scen, yr))
  save(clim, grid_clm, gcm, scen, yr, W, file = fn, compress = FALSE)
}

# ------------------------------------ #
# Ring seeding (identical anchoring/seed-cache logic as the unified driver).
data_start <- min(file_year_range(clm_file_list[["tas"]])$fy)
fill_lo    <- max(data_start, Y0 - W); fill_hi <- fill_lo + W - 1L
serve_hi   <- fill_hi + 1L
seed_dir   <- paste0(output_dir, "crop_calendars/seed_rings/")
seed_file  <- function(end_year) paste0(seed_dir, "seed_ring_", gcm, "_w", W, "_end", end_year, ".rds")
save_seed  <- Sys.getenv("SAVE_SEED", "0") == "1"

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

# Anchored block: years [Y0, serve_hi] share the first full-window climatology.
blk <- years_write[years_write >= Y0 & years_write <= serve_hi]
if (length(blk) > 0) { t0 <- Sys.time()
  clim_blk <- ringClimatology(ring)
  for (yr in blk) save_clim(yr, clim_blk)
  cat(sprintf("  block %d-%d (1 climatology -> %d files): %.1fs\n",
              min(blk), max(blk), length(blk), as.numeric(Sys.time() - t0, units = "secs"))) }

# Slide: push year P, ring then serves T = P+1. Stop once past the last needed year.
t_all <- Sys.time(); P0 <- serve_hi; Plast <- last_needed - 1L
if (Plast >= serve_hi) for (P in serve_hi:Plast) {
  t0 <- Sys.time()                       # time the whole iteration, incl. the climate read
  ring <- read_push(ring, P)
  Tn <- P + 1L
  if (Tn %in% years_write) {
    save_clim(Tn, ringClimatology(ring))
    tot <- as.numeric(Sys.time() - t_all, units = "mins"); frac <- (P - P0 + 1L) / (Plast - P0 + 1L)
    cat(sprintf("[%s]  year %d (%.0f%%): %.1fs  (rss %.1f GB, elapsed %.1f min, eta %.1f min)\n",
                format(Sys.time(), "%H:%M:%S"), Tn, 100 * frac,
                as.numeric(Sys.time() - t0, units = "secs"), as.numeric(gc()[2, 2]) / 1024,
                tot, tot / frac - tot)) }
}

# Optional seed-ring cache for future-scenario runs (unchanged).
if (save_seed) {
  if (!dir.exists(seed_dir)) dir.create(seed_dir, recursive = TRUE)
  ring <- read_push(ring, Yend)
  saveRDS(ring, seed_file(Yend), compress = FALSE)
  cat("saved seed ring:", seed_file(Yend), "(window [", Yend - W + 1L, ",", Yend, "])\n")
}

cat("climatology written to:", clim_dir, "\n")
endtime <- Sys.time(); print(endtime); print(endtime - starttime)
