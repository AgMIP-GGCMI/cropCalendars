# ---------------------------------------------------------------------------- #
# Step 02 (annual): assemble the publication-ready GGCMI/ISIMIP3b crop-calendar
# NetCDFs directly from the annual sliding-window calendars (output of
# 01_compute_annual_calendars.R).
#
# Writes each DRS-compliant file in ONE pass (final variable names, time axis with
# reference year 1601, calendar "standard", ASCENDING latitude, _FillValue +
# missing_value = 1e20, per-timestep chunking, compression, DRS filename + publish
# path). This replaces the old stage 02 + the 04-07 NCO/CDO chain.
#
# Header conventions matched to the official ISIMIP3b reference
#   .../crop_calendar/v.../<soc>/ggcmi-crop-calendar_<gcm>_<soc>_<var>-<irr>_annual_<y0>_<y1>.nc
# Latitude is ASCENDING (-89.75..89.75) as in the official product (the old
# pipeline's cdo invertlat wrongly produced descending latitude).
#
# Args: GCM SCENARIO            -> all crops x irrigations in ONE process (default)
#       GCM SCENARIO CROP IRRI  -> a single crop x irrigation (ggcmi tokens)
# The crops are fast, so one job per gcm/scenario loops the whole 15x2 matrix here
# (ncdf4 + config + grid loaded once); each crop is independent and a failure in one
# does not abort the rest (reported at the end, non-zero exit if any failed).
# ---------------------------------------------------------------------------- #

rm(list = ls(all.names = TRUE))
suppressMessages(library(ncdf4))
stime <- Sys.time(); print(stime)

work_dir <- getwd()
source(file.path(work_dir, "00_config.R"))

if (cluster_job == TRUE) { options(echo = FALSE); args <- commandArgs(trailingOnly = TRUE) } else {
  args <- c("GFDL-ESM4", "historical")
}
print(args)
gcm <- args[1]; scen <- args[2]
gcm_lc <- tolower(gcm)

# Which crop x irrigation pairs to build: an explicit single pair (4 args) or the
# whole matrix (2 args).
if (length(args) >= 4) {
  pairs <- list(c(args[3], args[4]))
} else {
  pairs <- list()
  for (irri in irri_ls[["ggcmi"]]) for (cro in crop_ls[["ggcmi"]]) pairs[[length(pairs) + 1L]] <- c(cro, irri)
}

lons <- seq(-179.75, 179.75, by = 0.5); nlon <- length(lons)
lats <- seq( -89.75,  89.75, by = 0.5); nlat <- length(lats)   # ASCENDING (S->N), as official

# Load the annual calendar; the product year range comes from the file (set by stage
# 01), NOT hardcoded -- so any scenario works (ssp 2015-2100, GSWP3 1901-2019, ...).
# Rule-based crop: its own file; default crop (rb = NA): Maize's.
load_annual <- function(crop_name, scenario = scen, must = TRUE) {
  f <- Sys.glob(paste0(output_dir, "crop_calendars/annual/", scenario, "/", gcm, "/",
                       "annual_calendar_", crop_name, "_", gcm, "_", scenario, "_*.Rdata"))[1]
  if (is.na(f)) { if (must) stop("annual calendar not found: ", crop_name) else return(NULL) }
  e <- new.env(); load(f, envir = e); e
}

# ---------------------------------------------------------------------------- #
# Build the NetCDF for one crop x irrigation.
assemble <- function(cro, irri) {
  cr <- which(crop_ls[["ggcmi"]] == cro)
  ir <- which(irri_ls[["ggcmi"]] == irri)
  rb <- crop_ls[["rb_cal"]][cr]

  e <- load_annual(if (!is.na(rb)) rb else "Maize")
  grid_clm <- e$grid_clm; ncell <- nrow(grid_clm)
  years_nc <- as.integer(e$emit_years); nyears <- length(years_nc)
  y0 <- min(years_nc); y1 <- max(years_nc)

  # DRS naming / publish path. ISIMIP3b ESM runs carry the GCM token and an soc
  # (histsoc / <ssp>soc-adapt). ISIMIP3a observational runs (scen in isimip3a_scenarios,
  # any forcing dataset -- gcm stays a variable token) keep the same filename layout
  # (gcm token + soc) under ISIMIP3a, with the 3a soc: obsclim & spinclim -> histsoc,
  # counterclim -> countersoc; published under the <soc> directory (mirroring the ISIMIP3a
  # landuse layout). spinclim (1801-1900) and obsclim (1901-2019) are distinguished by
  # year range (as landuse has two histsoc_annual files).
  irr_tok <- if (irri == "ir") "firr" else "noirr"
  if (scen %in% isimip3a_scenarios) {
    soc_file <- if (scen == "counterclim") "countersoc" else "histsoc"
    publish_dir <- paste0(output_dir, "ISIMIP3a/InputData/socioeconomic/crop_calendar")
    outdir   <- paste0(publish_dir, "/", soc_file, "/")
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    ncfname  <- paste0(outdir, "ggcmi-crop-calendar_", gcm_lc, "_", soc_file, "_",
                       cro, "-", irr_tok, "_annual_", y0, "_", y1, ".nc")
  } else {
    if (scen == "historical") { soc_file <- "histsoc"; soc_dir <- "historical" } else {
      soc_file <- scen; soc_dir <- paste0(scen, "soc-adapt") }
    publish_dir <- paste0(output_dir, "ISIMIP3b/InputData/socioeconomic/crop_calendar")
    outdir   <- paste0(publish_dir, "/", gcm, "/", soc_dir, "/")
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    ncfname  <- paste0(outdir, "ggcmi-crop-calendar_", gcm_lc, "_", soc_file, "_",
                       cro, "-", irr_tok, "_annual_", y0, "_", y1, ".nc")
  }
  cat(sprintf("\n%s %s | crop %s (rb=%s) irri %s -> %s\n", gcm, scen, cro, rb, irri, basename(ncfname)))

  # ------------------------------------ #
  # Observed GGCMI/AgMIP dates, oriented to our ASCENDING lat / lon grid.
  nf <- nc_open(paste0(agmip_dir, cro, "_", irri, "_ggcmi_crop_calendar_phase3_v1.01.nc4"))
  sdggcmi <- ncvar_get(nf, "planting_day"); hdggcmi <- ncvar_get(nf, "maturity_day")
  gg_lat <- as.numeric(nf$dim[["lat"]]$vals); gg_lon <- as.numeric(nf$dim[["lon"]]$vals)
  nc_close(nf)
  if (gg_lat[1] > gg_lat[length(gg_lat)]) { sdggcmi <- sdggcmi[, ncol(sdggcmi):1]; hdggcmi <- hdggcmi[, ncol(hdggcmi):1] }
  if (gg_lon[1] > 0) { stop("Unexpected GGCMI lon origin; expected -179.75..179.75") }

  # ------------------------------------ #
  # Per-cell annual fields (rule-based crop) or all-default (rb = NA).
  ilon  <- match(round(grid_clm$lon, 2), round(lons, 2))
  ilat  <- match(round(grid_clm$lat, 2), round(lats, 2))
  lin   <- ilon + (ilat - 1L) * nlon
  yones <- rep(1, nyears)

  if (!is.na(rb)) {
    cal  <- e$cal
    pd <- cal$sow; md <- cal[[paste0("maty_", irri)]]; gp <- cal[[paste0("gp_", irri)]]
    seas <- cal$seas; hr <- cal[[paste0("hr_", irri)]]; ss <- cal$ss
    isdef <- cal$dflag == 0
  } else {
    pd <- md <- gp <- seas <- hr <- ss <- matrix(NA_real_, ncell, nyears)
    isdef <- matrix(TRUE, ncell, nyears)
  }

  # GGCMI default-date replacement (broadcast per-cell observed date over years).
  sdg <- outer(sdggcmi[cbind(ilon, ilat)], yones); hdg <- outer(hdggcmi[cbind(ilon, ilat)], yones)
  repl <- which(isdef)
  pd[repl] <- ifelse(!is.na(sdg[repl]), sdg[repl], 0)
  md[repl] <- ifelse(!is.na(hdg[repl]), hdg[repl], 0)
  gp[repl] <- ifelse(pd[repl] <= md[repl], md[repl] - pd[repl], md[repl] + 365 - pd[repl])

  # Decadal REPRESENTATIVE sowing/harvest: for each cell and decade, take the sowing and harvest dates of
  # the year whose GROWING PERIOD is the median in that decade (the year closest to the decade's median
  # growing period), and broadcast them to every year of the decade -- the same value for each year in the
  # decade. Using an actual year's dates (not an average) avoids circular-mean artefacts on the DOYs.
  # Decades END in a year divisible by 10 (..1860, 1870, .., 2010, 2020). The first group absorbs the
  # pre-1851 lead-in (1850 -> the 1850-1860 group; the calendar is ~constant there anyway).
  decade <- pmax(0L, (years_nc - 1851L) %/% 10L)
  rep_from <- function(GP, SOW, MAT, SS) {                           # median-growing-period representative per cell
    # Per cell: the decade-year whose GP is closest to the cell's median GP (first on ties).
    # Vectorised (matrixStats::rowMedians + max.col) -- ~56x faster than the per-row apply()
    # and bitwise-identical: NA cells set to +Inf never win the argmin; all-NA rows -> NA pick.
    # The sowing SEASON (SS) of that SAME representative year is carried too, so the decadal
    # planting_season-decadal is consistent with the decadal dates (used by the winter/spring
    # wheat merge in stage 04).
    med   <- matrixStats::rowMedians(GP, na.rm = TRUE)
    D     <- abs(GP - med)
    allna <- rowSums(!is.na(D)) == 0L
    D[is.na(D)] <- Inf
    pick  <- max.col(-D, ties.method = "first")                      # row argmin == which.min (first on ties)
    pick[allna] <- NA_integer_
    ok    <- which(!is.na(pick)); sel <- cbind(ok, pick[ok])
    list(ok = ok, sow = SOW[sel], mat = MAT[sel], ss = SS[sel])
  }
  pd_med <- matrix(NA_real_, ncell, nyears); md_med <- matrix(NA_real_, ncell, nyears)
  ss_med <- matrix(NA_real_, ncell, nyears)
  for (d in unique(decade)) {
    cols <- which(decade == d)
    r    <- rep_from(gp[, cols, drop = FALSE], pd[, cols, drop = FALSE], md[, cols, drop = FALSE], ss[, cols, drop = FALSE])
    pd_med[r$ok, cols] <- r$sow                                      # broadcast across the decade's years
    md_med[r$ok, cols] <- r$mat
    ss_med[r$ok, cols] <- r$ss
  }

  # 2011-2020 decade: complete it with ssp245 (2015-2020) spliced onto historical (2011-2014), and apply
  # that single representative to whatever 2011-2020 years appear in THIS scenario -- i.e. the SAME
  # 2011-2020 representative across all scenarios. Fall back to historical 2011-2014 alone if ssp245 is
  # absent. (Rule-based cells only; default cells are forced constant just below.)
  g16 <- (2011L - 1851L) %/% 10L
  if (any(decade == g16) && !is.na(rb)) {
    grab <- function(scenario, yr_lo, yr_hi) {
      e2 <- load_annual(rb, scenario, must = FALSE); if (is.null(e2)) return(NULL)
      # Cells are matched POSITIONALLY across scenarios here, so the grabbed scenario must share THIS
      # scenario's grid (same GCM => same land mask/order). If it does not, skip rather than silently
      # splice dates onto mismatched cells -- the caller then falls back to the in-scenario median (the
      # same path as a missing file). nrow alone (checked below) cannot catch a reordered/shifted grid.
      if (!isTRUE(all.equal(e2$grid_clm, grid_clm, check.attributes = FALSE))) return(NULL)
      yy <- as.integer(e2$emit_years); k <- which(yy >= yr_lo & yy <= yr_hi); if (!length(k)) return(NULL)
      list(gp  = e2$cal[[paste0("gp_", irri)]][, k, drop = FALSE], sow = e2$cal$sow[, k, drop = FALSE],
           mat = e2$cal[[paste0("maty_", irri)]][, k, drop = FALSE], ss = e2$cal$ss[, k, drop = FALSE])
    }
    parts <- Filter(Negate(is.null), list(grab("historical", 2011, 2014), grab("ssp245", 2015, 2020)))
    if (length(parts) && all(vapply(parts, function(p) nrow(p$gp) == ncell, logical(1)))) {
      r <- rep_from(do.call(cbind, lapply(parts, `[[`, "gp")),
                    do.call(cbind, lapply(parts, `[[`, "sow")),
                    do.call(cbind, lapply(parts, `[[`, "mat")),
                    do.call(cbind, lapply(parts, `[[`, "ss")))
      g16cols <- which(decade == g16)
      pd_med[r$ok, g16cols] <- r$sow; md_med[r$ok, g16cols] <- r$mat; ss_med[r$ok, g16cols] <- r$ss
    } else if (scen != "historical") {
      cat("  NOTE: ssp245/historical 2011-2020 splice unavailable (grid/file); 2011-2020 uses in-scenario median\n")
    }
  }

  # Default-date cells (GGCMI observed, broadcast over years): the representative is just that constant.
  # Default crops have no seasonality (ss is NA), so planting_season-decadal stays NA there.
  pd_med[isdef] <- pd[isdef]; md_med[isdef] <- md[isdef]; ss_med[isdef] <- ss[isdef]

  to_grid <- function(field) { A <- matrix(NA_real_, nlon * nlat, nyears); A[lin, ] <- field
    dim(A) <- c(nlon, nlat, nyears); A }
  AR <- list(planting_day = to_grid(pd), maturity_day = to_grid(md), growing_period = to_grid(gp),
             seasonality = to_grid(seas), harvest_reason = to_grid(hr), planting_season = to_grid(ss),
             "planting_day-decadal" = to_grid(pd_med), "maturity_day-decadal" = to_grid(md_med),
             "planting_season-decadal" = to_grid(ss_med))

  # ------------------------------------ #
  # Write the DRS-compliant NetCDF (one pass).
  FV <- 1.0e20
  londim <- ncdim_def("lon", "degrees_east",  lons, longname = "longitude")
  latdim <- ncdim_def("lat", "degrees_north", lats, longname = "latitude")
  timdim <- ncdim_def("time", "years since 1601-1-1 00:00:00", as.double(years_nc - 1601L),
                      unlim = TRUE, longname = "time", calendar = "standard")
  diml <- list(londim, latdim, timdim); chk <- c(nlon, nlat, 1L)
  def <- function(name, units, long) ncvar_def(name, units, diml, missval = FV, longname = long,
                                               prec = "single", chunksizes = chk, compression = 6)
  vdef <- list(
    planting_day    = def("planting_day",    "day of year", "Rule-based sowing date (annual, 30-yr sliding window)"),
    maturity_day    = def("maturity_day",    "day of year", "Rule-based harvest date (annual, 30-yr sliding window)"),
    growing_period  = def("growing_period",  "days",        "Rule-based growing period duration (annual)"),
    seasonality     = def("seasonality",     "-", "Climate seasonality type, (1=No Seas; 2=Prec; 3=PrecTemp; 4=Temp; 5=TempPrec)"),
    harvest_reason  = def("harvest_reason",  "-", "Rule triggering harvest (1=GPmin; 2=GPmed; 3=GPmax; 4=Wstress; 5=Topt; 6=Thigh)"),
    planting_season = def("planting_season", "days", "Sowing season (1=Winter; 2=Spring)"),
    "planting_day-decadal" = def("planting_day-decadal", "day of year", "Decadal representative sowing date: sowing of the year with the median growing period in the decade (same value for each year of the decade)"),
    "maturity_day-decadal" = def("maturity_day-decadal", "day of year", "Decadal representative harvest date: harvest of the year with the median growing period in the decade (same value for each year of the decade)"),
    "planting_season-decadal" = def("planting_season-decadal", "-", "Sowing season (1=Winter; 2=Spring) of the decadal representative year (the median-growing-period year; same value for each year of the decade)"))

  ncout <- nc_create(ncfname, vdef, force_v4 = TRUE, verbose = FALSE)
  for (v in names(vdef)) {
    ncvar_put(ncout, vdef[[v]], AR[[v]])
    ncatt_put(ncout, v, "missing_value", FV, prec = "float")
  }
  ncatt_put(ncout, "lon", "standard_name", "longitude"); ncatt_put(ncout, "lon", "axis", "X")
  ncatt_put(ncout, "lat", "standard_name", "latitude");  ncatt_put(ncout, "lat", "axis", "Y")
  ncatt_put(ncout, "time", "standard_name", "time");     ncatt_put(ncout, "time", "axis", "T")
  ncatt_put(ncout, 0, "Conventions", "CF-1.6")
  ncatt_put(ncout, 0, "Note", paste("The unit of the time dimension is calendar year and it refers to the",
            "plant-day variable. Mind that this does not always coincide with the maty-day year.",
            "If in a year (t), plant-day > maty-day, maty-day occurs the following year (t+1)."))
  ncatt_put(ncout, 0, "Crop", paste0(cro, "_", irri))
  ncatt_put(ncout, 0, "Institution", "Potsdam Institute for Climate Impact Research (PIK), Germany")
  ncatt_put(ncout, 0, "History", paste0("Created by Jens Heinke on ", format(Sys.time(), "%Y-%m-%d"),
            " with the cropCalendars annual sliding-window pipeline (30-yr window slid by 1 year)."))
  nc_close(ncout)
  cat("saved", ncfname, "\n")
}

# ---------------------------------------------------------------------------- #
# Loop the requested crop x irrigation pairs; one bad crop must not abort the rest.
fail <- character(0); t_all <- Sys.time(); np <- length(pairs)
for (pi in seq_along(pairs)) {
  p <- pairs[[pi]]; cro <- p[1]; irri <- p[2]; t_p <- Sys.time()
  cat(sprintf("[%s] [%2d/%d] %s-%s ...\n", format(Sys.time(), "%H:%M:%S"), pi, np, cro, irri))
  ok <- tryCatch({ assemble(cro, irri); TRUE },
                 error = function(ec) { cat(sprintf("  FAILED %s %s: %s\n", cro, irri, conditionMessage(ec))); FALSE })
  if (ok) { tot <- as.numeric(Sys.time() - t_all, units = "mins")
    cat(sprintf("  done in %.0fs  (elapsed %.1f min, eta %.1f min)\n",
                as.numeric(Sys.time() - t_p, units = "secs"), tot, tot / pi * (np - pi))) }
  if (!ok) fail <- c(fail, paste0(cro, "-", irri))
}
cat(sprintf("\n%s %s: %d of %d crop-irrigation files written%s\n", gcm, scen,
            length(pairs) - length(fail), length(pairs),
            if (length(fail)) paste0("; FAILED: ", paste(fail, collapse = ", ")) else ""))
print(Sys.time() - stime)
if (length(fail)) quit(status = 1L, save = "no")
