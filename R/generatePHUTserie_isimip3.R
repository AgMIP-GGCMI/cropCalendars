#' @title Generate decadal crop Phenological Heat Unit (PHU) netCDFs for LPJmL.
#'
#' @description One PHU per decade (broadcast to each year of the decade): for every
#' decade the decadal-representative sowing/harvest (planting_day-decadal /
#' maturity_day-decadal from the stage-02 product) defines a fixed growing window, the
#' PHU is accumulated for each year of the decade over that window from that year's daily
#' temperature, and the per-cell median across the decade's years is taken. Decade
#' boundaries match stage 02 (end in years divisible by 10).
#'
#' All crop x irrigation combinations of one gcm x scenario are processed in a SINGLE
#' call so each year's daily temperature is read ONCE and reused across crops (the
#' temperature is crop-independent) -- one compact job per gcm x scenario instead of
#' one per crop. One netCDF is written per crop x irrigation.
#'
#' @export
generatePHUTserie_isimip3 <- function(
    ncdir         = NULL,
    gcm           = NULL,
    scen          = NULL,
    cros          = NULL,    # ggcmi crop tokens (e.g. c("wwh","mai",...))
    irris         = c("rf", "ir"),
    FYnc          = NULL,
    LYnc          = NULL,
    grid_df       = NULL,
    cal_dir       = NULL,    # stage-02 product directory (gcm x scenario)
    soc_file      = NULL,    # soc token in the stage-02 filename (histsoc / <ssp> / countersoc)
    crop_par_file = NULL,
    ncores        = 1L       # parallelise the per-crop PHU over this many cores (fork; needs R_GC_MEM_GROW=0)
) {
  if (is.null(grid_df)) stop("generatePHUTserie_isimip3: grid_df (LPJmL grid) is required.")
  years      <- FYnc:LYnc
  nyears     <- length(years)
  NCELLS     <- nrow(grid_df)
  gcm_lc     <- tolower(gcm)
  irr_tok    <- function(ir) if (ir == "ir") "firr" else "noirr"
  ncpath     <- function(cro, ir) paste0(cal_dir, "ggcmi-crop-calendar_", gcm_lc, "_", soc_file,
                                         "_", cro, "-", irr_tok(ir), "_annual_", FYnc, "_", LYnc, ".nc")

  # ------------------------------------------------------#
  # Crop parameters (once) + per crop x irrigation job table ----
  if (is.null(crop_par_file)) {
    crop_par_file <- system.file("extdata", "lpjml_crop_parameters_ggcmi_ph3.csv",
                                package = "cropCalendars", mustWork = TRUE)
  }
  allpar <- read.csv(crop_par_file, header = TRUE, stringsAsFactors = FALSE)

  jobs <- list()
  for (cro in cros) for (ir in irris) {
    cr <- which(crop_ls[["ggcmi"]] == cro)
    cp <- allpar[allpar$crop == cro, ]
    jobs[[length(jobs) + 1L]] <- list(
      cro = cro, ir = ir, cr = cr,
      basetemp = cp$basetemp, max.vern.days = cp$max.vern.days,
      tv1 = cp$vern.temp.min, tv2 = cp$vern.temp.opt.min,
      tv3 = cp$vern.temp.opt.max, tv4 = cp$vern.temp.max,
      is_vernal_all  = crop_ls[["vernal"]][cr] == "yes_all",
      is_vernal_cond = crop_ls[["vernal"]][cr] == "yes",
      ncfile = ncpath(cro, ir))
  }
  njob <- length(jobs)
  cat(sprintf("\nPHU (decadal): %s %s | %d crop x irrigation | years %d-%d | %d core(s)\n",
              gcm, scen, njob, FYnc, LYnc, ncores))

  # ------------------------------------------------------#
  # 720x360 <-> NCELLS index mapping (once; all crops share the product grid) ----
  nc_tmp <- nc_open(jobs[[1]]$ncfile)
  lons   <- ncvar_get(nc_tmp, "lon")
  lats   <- ncvar_get(nc_tmp, "lat")
  pmask  <- !is.na(ncvar_get(nc_tmp, "planting_day", start = c(1, 1, 1), count = c(720, 360, 1)))
  nc_close(nc_tmp)
  lin_idx <- (match(grid_df$lat, lats) - 1L) * 720L + match(grid_df$lon, lons)
  # Reconcile the LPJmL vs GGCMI grid mismatch (a 1-cell sub-antarctic-island artefact):
  # any LPJmL cell off the product grid inherits the nearest product land cell's calendar.
  off <- which(!pmask[lin_idx])
  if (length(off) > 0) {
    land     <- which(as.vector(pmask))
    land_lon <- lons[((land - 1L) %% 720L) + 1L]
    land_lat <- lats[((land - 1L) %/% 720L) + 1L]
    for (i in off) {
      dd <- (land_lon - grid_df$lon[i])^2 + (land_lat - grid_df$lat[i])^2
      lin_idx[i] <- land[which.min(dd)]
    }
    cat("Remapped", length(off), "LPJmL cell(s) off the product grid to nearest land cell\n")
  }

  # ------------------------------------------------------#
  # Decade grouping (MUST match stage 02: decades end in years divisible by 10) ----
  decade_id <- pmax(0L, (years - 1851L) %/% 10L)
  dec_list  <- unique(decade_id)
  ndec      <- length(dec_list)
  dec_cols  <- lapply(dec_list, function(d) which(decade_id == d))

  # 2011-2020 special-casing: stage 02 makes the decadal-representative DATES for this decade
  # identical across all ESM scenarios by splicing historical 2011-2014 + ssp245 2015-2020. To
  # keep the PHU consistent with those identical dates, the decade-16 temperature is taken from
  # the SAME spliced near-term climate for every ESM scenario (incl. historical) rather than
  # each scenario's own 2011-2020 years. Fallbacks: ssp245 .clm missing -> historical 2011-2014;
  # historical also missing -> the product's own years. Observational products (obsclim/spinclim/
  # counterclim) keep their own years (no scenario splice).
  g16     <- (2011L - 1851L) %/% 10L
  obs_scn <- scen %in% isimip3a_scenarios
  has_clm <- function(sc, fy, ly) file.exists(paste.isimip3b.clm.fn(isimip3b.path, gcm, sc, "tas", fy, ly))
  decade_tas_src <- function(d, yrs_present) {
    if (d == g16 && !obs_scn) {
      if (has_clm("historical", 1850L, 2014L) && has_clm("ssp245", 2015L, 2100L))
        return(rbind(data.frame(sc = "historical", yr = 2011:2014, stringsAsFactors = FALSE),
                     data.frame(sc = "ssp245",     yr = 2015:2020, stringsAsFactors = FALSE)))
      if (has_clm("historical", 1850L, 2014L))
        return(data.frame(sc = "historical", yr = 2011:2014, stringsAsFactors = FALSE))
    }
    data.frame(sc = scen, yr = yrs_present, stringsAsFactors = FALSE)
  }

  # Per-job PHU over a fixed window from one year's daily temperature (thermal / vernal model).
  phu_one <- function(job, sda, hda, tas_day, mtemp_mat) {
    bt <- job$basetemp
    if (job$is_vernal_all) {
      vd  <- .calc_vd_vec(mtemp_mat, job$max.vern.days, tv2 = job$tv2, tv3 = job$tv3)
      vrf <- .build_vrf_mat(sda, hda, tas_day, vd, tv1 = job$tv1, tv2 = job$tv2, tv3 = job$tv3, tv4 = job$tv4)
      .calc_phu_vernal_vec(sda, hda, tas_day, vrf, bt)
    } else if (job$is_vernal_cond) {
      tcm <- apply(mtemp_mat, 1L, min)
      wc  <- .wintercrop_vec(sda, hda, tcm, grid_df$lat)
      vd  <- .calc_vd_vec(mtemp_mat, job$max.vern.days, tv2 = job$tv2, tv3 = job$tv3) * wc
      vrf <- .build_vrf_mat(sda, hda, tas_day, vd, tv1 = job$tv1, tv2 = job$tv2, tv3 = job$tv3, tv4 = job$tv4)
      pt  <- .calc_phu_thermal_vec(sda, hda, tas_day, bt)
      pv  <- .calc_phu_vernal_vec(sda, hda, tas_day, vrf, bt)
      ifelse(wc == 1L, pv, pt)
    } else {
      .calc_phu_thermal_vec(sda, hda, tas_day, bt)
    }
  }

  # ------------------------------------------------------#
  # Loop decades; within each, read each year's temperature ONCE and compute every crop ----
  phu_dec <- lapply(seq_len(njob), function(.) matrix(NA_real_, NCELLS, ndec))   # decadal PHU per cell
  t_all <- Sys.time()

  for (di in seq_along(dec_list)) {
    t_dec <- Sys.time()
    d <- dec_list[di]; in_dec <- dec_cols[[di]]; yrs_d <- years[in_dec]
    cat(sprintf("[%s] decade %d/%d  %d-%d (%d yr)", format(Sys.time(), "%H:%M:%S"),
                di, ndec, min(yrs_d), max(yrs_d), length(yrs_d)))

    # Representative window per job (crop x irrigation), constant within the decade.
    sda_l <- vector("list", njob); hda_l <- vector("list", njob)
    for (jj in seq_len(njob)) {
      nc    <- nc_open(jobs[[jj]]$ncfile)
      sdate <- ncvar_get(nc, "planting_day-decadal", start = c(1, 1, in_dec[1]), count = c(720, 360, 1))
      hdate <- ncvar_get(nc, "maturity_day-decadal", start = c(1, 1, in_dec[1]), count = c(720, 360, 1))
      nc_close(nc)
      sa <- as.integer(round(matrix(sdate, nrow = 720L * 360L)[lin_idx]))
      ha <- as.integer(round(matrix(hdate, nrow = 720L * 360L)[lin_idx]))
      # No-crop / missing-date cells (NA, or the stage-02 zero fill where GGCMI has no date)
      # get a 1-day growing period (PHU ~ 0). A DOY of 0 must NOT reach the PHU kernels: the
      # vernal .build_vrf_mat would index column 0 (length-zero -> crash) and the thermal
      # path would read a spurious full-year sum.
      bad <- is.na(sa) | is.na(ha) | sa < 1L | ha < 1L
      sa[bad] <- 1L; ha[bad] <- 2L
      sda_l[[jj]] <- sa; hda_l[[jj]] <- ha
    }

    tsrc <- decade_tas_src(d, yrs_d)
    if (d == g16 && !obs_scn) cat(sprintf(" [2011-2020 splice: %d source-years %s..%s]",
        nrow(tsrc), tsrc$sc[1L], tsrc$sc[nrow(tsrc)]))

    pacc <- lapply(seq_len(njob), function(.) matrix(NA_real_, NCELLS, nrow(tsrc)))
    for (k in seq_len(nrow(tsrc))) {
      tas_day   <- get.isimip.tas(gcm, tsrc$sc[k], tsrc$yr[k], tsrc$yr[k], ncells = NCELLS)[, , 1L]
      mtemp_mat <- .monthlyFromDaily(tas_day, "mean")
      # Crops are independent; spread them over cores (the few vernal crops are the long poles,
      # so dynamic scheduling -- mc.preschedule=FALSE -- balances them across workers). tas_day /
      # mtemp_mat are read ONCE and shared with the forked workers (copy-on-write).
      one <- function(jj) phu_one(jobs[[jj]], sda_l[[jj]], hda_l[[jj]], tas_day, mtemp_mat)
      res <- if (ncores > 1L)
        parallel::mclapply(seq_len(njob), one, mc.cores = ncores, mc.preschedule = FALSE)
      else lapply(seq_len(njob), one)
      if (any(vapply(res, function(x) !is.numeric(x) || length(x) != NCELLS, logical(1))))
        stop("a PHU worker failed (re-run with ncores=1 to see the error)")
      for (jj in seq_len(njob)) pacc[[jj]][, k] <- res[[jj]]
      rm(tas_day, mtemp_mat, res)
    }
    for (jj in seq_len(njob)) phu_dec[[jj]][, di] <- apply(pacc[[jj]], 1L, median, na.rm = TRUE)
    rm(pacc, sda_l, hda_l)
    el  <- as.numeric(Sys.time() - t_dec, units = "secs")
    tot <- as.numeric(Sys.time() - t_all, units = "mins")
    cat(sprintf("  done in %.0fs  (elapsed %.1f min, eta %.1f min)\n", el, tot, tot / di * (ndec - di)))
  } # decade

  # ------------------------------------------------------#
  # Write one netCDF per crop x irrigation (broadcast the decadal PHU over its years) ----
  cat(sprintf("[%s] PHU computed in %.1f min; writing %d netCDFs ...\n",
              format(Sys.time(), "%H:%M:%S"), as.numeric(Sys.time() - t_all, units = "mins"), njob))
  londim <- ncdim_def("lon", "degrees_east",  lons)
  latdim <- ncdim_def("lat", "degrees_north", lats)
  timdim <- ncdim_def("time", "year", years)
  nc_dimension <- list(londim, latdim, timdim)

  for (jj in seq_len(njob)) {
    job  <- jobs[[jj]]
    cube <- array(NA, c(720L, 360L, nyears)); pa <- array(NA, c(720L, 360L))
    for (di in seq_along(dec_list)) {
      flat <- rep(NA_integer_, 720L * 360L)
      flat[lin_idx] <- as.integer(round(phu_dec[[jj]][, di]))
      pa[] <- flat
      for (yy in dec_cols[[di]]) cube[, , yy] <- pa
    }
    ncfname <- paste0(ncdir, crop_ls[["ggcmi"]][job$cr], "_", job$ir, "_",
                      gcm, "_", scen, "_", FYnc, "-", LYnc, "_ggcmi_ph3_rule_based_phu.nc4")
    phu_def <- ncvar_def(name = "phu", units = "degree days", dim = nc_dimension,
                         longname = "Phenological Heat Unit Requirements",
                         prec = "single", compression = 6)
    ncout <- nc_create(ncfname, list(phu_def), verbose = FALSE)
    ncvar_put(ncout, phu_def, cube)
    ncatt_put(ncout, "lon",  "axis", "X")
    ncatt_put(ncout, "lat",  "axis", "Y")
    ncatt_put(ncout, "time", "axis", "T")
    ncatt_put(ncout, 0, "Crop", paste0(crop_ls[["ggcmi"]][job$cr], "_", job$ir))
    ncatt_put(ncout, 0, "Institution", "Potsdam Institute for Climate Impact Research (PIK), Germany")
    ncatt_put(ncout, 0, "history", paste("Created by Jens Heinke on", format(Sys.time(), "%Y-%m-%d")))
    nc_close(ncout)
    cat(sprintf("  [%2d/%d] wrote %s\n", jj, njob, basename(ncfname)))
  }
  cat(sprintf("[%s] %s %s DONE -- %d netCDFs in %.1f min total\n", format(Sys.time(), "%H:%M:%S"),
              gcm, scen, njob, as.numeric(Sys.time() - t_all, units = "mins")))
}


# ------------------------------------ #
# Unexported helpers: climate I/O

read.climate.input <- function(fname, ncells, ryear,
                               fyear, lyear, header = 43L,
                               nbands = 365L, dtype = "integer", scalar = 0.1) {

  cat(paste("\nReading climate input:\n-----------------------\n", fname))

  nyears <- lyear - fyear + 1
  dsize  <- ifelse(dtype == "integer", 2, 4)

  cat(paste("\nNBANDS =", sprintf("%.10f", ((file.size(fname) - header) / ncells / dsize / (lyear - ryear + 1)))))

  fcon <- file(fname, "rb")
  x    <- array(data = NA, dim = c(ncells, nbands, nyears))

  for (i in 1:nyears) {
    year <- fyear + (i - 1)
    seek(fcon, where = header + ((year - ryear) * ncells * nbands * dsize), origin = "start")
    for (j in 1:ncells) {
      x[j, , i] <- readBin(fcon, dtype, n = nbands, size = dsize) * scalar
    }
  }
  close.connection(fcon)
  cat(str(x))
  return(x)
}

paste.isimip3b.clm.fn <- function(path, gcm, scen, var, syear, eyear) {
  paste0(path,
         paste(scen, gcm,
               paste(var, tolower(gcm), scen,
                     paste(syear, eyear, sep = "-"), sep = "_"), sep = "/"), ".clm")
}

get.isimip.tas <- function(GCM, SC, SY, EY, ncells) {

  # ISIMIP3a observational forcings (GSWP3-W5E5 etc.): read the single dataset file
  # directly, with NO historical/SSP splice. ryear (= the seek-offset reference) is
  # the file's first year, so FY1 must be the dataset's actual first year. (The
  # ESM branch below still maps to the 1850-2014 / 2015-2100 ISIMIP3b windows.)
  obs_ranges <- list(obsclim     = c(1901, 2019),
                     spinclim    = c(1801, 1900),
                     counterclim = c(1901, 2019))

  if (SC %in% names(obs_ranges)) {

    FY1 <- obs_ranges[[SC]][1]
    LY1 <- obs_ranges[[SC]][2]

  } else {

    if (EY <= 2014) {
      FY1 <- 1850
      LY1 <- 2014
      SC  <- "historical"
    } else if (SY <= 2014 & EY > 2014) {
      FY1 <- 1850
      LY1 <- 2014
      SC1 <- "historical"
      FY2 <- 2015
      LY2 <- 2100
      SC2 <- SC
    } else {
      FY1 <- 2015
      LY1 <- 2100
    }

  }

  if (!(SC %in% names(obs_ranges)) && SY <= 2014 & EY > 2014) {

    tas_fn1 <- paste.isimip3b.clm.fn(isimip3b.path, GCM, SC1, "tas", FY1, LY1)
    tas_fn2 <- paste.isimip3b.clm.fn(isimip3b.path, GCM, SC2, "tas", FY2, LY2)

    tas1 <- read.climate.input(tas_fn1, ncells = ncells, ryear = FY1,
                               fyear = SY, lyear = 2014, header = 43,
                               nbands = 365, dtype = "integer", scalar = 0.1)
    tas2 <- read.climate.input(tas_fn2, ncells = ncells, ryear = FY2,
                               fyear = 2015, lyear = EY, header = 43,
                               nbands = 365, dtype = "integer", scalar = 0.1)

    tas <- array(NA, dim = c(ncells, 365, length(SY:EY)))
    tas[1:ncells, 1:365, 1:dim(tas1)[3]] <- tas1
    tas[1:ncells, 1:365, (dim(tas1)[3] + 1):(dim(tas)[3])] <- tas2
    rm(tas1, tas2)

  } else {

    # Observational forcings (obsclim/spinclim/counterclim) live under the ISIMIP3a .clm tree,
    # not ISIMIP3b; ESM scenarios use the ISIMIP3b tree. Same per-file layout (scen/gcm/...).
    base   <- if (SC %in% names(obs_ranges)) isimip3a.path else isimip3b.path
    tas_fn <- paste.isimip3b.clm.fn(base, GCM, SC, "tas", FY1, LY1)

    tas <- read.climate.input(tas_fn, ncells = ncells, ryear = FY1,
                              fyear = SY, lyear = EY, header = 43,
                              nbands = 365, dtype = "integer", scalar = 0.1)
  }

  return(tas)

}


# ------------------------------------ #
# Unexported helpers: vectorized PHU computation

# Vernalization days required (per cell): vectorised twin of calcVd, the canonical
# reference. Per row: rank the 12 monthly means coldest-first (order), keep the
# `max.vern.months` coldest, and give each a vernalization-day credit that is piecewise
# in its temperature -- the full max_per_m below tv2 (vern optimum floor), 0 above tv3
# (too warm to vernalize), a linear ramp down between -- then sum the credits and round.
# (Unlike the scalar, which always picks the 5 coldest regardless of max.vern.months, this
# keeps exactly max.vern.months; identical at the default 5 used in production.)
.calc_vd_vec <- function(mtemp_mat, max.vern.days, max.vern.months = 5L, tv2, tv3) {
  nc         <- nrow(mtemp_mat)
  max_per_m  <- max.vern.days / max.vern.months
  sorted_idx <- t(apply(mtemp_mat, 1, order))
  cold_cols  <- as.vector(sorted_idx[, 1:max.vern.months])
  row_idx    <- rep(seq_len(nc), max.vern.months)
  cold_temp  <- matrix(mtemp_mat[cbind(row_idx, cold_cols)], nrow = nc, ncol = max.vern.months)
  days_m     <- ifelse(cold_temp <= tv2, max_per_m,
                ifelse(cold_temp >= tv3, 0,
                       max_per_m * (1 - (cold_temp - tv2) / (tv3 - tv2))))
  round(rowSums(days_m))
}

# Winter-crop classification (per cell): vectorised twin of isWinterCrop (Portmann 2010,
# with tcm <= 7 not 6). growp is the circular sowing->harvest length (wrapping the year).
# A season is winter crop (1) iff it is long (growp >= 150 d), its coldest month is
# cold-but-not-killing (tcm in [-10, 7]), AND it overwinters: in the N hemisphere it
# crosses the year boundary (sdate + growp > 365); in the S hemisphere it straddles
# mid-winter (sows before and harvests after DOY 182). `valid` masks the NA/sdate<=0 rows
# the scalar guards with its outer `if` (returns 0 there). Returns 0/1.
.wintercrop_vec <- function(sdate_v, hdate_v, tcm_v, lat_v) {
  growp    <- ifelse(sdate_v <= hdate_v, hdate_v - sdate_v, 365L + hdate_v - sdate_v)
  valid    <- !is.na(sdate_v) & sdate_v > 0L & !is.na(lat_v) & !is.na(tcm_v)
  temp_ok  <- tcm_v >= -10 & tcm_v <= 7
  nh <- valid & lat_v >  0 & (sdate_v + growp > 365L) & (growp >= 150L) & temp_ok
  sh <- valid & lat_v <= 0 & (sdate_v < 182L) & (sdate_v + growp > 182L) & (growp >= 150L) & temp_ok
  as.integer(nh | sh)
}

# Shared window-sum core for the PHU models. Given a row-wise cumulative sum of daily
# effective thermal units (cumteff[, d] = sum of teff over days 1..d), return the sum over
# the growing window [sdate, hdate) WITHOUT a per-cell loop, as a difference of cumsums:
#   no wrap (sdate < hdate):  cum(hdate-1) - cum(sdate-1)            = sum[sdate .. hdate-1]
#   wrap    (hdate <= sdate): cum(365) - cum(sdate-1)  +  cum(hdate-1)  (tail + head of year)
# The pmin/pmax/<1 guards keep the column index in 1..365 and treat "sdate-1 = 0" as cum 0.
.phu_cumsum <- function(sdate_v, hdate_v, cumteff) {
  nc      <- nrow(cumteff)
  idx     <- seq_len(nc)
  no_wrap <- sdate_v < hdate_v
  hd1     <- hdate_v - 1L
  cum_hd1 <- ifelse(hd1 >= 1L, cumteff[cbind(idx, pmin(pmax(hd1, 1L), 365L))], 0)
  cum_hd1[hd1 < 1L] <- 0
  sd1     <- sdate_v - 1L
  cum_sd1 <- ifelse(sd1 >= 1L, cumteff[cbind(idx, pmin(pmax(sd1, 1L), 365L))], 0)
  cum_sd1[sd1 < 1L] <- 0
  cum_365 <- cumteff[, 365L]
  ifelse(no_wrap, cum_hd1 - cum_sd1, cum_365 - cum_sd1 + cum_hd1)
}

# PHU thermal model (phen_model "t"), vectorised twin of calcPHU: daily effective thermal
# units teff = max(T - basetemp, 0), cumulated, then summed over the growing window via
# .phu_cumsum. Positive heat-unit sum.
.calc_phu_thermal_vec <- function(sdate_v, hdate_v, temp_mat, basetemp) {
  teff    <- pmax(temp_mat - basetemp, 0)
  cumteff <- t(apply(teff, 1, cumsum))
  as.integer(.phu_cumsum(sdate_v, hdate_v, cumteff))
}

# PHU vernal-thermal model (phen_model "tv"), vectorised twin of calcPHU: as the thermal
# model but teff is scaled by the daily vernalization reduction factor (vrf_mat) before
# accumulation. Returned NEGATED -- LPJmL reads a negative PHU as "this crop needs
# vernalization".
.calc_phu_vernal_vec <- function(sdate_v, hdate_v, temp_mat, vrf_mat, basetemp) {
  teff    <- pmax(temp_mat - basetemp, 0) * vrf_mat
  cumteff <- t(apply(teff, 1, cumsum))
  as.integer(-.phu_cumsum(sdate_v, hdate_v, cumteff))
}

# Vernalization reduction factor matrix (NCELLS x 365), vectorised twin of calcVrf.
# Per day, vernalization EFFECTIVENESS veff(T) is a trapezoid in temperature: 0 below tv1,
# ramping up to 1 across [tv1,tv2], a plateau of 1 on [tv2,tv3], ramping back to 0 across
# [tv3,tv4], 0 above. The year is doubled (730 d) so a season wrapping past Dec 31 stays
# contiguous, and veff is cumulated once per row. Then per cell (the loop): accumulate veff
# from sdate, find the day the required vernalization vd is reached, and set the reduction
# factor vrf along the way -- 0 until vd_b (20%) of vd is banked, then a linear ramp to 1 at
# full vd (so growth is throttled until enough cold has accrued). vd <= 0 leaves vrf = 1
# (no requirement). The cumsum + difference (cumveff730[k] - cum0) replaces the scalar's
# day-by-day running sum; results match.
.build_vrf_mat <- function(sdate_v, hdate_v, temp_mat, vd_vec,
                           vd_b = 0.2, tv1, tv2, tv3, tv4) {
  nc      <- nrow(temp_mat)
  temp730 <- cbind(temp_mat, temp_mat)
  # Vernalization effectiveness: trapezoid (0 below tv1, 0->1 on [tv1,tv2], 1 on [tv2,tv3],
  # 1->0 on [tv3,tv4], 0 above). pmin/pmax form of the old 4-level nested ifelse -- bitwise-
  # identical and ~10x faster. The matrix is the FIRST pmin arg so its dim attribute survives.
  veff730    <- pmax(pmin((temp730 - tv1) / (tv2 - tv1), (tv4 - temp730) / (tv4 - tv3), 1), 0)
  cumveff730 <- t(apply(veff730, 1, cumsum))   # base cumsum (long-double accumulation); kept for bit-parity

  # Vectorised replacement of the old per-cell vernalization loop -- bitwise-identical given the
  # same cumveff730. Work in the doubled-year (730 d) space so a season wrapping past Dec 31
  # stays contiguous; fold back to 365 at the end.
  active <- !is.na(vd_vec) & vd_vec > 0
  hd   <- ifelse(sdate_v < hdate_v, hdate_v, hdate_v + 365L)          # window end (<= 730)
  cum0 <- numeric(nc); g <- which(sdate_v > 1L)
  if (length(g)) cum0[g] <- cumveff730[cbind(g, sdate_v[g] - 1L)]     # vern banked before sowing
  vdsum <- cumveff730 - cum0                                          # nc x 730 (cum0 per row)
  col   <- matrix(seq_len(730L), nc, 730L, byrow = TRUE)
  # endday = first day in [sdate, hd] where vdsum reaches vd; throttle window end last = min(endday, hd).
  reached <- active & (col >= sdate_v) & (col <= hd) & (vdsum >= vd_vec)
  he   <- rowSums(reached) > 0L
  end  <- rep(Inf, nc); if (any(he)) end[he] <- max.col(reached[he, , drop = FALSE], ties.method = "first")
  last <- pmin(end, hd)
  # vrf = ramp(vdsum) within [sdate, last] (0 until vd_b of vd is banked, then linear to 1),
  # else 1 (and 1 for inactive cells -- vd <= 0).
  in_win <- active & (col >= sdate_v) & (col <= last)
  ramp   <- pmax(0, pmin(1, (vdsum - vd_vec * vd_b) / (vd_vec * (1 - vd_b))))
  vrf730 <- matrix(1.0, nc, 730L); vrf730[in_win] <- ramp[in_win]
  # Fold the doubled year back to 365: the wrap (second-year) value wins where present.
  in2 <- in_win[, 366:730, drop = FALSE]
  ifelse(in2, vrf730[, 366:730, drop = FALSE], vrf730[, 1:365, drop = FALSE])
}
