# ---------------------------------------------------------------------------- #
# Step 04 (annual): write LPJmL CLM inputs (sdate, hdate, phu) from the pipeline's
# netCDF products. One CLM (version-2) binary per variable, 30 bands = 15 crops x 2
# irrigations (rainfed crops first, then irrigated), cell-major (all bands of a cell
# contiguous), int16, scalar 1.
#
# Sources (decadal -- the PHU and the dates LPJmL is driven with all change only per
# decade and are mutually consistent):
#   sdate <- planting_day-decadal   (stage-02 product)
#   hdate <- maturity_day-decadal   (stage-02 product)
#   phu   <- phu                   (stage-03 product, output/crop_calendars/ncdf)
#
# Args: GCM SCENARIO
# ---------------------------------------------------------------------------- #

rm(list = ls(all.names = TRUE))
suppressMessages(library(ncdf4))
suppressMessages(library(lpjmlkit))
stime <- Sys.time(); print(stime)

work_dir <- getwd()
source(file.path(work_dir, "00_config.R"))

if (cluster_job == TRUE) { options(echo = FALSE); args <- commandArgs(trailingOnly = TRUE) } else {
  args <- c("GFDL-ESM4", "ssp126")
}
print(args)
gcm <- args[1]; scen <- args[2]

# ------------------------------------ #
# Stage-02 product directory + soc token (mirror 02/03), and stage-03 PHU directory.
if (scen %in% isimip3a_scenarios) {
  soc_file <- if (scen == "counterclim") "countersoc" else "histsoc"
  cal_dir  <- paste0(output_dir, "ISIMIP3a/InputData/socioeconomic/crop_calendar/", soc_file, "/")
} else {
  soc_dir  <- if (scen == "historical") "historical" else paste0(scen, "soc-adapt")
  soc_file <- if (scen == "historical") "histsoc" else scen
  cal_dir  <- paste0(output_dir, "ISIMIP3b/InputData/socioeconomic/crop_calendar/", gcm, "/", soc_dir, "/")
}
phu_dir <- paste0(output_dir, "crop_calendars/ncdf/", gcm, "/", scen, "/")
clm_dir <- paste0(output_dir, "crop_calendars/clm/", gcm, "/", scen, "/")
if (!dir.exists(clm_dir)) dir.create(clm_dir, recursive = TRUE)

# Product year range from any stage-02 file (all crops share it).
gcm_lc  <- tolower(gcm)
irr_tok <- function(ir) if (ir == "ir") "firr" else "noirr"
hits <- Sys.glob(paste0(cal_dir, "ggcmi-crop-calendar_", gcm_lc, "_", soc_file, "_*_annual_*.nc"))
if (length(hits) == 0) stop("Stage-02 crop-calendar files not found (run stage 02 first): ", cal_dir)
m  <- regmatches(basename(hits[1]), regexec("_annual_([0-9]{4})_([0-9]{4})\\.nc$", basename(hits[1])))[[1]]
SY <- as.integer(m[2]); EY <- as.integer(m[3])
years <- SY:EY; NYEARS <- length(years)

# ------------------------------------ #
# Band layouts. By default BOTH are written; BANDS env (e.g. "24", "30", "24 30") restricts.
#   30-band: the 15 GGCMI crops, rainfed (1-15) then irrigated (16-30).
#   24-band: the 12 LPJmL CFTs, rainfed (1-12) then irrigated (13-24). GGCMI->CFT mapping
#     (create_lpjml_sdate_hdate_input.R): Wheat (winter/spring merged), Rice<-rice1, Maize,
#     Sorghum (tropical cereals), Pulses<-peas, Temperate_Roots<-sugar_beet, Tropical_Roots
#     <-cassava, Sunflower, Soybean, Groundnut<-nuts, Rapeseed, Sugarcane. The "wheat" band
#     takes winter_wheat where its decadal-representative season is winter (planting_season-
#     median == 1), else spring_wheat (merge_winter_spring_wheat.R).
date_nc <- function(cro, ir) paste0(cal_dir, "ggcmi-crop-calendar_", gcm_lc, "_", soc_file,
                                    "_", cro, "-", irr_tok(ir), "_annual_", SY, "_", EY, ".nc")
phu_nc  <- function(cro, ir) paste0(phu_dir, cro, "_", ir, "_", gcm, "_", scen, "_",
                                    SY, "-", EY, "_ggcmi_ph3_rule_based_phu.nc4")
layouts <- list("24" = c("wheat", "ri1", "mai", "sor", "pea", "sgb", "cas", "sun", "soy", "nut", "rap", "sgc"),
                "30" = crop_ls[["ggcmi"]])
modes <- strsplit(trimws(Sys.getenv("BANDS", "24 30")), "[, ]+")[[1]]; modes <- modes[nzchar(modes)]
if (!all(modes %in% names(layouts))) stop("BANDS must be from {24,30}; got: ", Sys.getenv("BANDS"))

# Crops actually needed (union over modes; the "wheat" band needs wwh + swh).
need <- unique(unlist(lapply(modes, function(m) { L <- layouts[[m]]
  c(L[L != "wheat"], if ("wheat" %in% L) c("wwh", "swh")) })))
ncfiles <- c(outer(need, c("rf", "ir"), Vectorize(date_nc)), outer(need, c("rf", "ir"), Vectorize(phu_nc)))
miss <- ncfiles[!file.exists(ncfiles)]
if (length(miss) > 0) stop("Missing input(s) (run stages 02 & 03 first):\n  ", paste(miss, collapse = "\n  "))

# ------------------------------------ #
# LPJmL grid (the CLM cell order) and the 720x360 -> cell index mapping.
NCELLS  <- 67420L
grid_io <- suppressWarnings(read_io(grid_file, silent = TRUE))
grid_df <- data.frame(lon = round(as.numeric(grid_io$data[, 1, 1]), 2),
                      lat = round(as.numeric(grid_io$data[, 1, 2]), 2))
if (nrow(grid_df) != NCELLS) stop("Unexpected grid cell count: ", nrow(grid_df))

nc_tmp <- nc_open(date_nc(need[1], "rf")); lons <- ncvar_get(nc_tmp, "lon"); lats <- ncvar_get(nc_tmp, "lat")
pmask  <- !is.na(ncvar_get(nc_tmp, "planting_day", start = c(1, 1, 1), count = c(720, 360, 1)))
nc_close(nc_tmp)
lin_idx <- (match(grid_df$lat, lats) - 1L) * 720L + match(grid_df$lon, lons)
# Reconcile the LPJmL vs GGCMI grid mismatch (a 1-cell sub-antarctic-island artefact, as in
# stage 03): any LPJmL cell off the product grid inherits the nearest product land cell.
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

# ------------------------------------ #
# CLM version-2 header writer (LPJmL binary: headername + 7 int32 + 2 float32).
fwriteheader2 <- function(con, headername, bands, firstyear, nyears,
                          ncells = NCELLS, resolution = 0.5, scalar = 1) {
  writeChar(headername, con, eos = NULL)
  writeBin(as.integer(c(2L, 1L, firstyear, nyears, 0L, ncells, bands)), con, size = 4, endian = .Platform$endian)
  writeBin(c(resolution, scalar), con, size = 4, endian = .Platform$endian)   # float32 CELLSIZE, SCALAR
}

# Cached nc handles + field reader; each (crop, irri, date|phu) opened once.
nccache <- new.env()
getnc <- function(cro, ir, kind) { key <- paste(cro, ir, kind); h <- nccache[[key]]
  if (is.null(h)) { h <- nc_open(if (kind == "phu") phu_nc(cro, ir) else date_nc(cro, ir)); assign(key, h, nccache) }; h }
rd <- function(cro, ir, var, yy) ncvar_get(getnc(cro, ir, if (var == "phu") "phu" else "date"),
                                           var, start = c(1, 1, yy), count = c(720, 360, 1))
# One band's (sdate, hdate, phu) 720x360 fields for a year; "wheat" merges winter/spring.
band_data <- function(tok, ir, yy) {
  if (tok == "wheat") {
    win <- { s <- rd("wwh", ir, "planting_season-decadal", yy); !is.na(s) & s == 1 }   # winter where overwinters
    list(sd = ifelse(win, rd("wwh", ir, "planting_day-decadal", yy), rd("swh", ir, "planting_day-decadal", yy)),
         hd = ifelse(win, rd("wwh", ir, "maturity_day-decadal", yy), rd("swh", ir, "maturity_day-decadal", yy)),
         ph = ifelse(win, rd("wwh", ir, "phu", yy),                 rd("swh", ir, "phu", yy)))
  } else {
    list(sd = rd(tok, ir, "planting_day-decadal", yy), hd = rd(tok, ir, "maturity_day-decadal", yy),
         ph = rd(tok, ir, "phu", yy))
  }
}

# ------------------------------------ #
# Write one CLM set (sdate/hdate/phu) for a band layout.
write_mode <- function(mode) {
  L <- layouts[[mode]]; NCFT <- length(L); NB <- NCFT * 2L
  btok <- rep(L, 2); bir <- rep(c("rf", "ir"), each = NCFT)
  fn <- function(v) paste0(clm_dir, v, "_", gcm, "_", scen, "_", SY, "_", EY,
                           "_ggcmi_ph3_rule_based_crop_calendar_", NB, "bands.clm")
  sdf <- file(fn("sdate"), "wb"); fwriteheader2(sdf, "LPJSOWD", NB, SY, NYEARS)
  hdf <- file(fn("hdate"), "wb"); fwriteheader2(hdf, "LPJSOWD", NB, SY, NYEARS)
  huf <- file(fn("phu"),   "wb"); fwriteheader2(huf, "LPJmLHU", NB, SY, NYEARS)
  cat(sprintf("\n[%s] %s %s | %d bands | years %d-%d -> CLM (cell-major, int16)\n",
              format(Sys.time(), "%H:%M:%S"), gcm, scen, NB, SY, EY))
  t0 <- Sys.time()
  for (yy in seq_len(NYEARS)) {
    if (yy %% 20L == 1L || yy == NYEARS)
      cat(sprintf("[%s]   %d-band year %d/%d (%d)  elapsed %.1f min\n", format(Sys.time(), "%H:%M:%S"),
                  NB, yy, NYEARS, years[yy], as.numeric(Sys.time() - t0, units = "mins")))
    xsd <- matrix(0L, NCELLS, NB); xhd <- matrix(0L, NCELLS, NB); xph <- matrix(0L, NCELLS, NB)
    for (bb in seq_len(NB)) { bd <- band_data(btok[bb], bir[bb], yy)
      xsd[, bb] <- bd$sd[lin_idx]; xhd[, bb] <- bd$hd[lin_idx]; xph[, bb] <- bd$ph[lin_idx] }
    xsd[is.na(xsd)] <- 0L; xhd[is.na(xhd)] <- 0L; xph[is.na(xph)] <- 0L
    writeBin(as.integer(c(t(xsd))), sdf, size = 2, endian = .Platform$endian)
    writeBin(as.integer(c(t(xhd))), hdf, size = 2, endian = .Platform$endian)
    writeBin(as.integer(c(t(xph))), huf, size = 2, endian = .Platform$endian)
  }
  close(sdf); close(hdf); close(huf)
  cat(sprintf("[%s]   %d-band CLM written (sdate/hdate/phu)\n", format(Sys.time(), "%H:%M:%S"), NB))
}

for (m in modes) write_mode(m)
for (k in ls(nccache)) nc_close(get(k, nccache))
cat("\nwrote CLM files to:", clm_dir, "\n"); print(Sys.time() - stime)
