#' @title Generate an annual crop-calendar time series
#'
#' @description This function assembles
#'  crop calendars of individual time slices (stored in data.tables)
#'  into a continuous annual time series and saves it in ncdf file.
#' The function performs some filtering of crop calendar dates to avoid
#'  discontinuities and to replace default dates with observed dates.
#' The function has arguments, as all variables should be defined in the
#'  script that calls this function. This is non-optimal, but fine as
#'  the function is very specific for a single dataset (isimip3).
#' @export
generateCropCalTSerie_isimip3 <- function(
    gcm        = NULL,
    scen       = NULL,
    cro        = NULL,
    irri       = NULL,
    SYs        = NULL,
    EYs        = NULL,
    HYs        = NULL,
    years_nc   = NULL,
    crop_ls    = NULL,
    irri_ls    = NULL,
    output_dir = NULL,
    makeplot   = FALSE
) {

  # Initialize array of full time series (hist and ssp) ----
  # ------------------------------------------------------#

  lons  <- seq(-179.75, 179.75, by = 0.5)
  lats  <- seq( -89.75,  89.75, by = 0.5)
  years <- c(min(SYs):max(EYs))

  AR <- array(NA, dim = c(length(lons), length(lats), length(years)),
              dimnames = list(lon = lons, lat = lats, year = years))
  str(AR)

  # Years of the ncdf output file: first we process the whole
  #  time series and then we cut it where needed. This is to
  #  filter the the full time series without interruptions
  #  around 2014-2015 (shift historical to future climate).
  # ------------------------------------------------------#
  years_nc <- YEARSnc
  which_years_nc <- which(years %in% years_nc)

  # Read DT crop calendar and fill-in array ----
  # ------------------------------------------------------#

  cr <- which(crop_ls[["ggcmi"]] == cro)  # crop index
  ir <- which(irri_ls[["ggcmi"]] == irri) # irrigation index
  cat("\n--------------\n", cro, "---", irri, "\n--------------\n")


  # Get GGCMI phase 3 sowing and harvest dates (to replace default dates) ----
  # ------------------------------------------------------#
  fn <- paste0(ggdir, crop_ls[["ggcmi"]][cr], "_", irri_ls[["ggcmi"]][ir],
               "_ggcmi_crop_calendar_phase3_v1.01.nc4")
  nf      <- nc_open(fn)
  sdggcmi <- ncvar_get(nf, "planting_day", c(1, 1), c(720, 360))[ , 360:1]
  hdggcmi <- ncvar_get(nf, "maturity_day", c(1, 1), c(720, 360))[ , 360:1]
  nc_close(nf)


  # Initialize arrays for each variable
  # ------------------------------------------------------#
  ARsd <- ARhd <- ARst <- ARhr <- ARss <- ARgp <- AR
  ARdd <- AR # default sdate

  # Loop through time-slice-specific DT and fill in arrays
  # ------------------------------------------------------#
  for (tt in 1:length(SYs)) { # time-slice index
    # tt <- 1
    iyears <- which(years==SYs[tt]):which(years==EYs[tt])
    cat("Doing", years[iyears], "\n")

    if (!is.na(crop_ls[["rb_cal"]][cr])) {

      # Crop calendar DT.Rdata file
      fname <- paste0(paste0(output_dir, HYs[tt], "/", gcm, "/"),
                      "DT_output_crop_calendars_", crop_ls[["rb_cal"]][cr],
                      "_", gcm, "_", HYs[tt], "_", FYs[tt], "_", LYs[tt], ".Rdata")
      DT <- get(load(fname))
      DT <- DT[DT$irrigation == irri_ls[["rb_cal"]][ir], ] # subset irrig

    } else {

      # Create a DT template for crops that aren't simulated by the rule-based
      #  approach and for which we assume GGCMI ph2 approach (regain original GS)
      #   Take crop calendar DT.Rdata file of crop 1
      fname <- paste0(paste0(output_dir, HYs[1], "/"),
                      "DT_output_crop_calendars_", crop_ls[["rb_cal"]][1],
                      "_", gcm, "_", HYs[1], "_", FYs[1], "_", LYs[1], ".Rdata")
      DT <- get(load(fname))
      DT <- DT[DT$irrigation == irri_ls[["rb_cal"]][ir], ] # subset irrig

      # Replace all values with dummy number, except sowing_month set to default
      DT$crop             <- crop_ls[["all_low"]][cr]
      DT$seasonality_type <- ""
      DT$sowing_season    <- ""
      DT$sowing_month     <- 0 # default, so all will be replaced with ggcmi dates
      DT$sowing_doy       <- 0
      DT$harvest_rule     <- ""
      DT$harvest_reason   <- ""
      DT$maturity_doy     <- 0
      DT$growing_period   <- 0

    }

    print(dim(DT))

    # Loop through pixels and extract values from crop calendar DT
    for (i in 1:nrow(DT)){ # = pixel index

      if(i%%1e4==0) cat(i, "\t")

      ilat <- which(lats == DT$lat[i])
      ilon <- which(lons == DT$lon[i])

      # Repeat average dates each year in the time-slice tt
      for (j in iyears) { # iyears index of years in time slice tt

        ARsd[ilon, ilat, j] <- DT$sowing_doy[i]
        ARhd[ilon, ilat, j] <- DT$maturity_doy[i]
        ARgp[ilon, ilat, j] <- DT$growing_period[i]
        ARst[ilon, ilat, j] <- DT$seasonality_type[i]
        ARhr[ilon, ilat, j] <- DT$harvest_reason[i]
        ARss[ilon, ilat, j] <- ifelse(DT$sowing_season[i] == "winter", 1, 2)
        ARdd[ilon, ilat, j] <- ifelse(DT$sowing_month[i] == 0, 0, 1)
        # Note: # ARdd = indicates if is default sdate

      } # j
    } # i
    cat("\n")

  } # tt

  # Loop through pixels and filter + interpolate time series
  # ------------------------------------------------------#

  # Initialize arrays for each variable
  # --------------------------#

  ARsd.annual <- AR       # Sowing dates (moving-averaged)
  ARsd.mavgw  <- AR       # Sdate (moving avg window)
  ARsd.steps  <- AR       # Sowing dates (step-wise)
  ARhd.steps  <- AR       # Harvest date (step-wise)
  ARgp.steps  <- AR       # Growing period (step-wise)
  ARst.repl   <- AR       # Seasonality type (replaced)
  ARhr.repl   <- AR       # Harvest reason (replaced)
  # ARss                  # Unmodified, remain as in the original


  # Keep track of replaced values
  # --------------------------#
  count <- 0                                     # for plotting only some pixels
  count.dt <- data.frame(lon            = numeric(),
                         lat            = numeric(),
                         sdate.smoothed = logical(),    # removed jumps
                         hdate.smoothed = logical(),    # removed jumps
                         ddate.smoothed = logical(),    # removed jumps
                         ddate.replaced = logical())    # replaced default sdate


  # --------------------------#
  for (i in seq_len(nrow(DT))) {

    if (i %% 1e3 == 0) cat(i, "\t") #else cat(".")

    ilat <- which(lats == DT$lat[i])
    ilon <- which(lons == DT$lon[i])

    sdate <- ARsd[ilon, ilat, ]   # sowing date
    seast <- as.numeric(as.factor(ARst[ilon, ilat, ]))   # seasonality type
    hdate <- ARhd[ilon, ilat, ]   # harvest date
    hreas <- as.numeric(as.factor(ARhr[ilon, ilat, ]))   # harvest reason
    ddate <- ARdd[ilon, ilat, ]   # default date == 0
    sggcm <- sdggcmi[ilon, ilat] # sowing date ggcmi
    hggcm <- hdggcmi[ilon, ilat] # harvest date ggcmi

    # Filter sdate and hdates: ----
    # ------------------------------------------------------#

    # Check for temporary changes in Seasonality Type, replace and mark
    sdate.r <- replaceJumps(seast, sdate, replacing.value =     "mean")
    seast.r <- replaceJumps(seast, seast, replacing.value = "previous")
    sdate.m <- replaceJumps(seast, sdate, marking = T, mark.value = 1L)

    # Check for temporary changes in Harvest Reason, replace and mark
    hdate.r <- replaceJumps(hreas, hdate, replacing.value =     "mean")
    hreas.r <- replaceJumps(hreas, hreas, replacing.value = "previous")
    hdate.m <- replaceJumps(hreas, hdate, marking = T, mark.value = 1L)

    # --------------------------#

    sdate.r2 <- sdate.r
    sdate.m2 <- sdate.m
    hdate.r2 <- hdate.r
    hdate.m2 <- hdate.m

    # Replace default dates with GGCMI phase3 dates
    # note: for ri2 there are NAs, setting sdate and hdate == 0
    sdate.r2[which(ddate == 0)] <- ifelse(!is.na(sggcm), sggcm, 0)
    hdate.r2[which(ddate == 0)] <- ifelse(!is.na(hggcm), hggcm, 0)

    # --------------------------#

    # Check for temporary Default Sdate, replace sdate and mark
    sdate.r3 <- replaceJumps(ddate, sdate.r2, replacing.value =     "mean")
    sdate.m3 <- replaceJumps(ddate, sdate.r2, marking = T, mark.value = 2L)
    ddate.r  <- replaceJumps(ddate, ddate,    replacing.value = "previous")

    # Check for temporary Default Sdate, replace hdate and mark
    hdate.r3 <- replaceJumps(ddate, hdate.r2, replacing.value =     "mean")
    hdate.m3 <- replaceJumps(ddate, hdate.r2, marking = T, mark.value = 2L)

    # --------------------------#

    # Check for large sdate changes and mark
    sdate.m4 <- rep(NA, length(sdate.r3))
    # Which sdate changes > 90 days
    large.change.idx <- c(0, which(abs(diff(sdate.r3)) > 90),
                          length(sdate.m3)) + 1

    for (ii in 1:(length(large.change.idx) - 1)) {
      sdate.m4[large.change.idx[ii]:(large.change.idx[ii + 1] - 1)] <- ii
    }


    # --------------------------#

    # Sum vectors of Seasonality type, Default date and large sdate changes
    #  to identify all jumps to be considered as windows for rolling mean.
    #  The value is not meaningful, it's needed just to identify when the
    #  interpolation window should change.
    sdate.m5 <- 1e1 * seast.r + 1e2 * ddate + 1e3 * sdate.m4
    hdate.m4 <- 1e1 * hreas.r + 1e2 * ddate


    # Moving average sdate: ----
    # ------------------------------------------------------#

    # Compute annual sdate by 15-years moving average (on "replaced" s/hdates)
    sdate.annual    <- rollMeanInSteps(sdate.m5, sdate.r3, kk = 15)
    sdate.annual.m  <- rollMeanInSteps(sdate.m5, sdate.r3, kk = 15,
                                       marking = TRUE)

    sdate.steps <- sdate.r3
    hdate.steps <- hdate.r3


    # Remove overlapping growing seasons: ----
    # ------------------------------------------------------#
    ovlap.index <- whichOverlappingSeasons(sdate = sdate.annual,
                                           hdate =  hdate.steps,
                                           syear =   min(years),
                                           eyear =   max(years))
    sdate.annual[ovlap.index]   <- NA
    sdate.annual.m[ovlap.index] <- NA
    hdate.steps[ovlap.index]    <- NA



    # Compute growing period length: ----
    # ------------------------------------------------------#
    gp.steps <- ifelse(sdate.steps <= hdate.steps,
                       hdate.steps - sdate.steps,
                       hdate.steps + 365 - sdate.steps)

    # Fill in new arrays with filtered values ----
    # ------------------------------------------------------#

    ARsd.annual[ilon, ilat, ] <- sdate.annual   # Sowing dates (moving-averaged)
    ARsd.mavgw[ilon, ilat, ]  <- sdate.annual.m # Sdate (moving avg window)
    ARsd.steps[ilon, ilat, ]  <- sdate.steps   # Sowing dates (step-wise)
    ARhd.steps[ilon, ilat, ]  <- hdate.steps    # Harvest date (step-wise)
    ARgp.steps[ilon, ilat, ]  <- gp.steps      # Growing period (step-wise)
    ARst.repl[ilon, ilat, ]   <- seast.r        # Seasonality type (replaced)
    ARhr.repl[ilon, ilat, ]   <- hreas.r        # Harvest reason (replaced)
    #ARss (unmodified, remain as in the original)


    # Count replaced values ----
    # ------------------------------------------------------#

    count.dt <- rbind(
      count.dt,
      data.frame(pixelnr = i,
                 lon            = DT$lon[i],
                 lat            = DT$lat[i],
                 sdate.smoothed = any(sdate.m[which_years_nc] == 1L),
                 hdate.smoothed = any(hdate.m[which_years_nc] == 1L),
                 ddate.smoothed = any(sdate.m3[which_years_nc] == 2L),
                 ddate.replaced = any(ddate.r[which_years_nc] == 0))
    )


    # Plot time series for testing ----
    # ------------------------------------------------------#
    #if ( makeplot==T && count%%1000==0) {
    if ((makeplot == TRUE) &
         (irri_ls[["ggcmi"]][ir] == "rf" &
          crop_ls[["ggcmi"]][cr] %in% c("mai", "whe")) &
          i %% 1e3 == 0) {
      # File name
      pfile <- paste(crop_ls[["ggcmi"]][cr], irri_ls[["ggcmi"]][ir],
                     scen, gcm, DT$pixelnr[i], lons[ilon], lats[ilat], sep="_")

      pdf(paste0(pldir, pfile, ".pdf"), width = 7, height = 4)

      # Split panels
      layout(matrix(1:4, nrow = 2, byrow = T), heights = c(.55, .45))
      par(cex.lab=0.7, cex.axis=0.7, cex.main=1)
      par(mar = c(2, 4, 2, 1)) #c(bottom, left, top, right)

      # sdate
      plot(years, sdate, type = "l", ylim = c(1,365), xlab = "", ylab = "sdate")
      text(x = 2040, y = 300,
           labels = paste0(crop_ls[["all_low"]][cr], " ",
                           irri_ls[["all_low"]][ir], "; ",
                           "lon/lat: ", lons[ilon], "/", lats[ilat], "\n",
                           gcm, ", ", scen), cex = .7)
      lines(years, sdate.r3, type = "l", col = "blue")
      lines(years, sdate.annual, type = "l", col = "red4", lty=2)#, lwd=2)
      legend("topleft", lty=1, cex = .6, seg.len=.5, horiz=TRUE,
             legend = c("sdate.original", "sdate.replaced", "sdate.averaged"),
             col = c("black", "blue", "red4"))

      # hdate
      plot(years, hdate, type = "l", ylim = c(1,365), xlab = "", ylab = "hdate")
      lines(years, hdate.r3, type = "l", col = "blue")
      lines(years, hdate.steps, type = "l", col = "red4", lty=2)#, lwd=2)
      legend("topleft", lty=1, cex = .6, seg.len=.5, horiz=TRUE,
             legend = c("hdate.original", "hdate.replaced", "hdate.averaged"),
             col = c("black", "blue", "red4"))

      # sdate classes
      plot(years,  seast, type = "l", ylim = c(0,8), ylab = "sdate factors")
      lines(years, seast.r, type = "l", col = "blue")
      lines(years, ddate, type = "l", col = "orange")
      lines(years, sdate.annual.m+.5, type = "l", col = "red4", lty=2)#, lwd=2)
      legend("topleft", lty=1, cex = .5, seg.len=.5, horiz=TRUE,
             legend = c("seasonalty.orig", "seasonalty.repl",
                        "default.sdate", "avg.window"),
             col = c("black", "blue", "orange", "red4"))

      # hdate classes
      plot(years,  hreas, type = "l", ylim = c(0,8), ylab = "hdate factors")
      lines(years, hreas.r, type = "l", col = "blue")
      lines(years, ddate, type = "l", col = "orange")
      #lines(years, hdate.a.m+.5, type = "l", col = "red4", lty=2)#, lwd=2)
      legend("topleft", lty=1, cex = .5, seg.len=.5, horiz=TRUE,
             legend = c("hreason.orig", "hreason.repl",
                        "default.sdate", "avg.window"),
             col = c("black", "blue", "orange", "red4"))

      dev.off()

    } # plot

  } # i


  # Write count.dt in csv file to record which pixels have replaced values ----
  # ------------------------------------------------------#
  if ( any(unlist(count.dt[i, 4:7]))) { count <- count + 1 }
  cat("\nNr. of pixels for which sdate or hdate have been replaced: ", count,"\n")

  write.csv(count.dt,
            paste0(csvdir,
                   crop_ls[["ggcmi"]][cr], "_", irri_ls[["ggcmi"]][ir],
                   "_", gcm, "_", scen, "_", min(years_nc), "-", max(years_nc),
                   "_pixels_with_filtered_values.csv"))


  # Write NCDF file: ----
  # ------------------------------------------------------#

  ncfname <- paste0(ncdir,
                    crop_ls[["ggcmi"]][cr], "_", irri_ls[["ggcmi"]][ir],
                    "_", gcm, "_", scen, "_", min(years_nc), "-", max(years_nc),
                    "_ggcmi_ph3_rule_based_crop_calendar.nc4")

  # Define dimensions
  londim       <- ncdim_def("lon", "degrees_east",  lons)
  latdim       <- ncdim_def("lat", "degrees_north", lats)
  timdim       <- ncdim_def("time", "year",      years_nc)
  nc_dimension <- list(londim, latdim, timdim)

  nlon   <- length(lons)
  nlat   <- length(lats)
  ntim   <- length(years_nc)
  chkdim <- c(nlon, nlat, ntim) # ncdf chunking

  # Define variables
  sdatec_def <- ncvar_def(name = "plant-day",
                          units = "day of year", dim = nc_dimension,
                          longname = paste("Rule-based sowing date",
                                           "(10-year steps)"),
                          prec = "single", chunksizes = chkdim, compression = 6)

  hdatec_def  <- ncvar_def(name = "maty-day",
                           units = "day of year", dim = nc_dimension,
                           longname = paste("Rule-based harvest date",
                                            "(10-year steps)"), prec = "single",
                           chunksizes = chkdim, compression = 6)

  growpc_def <- ncvar_def(name = "grow-period",
                          units = "days", dim = nc_dimension,
                          longname = paste("Rule-based growing period duration",
                                           "(10-year steps)"),
                          prec = "single", chunksizes = chkdim, compression = 6)

  sdate_def  <- ncvar_def(name = "plant-day-mavg",
                          units = "day of year", dim = nc_dimension,
                          longname = paste("Rule-based sowing date",
                                           "(10-year moving-average)"),
                          prec = "single", chunksizes = chkdim, compression = 6)

  sroll_def <- ncvar_def(name = "plant-day-mavg-window",
                         units = "sequential nr.",
                         dim = nc_dimension,
                         longname = paste("Time window of moving-averaged",
                                          "sowing dates"),
                         prec = "single", chunksizes = chkdim, compression = 6)

  seast_def <- ncvar_def(name = "seasonality",
                         units = "-", dim = nc_dimension,
                         longname = paste("Climate seasonality type,
                                          (1=No Seas; 2=Prec; 3=PrecTemp;",
                                          "4=Temp; 5=TempPrec)"),
                         prec = "single", chunksizes = chkdim, compression = 6)

  harvr_def <- ncvar_def(name = "harv-reason",
                         units = "-", dim = nc_dimension,
                         longname = paste("Rule triggering harvest",
                                          "(1=GPmin; 2=GPmed; 3=GPmax;",
                                          " 4=Wstress; 5=Topt, 6=Thigh)"),
                         prec = "single", chunksizes = chkdim, compression = 6)

  sowse_def <- ncvar_def(name = "plant-season",
                         units = "days", dim = nc_dimension,
                         longname = "Sowing season (1=Winter; 2=Spring)",
                         prec = "single", chunksizes = chkdim, compression = 6)


  # Create netCDF file and put arrays
  ncout <- nc_create(ncfname, list(sdate_def,  sroll_def,
                                   sdatec_def, hdatec_def, growpc_def,
                                   seast_def,  harvr_def, sowse_def),
                     verbose = F)

  # Put variables. Note: only years needed for output file
  ncvar_put(ncout, sdatec_def,  ARsd.steps[, , which_years_nc])
  ncvar_put(ncout, hdatec_def,  ARhd.steps[, , which_years_nc])
  ncvar_put(ncout, growpc_def,  ARgp.steps[, , which_years_nc])
  ncvar_put(ncout, sdate_def,  ARsd.annual[, , which_years_nc])
  ncvar_put(ncout, sroll_def,   ARsd.mavgw[, , which_years_nc])
  ncvar_put(ncout, seast_def,    ARst.repl[, , which_years_nc])
  ncvar_put(ncout, harvr_def,    ARhr.repl[, , which_years_nc])
  ncvar_put(ncout, sowse_def,         ARss[, , which_years_nc])


  # Put additional attributes into dimension and data variables
  ncatt_put(ncout, "lon", "axis", "X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout, "lat", "axis", "Y")
  ncatt_put(ncout, "time", "axis", "T")
  ncatt_put(ncout, 0, "Note",
            paste("The unit of the time dimension is calendar year and",
                  "it refers to the plant-day variable. Mind that this",
                  "does not always coincide with the maty-day year.",
                  "If in a year (t), plant-day > maty-day,",
                  "maty-day occurs the following year (t+1)."))
  ncatt_put(ncout, 0, "Crop",
            paste0(crop_ls[["ggcmi"]][cr], "_", irri_ls[["ggcmi"]][ir]))
  ncatt_put(ncout, 0, "Institution",
            "Potsdam Institute for Climate Impact Research (PIK), Germany")
  history <- paste("Created by Sara Minoli on", date(), sep = " ")
  ncatt_put(ncout, 0, "History", history)

  # Close the file, writing data to disk
  nc_close(ncout)

  return()

}
