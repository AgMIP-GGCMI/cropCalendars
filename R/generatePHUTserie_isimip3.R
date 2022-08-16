#' @title Generate an annual crop Phenological Heat Unit (PHU) time series
#' for the LPJmL model.
#'
#' @export
generatePHUTserie_isimip3 <- function(
    ncdir         = NULL,
    gcm           = NULL,
    scen          = NULL,
    cro           = NULL,
    irri          = NULL,
    SYs           = NULL,
    EYs           = NULL,
    FYnc          = NULL,
    LYnc          = NULL,
    crop_par_file = NULL

) {

  cr <- which(crop_ls[["ggcmi"]] == cro)
  ir <- which(irri_ls[["ggcmi"]] == irri)

  ncfname <- paste0(ncdir,
                    crop_ls[["ggcmi"]][cr], "_", irri_ls[["ggcmi"]][ir],
                    "_", gcm, "_", scen, "_", FYnc, "-", LYnc,
                    "_ggcmi_ph3_rule_based_crop_calendar.nc4")
  cat("\nreading:", ncfname)


  # ------------------------------------------------------#
  # Get Crop Parameters ----
  if (is.null(crop_par_file)) {
    crop_par_file <- system.file("extdata", "lpjml_crop_parameters_ggcmi_ph3.csv",
                                package = "cropCalendars", mustWork = TRUE)
  }
  croppar    <- subset(read.csv(crop_par_file, header = T, stringsAsFactors = F),
                       crop == cro)

  basetemp      <- croppar$basetemp
  max.vern.days <- croppar$max.vern.days
  tv1           <- croppar$vern.temp.min
  tv2           <- croppar$vern.temp.opt.min
  tv3           <- croppar$vern.temp.opt.max
  tv4           <- croppar$vern.temp.max


  # ------------------------------------------------------#

  phu.annual  <- array(NA, c(720, 360))
  phu.cube    <- array(NA, c(720, 360, nyears))

  # Loop through years ----
  for (yy in seq_len(length(SYs))) {

    cat("\n--- Doing yy", yy, "---")

    # ------------------------------------------------------#
    # Get Climate Data ----
    tas          <- get.isimip.tas(gcm, HYs[yy], SYs[yy], EYs[yy])
    tas_mean_day <- apply(tas, c(1, 2), mean)

    # ------------------------------------------------------#
    # Get Sowing and Harvest dates ----
    # Note: using step-wise sdates for phu calibration
    nc    <- nc_open(ncfname)
    sy    <- which(years == SYs[yy])
    sdate <- ncvar_get(nc, varid = "plant-day", start = c(1, 1, sy),
                       count = c(720, 360, length(SYs[yy]:EYs[yy])))
    hdate <- ncvar_get(nc, varid = "maty-day",  start = c(1, 1, sy),
                       count = c(720, 360, length(SYs[yy]:EYs[yy])))
    lons  <- ncvar_get(nc, varid = "lon")
    lats  <- ncvar_get(nc, varid = "lat")
    nc_close(nc)

    # ------------------------------------------------------#
    # Loop through grid cells ----
    for (i in 1:NCELLS) {

      ilo <- which(lons == grid_df$lon[i])
      ila <- which(lats == grid_df$lat[i])

      sdate.avg <- as.integer(mean(sdate[ilo, ila, ]))
      hdate.avg <- as.integer(mean(hdate[ilo, ila, ]))

      # # Ric2 has missing values: make an artificial 1 day growing period
      # sdate.avg <- ifelse(is.na(sdate.avg), 1, sdate.avg)
      # hdate.avg <- ifelse(is.na(hdate.avg), 2, hdate.avg)

      dtemp <- tas_mean_day[i, ]

      # ------------------------------------------------------#
      # Compute monthly temperature ----
      sday <- c( 1, 32, 60,  91, 121, 152, 182, 213, 244, 274, 305, 335)
      eday <- c(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
      mtemp <- rep(NA, 12)

      for (m in 1:12) {
        mtemp[m] <- mean(dtemp[sday[m]:eday[m]])
      }

      # ------------------------------------------------------#
      # Test if it is winter crop ----
      if (crop_ls[["vernal"]][cr] == "yes") {

        wcrop <- wintercrop(start =   sdate.avg,
                            end   =   hdate.avg,
                            tcm   =  min(mtemp),
                            lat   = grid$lat[i])
      } else {

        wcrop <- 0

      }

      # ------------------------------------------------------#
      # If Vernal-crop:
      if (crop_ls[["vernal"]][cr] == "yes_all" | wcrop == 1) {

        # Calculate Vernalization Requirements ----
        vd <- calc.vd(temp_mean_month  =         mtemp,
                      max.vern.days    = max.vern.days,
                      max.vern.months  =             5,
                      tv2              =           tv2,
                      tv3              =           tv3)

        # Calculate Vernalization Reduction Factors ----
        vrf <- calc.vrf(sdate           =     sdate.avg,
                        hdate           =     hdate.avg,
                        mdt             =         dtemp,
                        vd              =            vd,
                        vd_b            =           0.2,
                        max.vern.days   = max.vern.days,
                        max.vern.months =             5,
                        tv1             =           tv1,
                        tv2             =           tv2,
                        tv3             =           tv3,
                        tv4             =           tv4)
      } else {

        vrf <- rep(1, 365)

      }


      # ------------------------------------------------------#
      # Calculate Phenological Heat Unit Requirements ----

      # If Vernal-crop use vernal-thermal model and return negative phu
      if (crop_ls[["vernal"]][cr] == "yes_all" | wcrop == 1) {

        phu <- calcPHU(sdate       = sdate.avg,
                        hdate       = hdate.avg,
                        mdt         =     dtemp,
                        vern_factor =       vrf,
                        basetemp    =  basetemp,
                        phen_model  =      "tv")

        phu.annual[ilo, ila] <- as.integer(phu)

        # else use Thermal model
      } else {

        phu <- calcPHU(sdate       = sdate.avg,
                        hdate       = hdate.avg,
                        mdt         =     dtemp,
                        vern_factor =       vrf,
                        basetemp    =  basetemp,
                        phen_model  =       "t")

        phu.annual[ilo, ila] <- as.integer(phu)

      }

    } # i

    # Repeat same phu value for the time slice
    ys <- which(years %in% SYs[yy]:EYs[yy])
    for (j in seq_len(length(ys))) {
      phu.cube[, , ys[j]] <- phu.annual
    }

  } # yy

  tmp_dir <- paste0(work_dir, "/tmp/")
  if (!dir.exists(tmp_dir)) dir.create(tmp_dir)

  fn <- paste0(tmp_dir,
               crop_ls[["ggcmi"]][cr], "_",
               irri_ls[["ggcmi"]][ir], "_",
               gcm, "_", scen, "_", FYnc, "-", LYnc,
               "_ggcmi_ph3_rule_based_phu.Rdata")
  save(phu.cube, file = fn)

  # Repeat same phu value for the entire time period (for 2015gs scenario)
  if (max(LYs) != LYnc) {
    ys <- which(years %in% FYnc:LYnc)
    for (j in seq_len(length(ys))) {
      phu.cube[, , ys[j]] <- phu.annual
    }
      save(phu.cube, file = fn)
  }





  # ------------------------------------------------------#
  # Write Crop-specific Output File ----
  # Write NCDF file
  # ------------------------------------------------------#

  ncfname <- paste0(ncdir,
                    crop_ls[["ggcmi"]][cr], "_", irri_ls[["ggcmi"]][ir],
                    "_", gcm, "_", scen, "_", FYnc, "-", LYnc,
                    "_ggcmi_ph3_rule_based_phu.nc4")
  cat("\nwriting:", ncfname)

  # Define dimensions
  londim       <- ncdim_def("lon", "degrees_east",  lons)
  latdim       <- ncdim_def("lat", "degrees_north", lats)
  timdim       <- ncdim_def("time", "year",        years)
  nc_dimension <- list(londim, latdim, timdim)

  # Define variables
  phu_def  <- ncvar_def(name = "phu", units = "degree days", dim = nc_dimension,
                        longname = "Phenological Heat Unit Requirements",
                        prec = "single", compression = 6)

  # Create netCDF file and put arrays
  ncout <- nc_create(ncfname, list(phu_def), verbose = F)

  # Put variables
  ncvar_put(ncout, phu_def, phu.cube)

  # Put additional attributes into dimension and data variables
  ncatt_put(ncout,  "lon", "axis", "X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,  "lat", "axis", "Y")
  ncatt_put(ncout, "time", "axis", "T")

  ncatt_put(ncout, 0, "Crop",
            paste0(crop_ls[["ggcmi"]][cr], "_", irri_ls[[ir]]["ggcmi"]))
  ncatt_put(ncout, 0, "Institution",
            "Potsdam Institute for Climate Impact Research (PIK), Germany")
  history <- paste("Created by Sara Minoli on", date(), sep = " ")
  ncatt_put(ncout, 0, "history", history)

  # Close the file, writing data to disk
  nc_close(ncout)

  unlink(fn)

}

# ------------------------------------ #
# Unexported functions


# Climate input
read.climate.input <- function(fname, ncells = NCELLS, ryear = RYEAR,
                               fyear = FYEAR, lyear = LYEAR, header = HEADER,
                               nbands = NBANDS, dtype = DTYPE, scalar = SCALAR) {

  cat(paste("\nReading climate input:\n-----------------------\n", fname))

  # number of years
  nyears <- lyear-fyear+1

  # data size
  dsize <- ifelse(dtype=="integer", 2, 4)

  # check file size
  cat(paste("\nNBANDS =", sprintf("%.10f", ((file.size(fname)-header)/ncells/dsize/(lyear-ryear+1))))) # need to change it, lyear!=lyear of the file...

  fcon <- file(fname, "rb")
  x <- array(data=NA, dim = c(ncells, nbands, nyears) )

  for (i in 1:nyears) {

    year <- fyear+(i-1)

    seek(fcon, where = header+((year-ryear)*ncells*nbands*dsize), origin = "start")

    for (j in 1:ncells) {

      x[j,,i] <- readBin(fcon, dtype, n = nbands, size = dsize)*scalar

    }
  }
  close.connection(fcon)
  cat(str(x))
  return(x)
}

# paste path and file name of isimip climate data
paste.isimip3b.clm.fn <- function(path, gcm, scen, var, syear, eyear) {
  paste0(path,
         paste(scen, gcm,
               paste(var, tolower(gcm), scen,
                     paste(syear, eyear, sep = "-"), sep = "_"), sep = "/"), ".clm")
}



# Get temperature file from isimip clm data
get.isimip.tas <- function(GCM, SC, SY, EY) {

  if (SC == "obsclim") {

    FY1 <- 1901
    LY1 <- 2016

  } else {

    # If no overlap historical / future, read only one file
    # If overlap historical / future need to read two files
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


  # Read Climate Data ----
  # ------------------------------------------------------#
  # If no overlap historical / future, read only one file
  # If overlap historical / future need to read two files
  if (SC != "obsclim" && SY <= 2014 & EY > 2014) {

    # Climate file names
    tas_fn1 <- paste.isimip3b.clm.fn(isimip3b.path, GCM, SC1, "tas", FY1, LY1)
    tas_fn2 <- paste.isimip3b.clm.fn(isimip3b.path, GCM, SC2, "tas", FY2, LY2)

    # Tas ----
    # Read 20-years daily climate from .clm files
    tas1 <- read.climate.input(tas_fn1, ncells = NCELLS, ryear = FY1,
                               fyear = SY, lyear = 2014, header = 43,
                               nbands = 365, dtype = "integer", scalar = 0.1)
    # Read 20-years daily climate from .clm files
    tas2 <- read.climate.input(tas_fn2, ncells = NCELLS, ryear = FY2,
                               fyear = 2015, lyear = EY, header = 43,
                               nbands = 365, dtype = "integer", scalar = 0.1)

    # Collate period 1 and 2 in one array
    tas <- array(NA, dim = c(NCELLS, 365, length(SY:EY)))
    tas[1:NCELLS, 1:365, 1:dim(tas1)[3]] <- tas1
    tas[1:NCELLS, 1:365, (dim(tas1)[3]+1):(dim(tas)[3])] <- tas2
    rm(tas1, tas2)

  } else {

    # Climate file names
    tas_fn <- paste.isimip3b.clm.fn(isimip3b.path, GCM, SC, "tas", FY1, LY1)

    # Tas ----
    # Read 20-years daily climate from .clm files
    tas <- read.climate.input(tas_fn, ncells = NCELLS, ryear = FY1,
                              fyear = SY, lyear = EY, header = 43,
                              nbands = 365, dtype = "integer", scalar = 0.1)
  }

  return(tas)

} # get.isimip.tas()


