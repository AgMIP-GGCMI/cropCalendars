# ------------------------------------------------------#
# Project: crop_calendars
#
# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
#
# Date Created: 2020-11-02
#
# Description: Read / Write NCDF4
# ------------------------------------------------------#

#readNcdfFile() by default reads only first year (or day)
read.ncdf <- function(fname, st = c(1,1,1), ct = c(720, 360, 1), var.id = NA, dimnames.only = FALSE) {
  
  require(ncdf4)
  
  nf <- nc_open(fname)
  #print(nf)
  vars <- names(nf$var)
  
  laidx <- grep("lat", attributes(nf$dim)$names)
  loidx <- grep("lon", attributes(nf$dim)$names)
  timeidx <- grep("time", attributes(nf$dim)$names)
  la <- round(ncvar_get( nf, attributes(nf$dim)$names[laidx]), 4)
  lo <- round(ncvar_get( nf, attributes(nf$dim)$names[loidx]), 4)
  
  #la <- round(ncvar_get(nf, varid = "lat"), 2)
  #lo <- round(ncvar_get(nf, varid = "lon"), 2)
  
  if (length(st)>2) {
    #ti <- ncvar_get(nf, varid = "time")
    ti <- ncvar_get(nf, attributes(nf$dim)$names[timeidx])
    di <- list("lon" = lo, "lat" = la, "time" = ti)
  } else {
    di <- list("lon" = lo, "lat" = la)
  }
  if (is.na(var.id)) varid <- vars[1]
  nc <- ncvar_get(nf, varid = var.id, start = st, count = ct)
  dimnames(nc) <- di[1:length(dim(nc))]
  nc_close(nf)
  
  if (dimnames.only == TRUE) {
    return(di)
  } else {
    return(nc) 
  }
}

# Example
# FNAME <- "C:/Users/minoli/Documents/Work/PROJECTS/MAPPY/DATA/CLIMATE/climat_input_test_lpjml/climat_input_test_lpjml/tas_CEUR0275_MPIESMLR-hist_MAPPYCEU_UKassel_CCLM-5-0-16_dm_19800101-19800131.nc"
# clm.var <- read.ncdf(FNAME, ct = c(594, 454, 31), var.id = "T_2M")
# lons <- read.ncdf(FNAME, ct = c(594, 454, 31), var.id = "T_2M", dimnames.only = TRUE)$lon
# lats <- read.ncdf(FNAME, ct = c(594, 454, 31), var.id = "T_2M", dimnames.only = TRUE)$lat

# Get grid array from ncdf cube (to build grid file for unusual grid)
nccube.to.lpjml.grid <- function(ARin,
                                 lons,
                                 lats){
  
  cat("\nlength(lons) = ", length(lons), "\nlength(lats) = ", length(lats))
  
  ARgrid <- NULL
  for (i in 1:length(lons)) {
    for (j in 1:length(lats)) {
      val <- ARcube[i,j,t]
      if (!is.na(val)) {
        ARgrid <- rbind(ARgrid, c(lons[i], lats[j]))
        cat(lons[i], lats[j], "\n")
      }
    }
  }
  return(ARgrid)
}

# Example
# ARin <- read.ncdf(FNAME, ct = c(594, 454, 31), var.id = "T_2M")
# lons <- read.ncdf(FNAME, ct = c(594, 454, 31), var.id = "T_2M", dimnames.only = TRUE)$lon
# lats <- read.ncdf(FNAME, ct = c(594, 454, 31), var.id = "T_2M", dimnames.only = TRUE)$lat


# convert lpjml 67420 rows array to a lon / lat cube
lpjml.array.to.nccube <- function(ARin,
                                  ARgrid,
                                  years = c(1901:1903),
                                  lons = seq(-179.75, 179.75, by=0.5),
                                  lats = seq(-89.75,   89.75, by=0.5) ){
  nrows  <- length(lons)
  ncols  <- length(lats)
  nyears <- length(years)
  
  
  if (nyears==1) {
    ARout <- array(NA, dim = c(nrows, ncols), dimnames = list(lons, rev(lats)))
  } else {
    ARout <- array(NA, dim = c(nrows, ncols, nyears), dimnames = list(lons, rev(lats), years))
  }
  for (i in 1:nrow(ARgrid)) {
    
    ilon <- as.character(ARgrid[i,1])
    ilat <- as.character(ARgrid[i,2])
    
    if (nyears==1) {
      ARout[ilon,ilat] <- ARin[i]
    } else {
      ARout[ilon,ilat,] <- ARin[i,] 
    }
    
  }
  return(ARout)  
}





# CREATE NCDF4
write.ncdf.1cube <- function(cube, fname = FNAME, cropname = CROPNAME,
                             varshort = VARSHORT, varlong = VARLONG,
                             years = YEARS, unit = UNIT,
                             precision = "float", compr = 6,
                             lons = seq(-179.75, 179.75, by=0.5),
                             lats = seq(-89.75,   89.75, by=0.5),
                             author = "Sara Minoli") {
  
  # file name
  ncfname <- paste0(fname, ".nc4")
  
  londim <- ncdim_def("lon", "degrees_east", lons)
  latdim <- ncdim_def("lat", "degrees_north", rev(lats))
  timdim <- ncdim_def("year", "year", years)
  
  # define variables
  fillvalue <- 1e32
  #dlname <- variable
  var_def  <- ncvar_def(name=varshort, units=unit,
                        dim=list(londim, latdim, timdim),
                        missval=fillvalue, longname = varlong,
                        prec=precision, compression = compr)
  
  # create netCDF file and put arrays
  ncout <- nc_create(ncfname, list(var_def), force_v4=T, verbose = F)
  
  # put variables
  ncvar_put(ncout, var_def, cube)
  
  # put additional attributes into dimension and data variables
  ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"lat","axis","Y")
  
  # add global attributes
  #ncatt_put(ncout, 0, "Description", DESCRIPTION)
  ncatt_put(ncout, 0, "Crop", cropname)
  ncatt_put(ncout, 0, "Institution", "Potsdam Institute for Climate Impact Research (PIK), Germany")
  #ncatt_put(ncout, 0, "References", "Minoli et al.")
  history <- paste(author, date(), sep=", ")
  ncatt_put(ncout, 0, "history", history)
  
  # close the file, writing data to disk
  nc_close(ncout)
  
}
