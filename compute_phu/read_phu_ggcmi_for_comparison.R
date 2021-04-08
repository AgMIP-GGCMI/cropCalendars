# ------------------------------------------------------# ----
# Project: lpjmlToolKit
#
# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
#
# Date Created: 2021-04-08
#
# Description: 
# ------------------------------------------------------#

rm(list=ls(all=T))


# sowing date and harvest date input (also multiple years)
read.crop.dates.input <- function(fname, ncells = NCELLS,
                                  ryear = RYEAR, fyear = FYEAR, lyear = LYEAR,
                                  header = HEADER, nbands = NBANDS,
                                  dtype = DTYPE, dsize = DSIZE, scalar = SCALAR) {
  print(paste("Reading crop dates input:", fname))
  
  #number of years
  nyears <- lyear-fyear+1
  
  # check file size
  check.fs <- (file.size(fname)-header)/ncells/dsize/nyears==nbands
  print(paste("File size as expected = ", check.fs))
  
  fcon <- file(fname, open = "rb")
  seek(fcon, header, origin = "start")
  
  x <- array(NA, dim = c(ncells, nbands, nyears))
  
  for (j in 1:nyears) {
    
    year <- fyear+(j-1)
    seek(fcon, where = header+((year-ryear)*ncells*nbands*dsize), origin = "start")
    
    for (i in 1:ncells) {
      x[i,,j] <- readBin(fcon, what = dtype, size = dsize , n = nbands)*scalar
    }
  }
  close.connection(fcon)
  print(str(x))
  return(x)
}

# ----

# Read pixel data from ggcmi_ph3 input data ####
# As a reference to check my scripts compute phu right

fpath <- paste0("/home/minoli/git_lpjmlToolKit/development/functions/")
fsources <- paste0(fpath, list.files(fpath, pattern = ".R", recursive = T))
invisible(lapply(fsources, source))
cat("\nImporting functions:\n", "-----------------",
    list.files(fpath, pattern = ".R", recursive = T), sep = "\n")      


ggcmi.dir <- "/p/projects/lpjml/input/crop_calendar/"

grid.fn  <- "/p/projects/lpjml/input/historical/input_VERSION2/grid.bin"
sdate.fn <- paste0(ggcmi.dir, "sdates_ggcmi_phase3_v1.01_67420.clm")
hdate.fn <- paste0(ggcmi.dir, "mdates_ggcmi_phase3_v1.01_67420.clm")
phu.fn   <- paste0(ggcmi.dir, "phu_ukesm1-0-ll_historical_1984-2014_ggcmi_phase3_v1.01_67420.clm")


read.lpjml.longheader(sdate.fn)
read.lpjml.longheader(hdate.fn)
read.lpjml.longheader(phu.fn)

grid.in <- read.lpjml.grid(grid.fn)


sdate <- read.crop.dates.input(sdate.fn, 67420, ryear = 2000, fyear = 2000, lyear = 2000, header = 43, nbands = 30, dtype = integer(), dsize = 2, scalar = 1)

hdate <- read.crop.dates.input(hdate.fn, 67420, ryear = 2000, fyear = 2000, lyear = 2000, header = 43, nbands = 30, dtype = integer(), dsize = 2, scalar = 1)

phu <- read.crop.dates.input(phu.fn, 67420, ryear = 2000, fyear = 2000, lyear = 2000, header = 43, nbands = 30, dtype = integer(), dsize = 2, scalar = 1)