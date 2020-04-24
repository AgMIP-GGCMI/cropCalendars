# macmit_intensification

# Functions ----

# Reading input/outputs
# get lpjml inout/output data generic
get.data.bin <- function(path, filename,
                         ncells=67420, nbands=365,
                         ryear=1901, syear=1901, eyear=2005, nyears=(eyear-syear+1),
                         header=43, datatype=integer(), datasize=2, scalar=0.01) {
  
  f.var <- file(paste(path, filename, sep=""),"rb")
  #seek(f.var, where=header, start="origin")
  seek(f.var, where=header+((syear-ryear)*ncells*nbands), origin="start")
  var <- readBin(f.var, what = datatype, size=datasize, n=ncells*nbands*nyears)*scalar
  close(f.var)
  return(var)
}

# grid input
read.grid <- function(fname, ncells = NCELLS, header = HEADER, nbands = NBANDS, dtype = DTYPE, dsize = DSIZE, scalar = SCALAR) {
  
  print(paste("Reading grid.bin input:", fname))
  
  # check file size
  check.fs <- (file.size(fname)-header)/ncells/dsize==nbands
  print(paste("File size as expected = ", check.fs))
  
  # read out data
  fcon <- file(fname, open = "rb")
  seek(con = fcon, where = header, origin = "start")
  x <- array(NA, c(ncells, nbands))
  for (i in 1:ncells){
    x[i,] <- readBin(fcon, what = dtype, size = dsize , n = nbands)*scalar
  }
  close.connection(fcon)
  print(str(x))
  return(x)
}


# sowing date and harvest date input (also multiple years)
read.crop.dates.input <- function(fname, ncells = NCELLS, ryear = RYEAR, fyear = FYEAR, lyear = LYEAR, header = HEADER, nbands = NBANDS, dtype = DTYPE, dsize = DSIZE, scalar = SCALAR) {
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

# Fertilizer input
read.fertilizer.input <- function(fname, ncells = NCELLS, ryear = RYEAR, fyear = FYEAR, lyear = LYEAR, header = HEADER, nbands = NBANDS, dtype = DTYPE, dsize = DSIZE, scalar = SCALAR) {
  print(paste("Reading fertilizer input:", fname))
  
  #number of years
  nyears <- lyear-fyear+1
  
  # check file size
  print(paste("NBANDS =",sprintf("%.10f", ((file.size(fname)-header)/ncells/dsize/(lyear-ryear+1))))) # need to change it, lyear!=lyear of the file...
  
  # read out data
  fcon <- file(fname, open = "rb")
  
  x <- array(NA, c(ncells, nbands, nyears))
  
  for (j in 1:nyears) {
    
    year <- fyear+(j-1)
    seek(fcon, where = header+((year-ryear)*ncells*nbands*dsize), origin = "start")
    
    for (i in 1:ncells){
      
      x[i,,j] <- readBin(fcon, what = dtype, size = dsize , n = nbands)*scalar
      
    } # i
  } # j
  
  close.connection(fcon)
  print(str(x))
  return(x)
}

# Climate input
read.climate.input <- function(fname, ncells = NCELLS, ryear = RYEAR, fyear = FYEAR, lyear = LYEAR, header = HEADER, nbands = NBANDS, dtype = DTYPE, dsize = DSIZE, scalar = SCALAR) {
  print(paste("Reading climate input:", fname))
  
  #number of years
  nyears <- lyear-fyear+1
  
  # check file size
  print(paste("NBANDS =",sprintf("%.10f", ((file.size(fname)-header)/ncells/dsize/(lyear-ryear+1))))) # need to change it, lyear!=lyear of the file...
  
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
  print(str(x))
  return(x)
}

# Pft-specific outputs
read.pft.specific.output <- function(fname, ncells = NCELLS, ryear = RYEAR, fyear = FYEAR, lyear = LYEAR, nbands = NBANDS, dtype = DTYPE, dsize = DSIZE, scalar = SCALAR) {
  
  #number of years
  nyears <- lyear-fyear+1
  
  # check file size
  print(paste("File size as expected = ", file.size(fname)/ncells/nbands/dsize==(lyear-ryear+1)))
  
  sdatef <- file(fname,"rb")
  x <- array(data = NA, dim = c(ncells, nbands, nyears) )
  
  for (j in 1:nyears) {
    
    year <- fyear+(j-1)
    
    seek(sdatef, where = (year-ryear)*ncells*nbands*dsize, origin = "start")
    
    for (i in 1:nbands) {
      
      x[,i,j] <- readBin(sdatef, dtype, n = ncells, size = dsize)
      
    }
  }
  
  close.connection(sdatef)
  print(str(x))
  return(x)
}



# https://redmine.pik-potsdam.de/projects/lpjml/wiki/Adding_new_input_files
# for header names see header.h
fwriteheader <- function(file.out,headername,version,bands,firstyear,nyears,ncells,scalar){
  writeChar(headername,file.out,eos=NULL)
  writeBin(as.integer(version),file.out,size=4,endian=.Platform$endian)   # CLIMATE VERSION
  writeBin(as.integer(1),file.out,size=4,endian=.Platform$endian)       # ORDER
  writeBin(as.integer(firstyear),file.out,size=4,endian=.Platform$endian) # FIRSTYEAR
  writeBin(as.integer(nyears),file.out,size=4,endian=.Platform$endian)       # NYEAR
  writeBin(as.integer(0),file.out,size=4,endian=.Platform$endian)       # FIRSTCELL
  writeBin(as.integer(ncells),file.out,size=4,endian=.Platform$endian)       # NCELL
  writeBin(as.integer(bands),file.out,size=4,endian=.Platform$endian)       # NBAND
  writeBin(0.5,file.out,size=4,endian=.Platform$endian)               # CELLSIZE
  writeBin(scalar,file.out,size=4,endian=.Platform$endian)               # SCALAR
}

# Write header version 3
fwriteheader3 <- function(file.out,headername,bands,scalar,fy=years[1],ny=length(years)){
  writeChar(headername,file.out,eos=NULL)
  writeBin(as.integer(3),file.out,size=4,endian=.Platform$endian) # CLIMATE VERSION
  writeBin(as.integer(1),file.out,size=4,endian=.Platform$endian)  # ORDER
  writeBin(as.integer(fy),file.out,size=4,endian=.Platform$endian)     # FIRSTYEAR
  writeBin(as.integer(ny),file.out,size=4,endian=.Platform$endian)     # NYEAR
  writeBin(as.integer(cell67420-1),file.out,size=4,endian=.Platform$endian)     # FIRSTCELL
  writeBin(as.integer(1),file.out,size=4,endian=.Platform$endian)  # NCELL
  writeBin(as.integer(bands),file.out,size=4,endian=.Platform$endian)     # BAND
  writeBin(1/9,file.out,size=4,endian=.Platform$endian)     # CELLSIZE
  writeBin(scalar,file.out,size=4,endian=.Platform$endian)     # SCALAR
  writeBin(1/9,file.out,size=4,endian=.Platform$endian)     # CELLSIZE
  writeBin(as.integer(3),file.out,size=4,endian=.Platform$endian) # DATA TYPE 3=float
}




