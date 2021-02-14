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