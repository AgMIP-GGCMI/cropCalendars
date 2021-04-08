# Read grid input ----
read.lpjml.grid <- function(fname = "grid/path/grid.bin",
                            band_names = c("lon", "lat"),
                            ncells = 67420,
                            header = 43,
                            nbands = 2,
                            dtype =  integer(),
                            dsize =  2,
                            scalar = .01) {
  
  cat(paste("\nReading grid.bin (input/output):\n-------------------------------\n", fname))
  
  # check file size
  check.fs <- (file.size(fname)-header)/ncells/dsize==nbands
  cat(paste("\nFile size as expected = ", check.fs, "\n"))
  
  # read out data
  fcon <- file(fname, open = "rb")
  seek(con = fcon, where = header, origin = "start")
  x <- array(NA, c(ncells, nbands))
  for (i in 1:ncells){
    x[i,] <- readBin(fcon, what = dtype, size = dsize , n = nbands)*scalar
  }
  close.connection(fcon)
  colnames(x) <- band_names
  cat(str(x))
  return(x)
}

