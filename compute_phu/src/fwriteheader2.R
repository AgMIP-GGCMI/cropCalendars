# Write header version 2 ----
fwriteheader2 <- function(file.out,     # "filename.bin"
                          headername,   # "LPJGRID", "LPJCLIM", "LPJLUSE", "LPJSOWD"
                          version = 2,
                          bands,        # 24, 32, 64, etc
                          firstyear,
                          nyears,
                          ncells = 67420,
                          resolution = 0.5, # spatial res. degrees
                          scalar){
  writeChar(headername,file.out,eos=NULL)
  writeBin(as.integer(version),file.out,size=4,endian=.Platform$endian)   # CLIMATE VERSION
  writeBin(as.integer(1),file.out,size=4,endian=.Platform$endian)     # ORDER
  writeBin(as.integer(firstyear),file.out,size=4,endian=.Platform$endian) # FIRSTYEAR
  writeBin(as.integer(nyears),file.out,size=4,endian=.Platform$endian)    # NYEAR
  writeBin(as.integer(0),file.out,size=4,endian=.Platform$endian)     # FIRSTCELL
  writeBin(as.integer(ncells),file.out,size=4,endian=.Platform$endian)    # NCELL
  writeBin(as.integer(bands),file.out,size=4,endian=.Platform$endian)     # NBAND
  writeBin(resolution,file.out,size=4,endian=.Platform$endian)               # CELLSIZE
  writeBin(scalar,file.out,size=4,endian=.Platform$endian)            # SCALAR
  
}
