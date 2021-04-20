# Write header version 3 ----
fwriteheader3 <- function(file.out,   # "filename.bin"
                          headername, # "LPJLUSE"
                          version = 3,
                          bands,      # 24, 32, 64, etc
                          firstyear=1,
                          nyears=1,
                          ncells = 67420,
                          resolution1 = 0.5, # spatial res. degrees
                          resolution2 = 0.5, # spatial res. degrees
                          dtype = 3,         # 0=LPJ_BYTE, 1=LPJ_SHORT, 2=LPJ_INT, 3=LPJ_FLOAT, 4=LPJ_DOUBLE -- see include/types.h
                          scalar){
  writeChar(headername,file.out,eos=NULL)
  writeBin(as.integer(version),file.out,size=4,endian=.Platform$endian)     # CLIMATE VERSION
  writeBin(as.integer(1),file.out,size=4,endian=.Platform$endian)           # ORDER
  writeBin(as.integer(firstyear),file.out,size=4,endian=.Platform$endian)   # FIRSTYEAR  
  writeBin(as.integer(nyears),file.out,size=4,endian=.Platform$endian)      # NYEAR      
  writeBin(as.integer(0),file.out,size=4,endian=.Platform$endian)           # FIRSTCELL
  writeBin(as.integer(ncell),file.out,size=4,endian=.Platform$endian)       # NCELL
  writeBin(as.integer(bands),file.out,size=4,endian=.Platform$endian)       # BAND
  writeBin(resolution1,file.out,size=4,endian=.Platform$endian)             # CELLSIZE
  writeBin(scalar,file.out,size=4,endian=.Platform$endian)                  # SCALAR
  writeBin(resolution2,file.out,size=4,endian=.Platform$endian)             # CELLSIZE
  writeBin(as.integer(dtype),file.out,size=4,endian=.Platform$endian)       # DATA TYPE 3=float
}

