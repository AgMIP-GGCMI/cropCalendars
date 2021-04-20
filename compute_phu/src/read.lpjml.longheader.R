# Read header ----
read.lpjml.longheader <- function(filename, print=T, version=2){
  # get the cell number
  header <- data.frame(name="",version=NA,order=NA,firstyear=NA,nyear=NA,
                       firstcell=NA,ncell=NA,bands=NA,cellsize=NA,scalar=NA,cellsize2=NA,datatype=NA)
  zz <- file(filename,"rb")
  header$name      <-readChar(zz,7)
  header$version   <- readBin(zz,integer(),size=4,n=1)
  header$order     <- readBin(zz,integer(),size=4,n=1)
  header$firstyear <- readBin(zz,integer(),size=4,n=1)
  header$nyear     <- readBin(zz,integer(),size=4,n=1)
  header$firstcell <- readBin(zz,integer(),size=4,n=1)
  header$ncell     <- readBin(zz,integer(),size=4,n=1)
  header$bands     <- readBin(zz,integer(),size=4,n=1)
  header$cellsize  <- readBin(zz,double(),size=4,n=1)
  header$scalar    <- readBin(zz,double(),size=4,n=1)
  if(version>2){
    header$cellsize2 <- readBin(zz,double(),size=4,n=1)
    header$datatype  <- readBin(zz,integer(),size=4,n=1)
  }
  # size header version 2 == 43
  # size header version 3 == 51
  
  close(zz)
  if(print)
  {
    cat("name:",header$name,"\n")
    cat("Version:",header$version,"\n")
    cat("order:",header$order,"\n")
    cat("firstyear:",header$firstyear,"\n")
    cat("nyear:",header$nyear,"\n")
    cat("firstcell:",header$firstcell,"\n")
    cat("ncell:",header$ncell,"\n")
    cat("bands:",header$bands,"\n")
    cat("cellsize:",header$cellsize,"\n")
    cat("scalar:",header$scalar,"\n")
    if(version>2){
      cat("cellsize2:",header$cellsize,"\n")
      cat("datatype:",header$datatype,"\n")
    }
  }
  header
}

