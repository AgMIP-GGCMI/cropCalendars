# paste path and file name of isimip climate data
paste.isimip3b.clm.fn <- function(path, gcm, scen, var, syear, eyear) {
  paste0(path,
         paste(scen, gcm,
               paste(var, tolower(gcm), scen,
                     paste(syear, eyear, sep = "-"), sep = "_"), sep = "/"), ".clm")
}


# Get temperature file from isimip clm data
get.isimip.tas <- function(GCM, SC, SY, EY) {
  
  # If no overlap historical / future, read only one file
  # If overlap historical / future need to read two files
  if (EY <= 2014) {
    FY1 <- 1850
    LY1 <- 2014
    SC  <- "historical"
  } else if (EY > 2014 & SY <= 2014) {
    FY1 <- 1850
    LY1 <- 2014
    SC1 <- "historical"
    FY2 <- 2014
    LY2 <- 2100
    SC2 <- SC
  } else {
    FY1 <- 2014
    LY1 <- 2100
  }
  
  # Read Climate Data ----
  # ------------------------------------------------------#
  # If no overlap historical / future, read only one file
  # If overlap historical / future need to read two files
  if (SY <= 2014 & EY > 2014) {
    
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


