#-----------------------------------------------------------#
# Climate-driven sowing and harvest dates                   #
# R-Code                                                    #
# Written by Sara Minoli                                    #
# Based on Minoli et al. (2019) Global and Planetary Change #
#-----------------------------------------------------------#

#-----------------------------------------------------------#
# PREPARE MONTHLY CLIMATE #
#-----------------------------------------------------------#
# prepare climate data for sowing and harvest dates estimation
# RUN TIME: 20 min

print(Sys.time())

rm(list=ls(all=TRUE))

working.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate/"


# Read Configuration ----
# ------------------------------------------------------#

source(paste0(working.dir, "configuration/configuration_ggcmi_ph3.R"))

isimip3b.path  <- "/p/projects/lpjml/input/scenarios/ISIMIP3b/" # .clm climate
tmp.dir  <- paste0(climate.dir, "tmp/")
plot.dir <- paste0(climate.dir, "plots/")
save.tmp.file <- FALSE # If T, do not delete tmp PET file (for testing only)

# Import Job Arguments ----
# ------------------------------------------------------#
options(echo=FALSE) # if want see commands in output file
args <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
print(args)
# args <- 1

# select variable, crop, model
batch.df1 <- batch.df[args,]
print(batch.df1)
GCM  <- batch.df1$gcm
SC   <- batch.df1$scenario
SY   <- batch.df1$syear
EY   <- batch.df1$eyear
FY   <- batch.df1$fyear
LY   <- batch.df1$lyear

years  <- c(SY:EY)
nyears <- length(years)


# Import Functions ----
# ------------------------------------------------------#
M_1_PI <- 0.318309886183790671538
M_PI   <- 3.14159265358979323846

source(paste0(working.dir, "src/units.R"))
source(paste0(working.dir, "src/petpar.R"))
source(paste0(working.dir, "src/read.climate.input.R"))
source(paste0(working.dir, "src/array2dataTable.R"))
source(paste0(working.dir, "src/ggplot.map.general.R"))

# paste path and file name of isimip climate data
paste.ismip3b.clm.fn <- function(path, gcm, scen, var, syear, eyear) {
  paste0(path,
         paste(scen, gcm,
               paste(var, tolower(gcm), scen,
                     paste(syear, eyear, sep = "-"), sep = "_"), sep = "/"), ".clm")
}


# Grid ----
# --------------------------# 
NCELLS <- nrow(coord.df)
lon <- coord.df[,1]
lat <- coord.df[,2]

# vector of full date time line ----
# --------------------------# 
# get dates from a non-leap year
vdate <- seq(from = strptime("1987-01-01 00:00:00",
                             format = "%Y-%m-%d %H:%M:%S", tz = "CET"),
             to = strptime("1987-12-31 00:00:00",
                           format = "%Y-%m-%d %H:%M:%S", tz = "CET"),
             by = "day")
# vectors of time line in weather file 1
vyear <- sprintf("%04d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$year+1900 ))
vmon  <- sprintf("%02d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$mon+1 ))
vday  <- sprintf("%02d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$mday ))
vdoy  <- sprintf("%02d", as.numeric(strptime(vdate, "%Y-%m-%d %H:%M:%S")$yday+1 ))
vyear.mon.day <- paste0(vyear, vmon, vday)


# Read Climate Data ----
# ------------------------------------------------------#
# Climate file names
tas_fn <- paste.ismip3b.clm.fn(isimip3b.path, GCM, SC, "tas", FY, LY)
pr_fn  <- paste.ismip3b.clm.fn(isimip3b.path, GCM, SC, "pr",  FY, LY)

# Tas and Pr ----
# Read 20-years daily climate from .clm files
tas <- read.climate.input(tas_fn, ncells = NCELLS, ryear = FY,
                          fyear = SY, lyear = EY, header = 43,
                          nbands = 365, dtype = "integer", scalar = 0.1)
pr  <- read.climate.input(pr_fn,  ncells = NCELLS, ryear = FY,
                          fyear = SY, lyear = EY, header = 43,
                          nbands = 365, dtype = "integer", scalar = 0.1)

# Compute PET (Potential ET) ----
cat("Computing daily PET...\n")
pet <- array(NA, dim = dim(tas))
for (la in 1:length(lat)) {
  if (la%%1000==0) cat(la, "\t")
  for (dd in 1:365) {
    pet[la,dd,] <- petpar(temp = tas[la,dd,], lat = lat[la], day = dd)[[3]]
  }
}

# Save PET temporarily (takes some time to compute)
save(pet, file = paste0(tmp.dir, "pet.Rdata"))


# Compute Monthly Statistics ----
# ------------------------------------------------------#
# Compute monthly values for each year
cat("Computing monthly values...\n")
tas.mavg.y <- array(NA, dim = c(NCELLS, 12, nyears))
pr.mavg.y <- pet.mavg.y <- ppet.mavg.y <- tas.mavg.y
for (yy in 1:nyears) {
  for (mm in 1:12) {
    
    dom <- which(vmon==sprintf("%02d", mm)) # which days belong to month mm
    
    tas.mavg.y[,mm,yy]  <- apply(tas[,dom,yy], 1, mean) # mean temp
    pr.mavg.y[,mm,yy]   <- apply(pr[,dom,yy], 1, sum)   # cumulative pr
    pet.mavg.y[,mm,yy]  <- apply(pet[,dom,yy], 1, sum)  # cumulative pet
    ppet.mavg.y[,mm,yy] <- pr.mavg.y[,mm,yy]/pet.mavg.y[,mm,yy] # ppet ratio
    
  }
}

# Compute 20-years Average ----
# ------------------------------------------------------#
cat("Computing 20-years average...\n")
tas.mavg  <- round(apply(tas.mavg.y,  c(1,2), mean), digits = 5)
pr.mavg   <- round(apply(pr.mavg.y,   c(1,2), mean), digits = 5)
pet.mavg  <- round(apply(pet.mavg.y,  c(1,2), mean), digits = 5)
ppet.mavg <- round(apply(ppet.mavg.y, c(1,2), mean), digits = 5)
dimnames(tas.mavg) <- list("pixelnr" = c(1:NCELLS), "month" = c(1:12))
dimnames(pr.mavg) <- dimnames(pet.mavg) <- dimnames(ppet.mavg) <- dimnames(tas.mavg)

# Convert array to data.table ----
# ------------------------------------------------------#
cat("Convert array to data.table...\n")
DTtas  <- array2dataTable(tas.mavg)
DTpr   <- array2dataTable(pr.mavg)
DTpet  <- array2dataTable(pet.mavg)
DTppet <- array2dataTable(ppet.mavg)

# Merge DTs ----
# ------------------------------------------------------#
DTclm1 <- merge(DTtas, DTpr, by = c("V1", "V2"), suffixes = c(".tas", ".pr"))
DTclm2 <- merge(DTpet, DTppet, by = c("V1", "V2"), suffixes = c(".pet", ".ppet"))

DTclm3  <- merge(DTclm1, DTclm2, by = c("V1", "V2"))
colnames(DTclm3) <- c("pixelnr", "month", "tas", "pr", "pet", "ppet")

DTgrid <- data.table(lon = coord.df[,1], lat = coord.df[,2], pixelnr = 1:67420)

DTout <- merge(DTgrid, DTclm3, by = c("pixelnr"))


# Save DTs ----
# ------------------------------------------------------#
save(DTout, file = paste0(climate.dir, "DT_average_monthly_climate_", GCM, "_", SC, "_", SY, "_", EY, ".Rdata"))


# Plot Maps ----
# ------------------------------------------------------#

# args <- 1
# 
# # select variable, crop, model
# batch.df1 <- batch.df[args,]
# GCM  <- batch.df1$gcm
# SC   <- batch.df1$scenario
# SY   <- batch.df1$syear
# EY   <- batch.df1$eyear
# FY   <- batch.df1$fyear
# LY   <- batch.df1$lyear

if (T) {
  cat("Plotting monthly climate...\n")
  DTclm <- get(load(file = paste0(climate.dir, "DT_average_monthly_climate_", GCM, "_", SC, "_", SY, "_", EY, ".Rdata")))
  
  pdf(paste0(plot.dir, "map_climate_", "tas_", GCM,"_",SC,"_",SY,"_",EY,".pdf"),
      width = a4w, height = a4h)
  p_tas  <- ggplot.map.general(DTclm, "tas",  limits = c(-10,30),
                               color_scale = rev(brewer.pal(11,"Spectral")))
  p_tas <- p_tas + facet_wrap(~month, nrow = 6)
  p_tas <- p_tas + labs(subtitle = paste(c("min", "max"), range(DTclm$tas), "(degC)", collapse = " "))
  print(p_tas)
  dev.off()
  
  pdf(paste0(plot.dir, "map_climate_", "pr_", GCM,"_",SC,"_",SY,"_",EY,".pdf"),
      width = a4w, height = a4h)
  p_pr   <- ggplot.map.general(DTclm, "pr",   limits = c(0, 200),
                               color_scale = brewer.pal(11,"Spectral"))
  p_pr <- p_pr + facet_wrap(~month, nrow = 6)
  p_pr <- p_pr + labs(subtitle = paste(c("min", "max"), range(DTclm$pr), "(mm)", collapse = " "))
  print(p_pr)
  dev.off()

  pdf(paste0(plot.dir, "map_climate_", "pet_", GCM,"_",SC,"_",SY,"_",EY,".pdf"),
      width = a4w, height = a4h)
  p_pet  <- ggplot.map.general(DTclm, "pet",  limits = c(0, 100),
                               color_scale = brewer.pal(11,"Spectral"))
  p_pet <- p_pet + facet_wrap(~month, nrow = 6)
  p_pet <- p_pet + labs(subtitle = paste(c("min", "max"), range(DTclm$pet), "(mm)", collapse = " "))
  print(p_pet)
  dev.off()
  
  pdf(paste0(plot.dir, "map_climate_", "ppet_", GCM,"_",SC,"_",SY,"_",EY,".pdf"),
      width = a4w, height = a4h)
  p_ppet <- ggplot.map.general(DTclm, "ppet", limits = c(0, 3),
                               color_scale = brewer.pal(11,"Spectral"))
  p_ppet <- p_ppet + facet_wrap(~month, nrow = 6)
  p_ppet <- p_ppet + labs(subtitle = paste(c("min", "max"), range(DTclm$ppet), "(-)", collapse = " "))
  print(p_ppet)
  dev.off()
}

if(save.tmp.file == F) file.remove(paste0(tmp.dir, "pet.Rdata"))

 cat("\n--------\nDone !\n--------\n")
