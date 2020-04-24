###########################################################

# Compute average monthly temperature of historical climate
# From AgMERRA dataset (monthly)

###########################################################

rm(list=ls(all=TRUE))

oncluster <- TRUE

if(oncluster==TRUE) {
  working.dir <- "/p/projects/macmit/users/minoli/PROJECTS/CROP_PHENOLOGY_v01/SCRIPTS/GROWING_PERIODS_PACKAGE/"
  agmerra.dir <- "/p/projects/macmit/data/GGCMI/AgMIP.input/AgMERRA/monthly/"
} else {
  working.dir <- "D:/PROJECTS/GROWING_PERIODS_PACKAGE/"
  agmerra.dir <- paste0(working.dir, "DATA/INPUT/")
}

climate.dir <- paste0(working.dir, "DATA/CLIMATE/")

# PACKAGES ----
library(ncdf4)
library(data.table)
library(ggplot2)

# FUNCTIONS ----
source(paste0(working.dir, "CODE/src/units.R"))
source(paste0(working.dir, "CODE/src/ggplot.map.general.R"))
source(paste0(working.dir, "CODE/configuration/graphics.R"))

#readOutFile() by default reads only first year (or day)
readOutFile <- function(fname, st = c(1,1,1), ct = c(720, 360, 1), varid = NA, ti = NA) {
  nf <- nc_open(fname)
  #print(nf)
  vars <- names(nf$var)
  
  #workaround for wrongly named dimensions
  # x <- round(ncvar_get(nf, varid = as.character(nf$dim[[1]][1])), 2)
  # y <- round(ncvar_get(nf, varid = as.character(nf$dim[[2]][1])), 2)
  # ifelse(length(x)==360, la <- x, la <- y)
  # ifelse(length(y)==720, lo <- y, lo <- x)
  
  laidx <- grep("lat", attributes(nf$dim)$names)
  loidx <- grep("lon", attributes(nf$dim)$names)
  timeidx <- grep("time", attributes(nf$dim)$names)
  la <- round( ncvar_get( nf, attributes(nf$dim)$names[laidx]), 2)
  lo <- round(ncvar_get( nf, attributes(nf$dim)$names[loidx]), 2)
  
  #la <- round(ncvar_get(nf, varid = "lat"), 2)
  #lo <- round(ncvar_get(nf, varid = "lon"), 2)
  
  if (length(st)>2) {
    #ti <- ncvar_get(nf, varid = "time")
    if (is.na(ti[1])==TRUE) ti <- ncvar_get( nf, attributes(nf$dim)$names[timeidx])
    di <- list("lon" = lo, "lat" = la, "time" = ti)
  } else {di <- list("lon" = lo, "lat" = la)}
  if (is.na(varid)) varid <- vars[1]
  
  nc <- ncvar_get(nf, varid = varid, start = st, count = ct)
  dimnames(nc) <- di
  nc_close(nf)
  return(nc)
}

# MAIN ----
fname <- "tas_agmerra_1980-2010.mm.nc4"
fn <- paste0(agmerra.dir, fname)

NYEARS <- 2010-1980+1
DTyears <- NULL
cat("Extracting climate one year at a time from ncdf ...", "\n")
for (i in seq(1, 12*NYEARS, by = 12)) {
  cube <- k2deg(readOutFile(fname = fn, st = c(1,1,i), ct = c(720, 360, 12),
                      varid = "tas", ti = as.character(1:12)))
  
  # convert array to data.table and remove NAs
  DT1 <- setDT(melt(cube, value.name = "tas"))[!is.na(tas)]
  DTyears <- rbind(DTyears, DT1)
  cat(i, "\t")
}
rm(DT1)

# Compute monthly average across years
cat("\n", "Compute average monthly climate (DT) ...", "\n")
DT <- DTyears[, mean(tas), by = c("lon", "lat", "time")]
colnames(DT)[colnames(DT)=="V1"] <- "mtemp"


save(DT, file = paste0(climate.dir, "DT_AgMERRA_monthly_average_1980_2010", ".Rdata"))

# Plot
pdf(paste0(climate.dir, "DT_AgMERRA_monthly_average_1980_2010", ".pdf"), width = a4h, height = a4w)
p <- ggplot.map.general(DT, "mtemp")
p <- p + facet_wrap("time", ncol = 3)
p
dev.off()

cat("--- Done ! ---", "\n")
