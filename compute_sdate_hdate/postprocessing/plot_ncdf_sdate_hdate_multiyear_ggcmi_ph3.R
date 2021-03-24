# ------------------------------------------------------# ----
# Project: GGCMI ph3 Adaptation
#
# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
#
# Date Created: 2021-03-04
#
# Description: Plot crop calendar time series
# ------------------------------------------------------#

rm(list=ls(all=T))

# ----
library(ncdf4)
library(maps)
library(fields)
library(RColorBrewer)
# ----

args <- commandArgs(T)
CROP   <- args[1]
IRRIG  <- args[2]
GCM    <- args[3]
SC     <- args[4]
VAR    <- args[5]
#CROP <- "mai"; IRRIG <- "rf"; GCM <- "UKESM1-0-LL"; SC <- "ssp126"; VAR <- "plant-day"

print(CROP)

working.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate/"
source(paste0(working.dir, "configuration/configuration_ggcmi_ph3.R"))
source(paste0(working.dir, "src/compute_sdate_hdate_functions.R"))

ncdir <- paste0(project.dir, "crop_calendars/ncdf/", GCM, "/", SC, "/")
pldir <- paste0(project.dir, "crop_calendars/plots/")

SYs <- seq(1991, 2091, by = 10)    # Start of the period
EYs <- seq(2000, 2100, by = 10)    # End of the period
YEARS <-  min(SYs):max(EYs)
NYEARS <- length(YEARS)


fn <- paste0(CROP, "_", IRRIG, "_", GCM, "_", SC,"_", min(SYs), "-", max(EYs),
             "_ggcmi_ph3_rule_based_crop_calendar")
FNAME <- paste0(ncdir, fn, ".nc4")

nf <- nc_open(FNAME)
ncsd <- ncvar_get(nf, "plant-day", c(1,1,1), c(720,360,NYEARS))
nchd <- ncvar_get(nf, "maty-day", c(1,1,1), c(720,360,NYEARS))
nc_close(nf)

lons <- seq(-179.75, 179.75, by = 0.5)
lats <- seq(-89.75 , 89.75,  by = 0.5)
dimnames(ncsd) <- list(lon = lons, lat = lats, time = YEARS)
dimnames(nchd) <- list(lon = lons, lat = lats, time = YEARS)

ncgd <- computeGrowingPeriod(ncsd, nchd, 365)

if (VAR=="plant-day") {
nc <- ncsd
} else if (VAR=="maty-day") {
nc <- nchd
} else if (VAR=="grow-days") {
nc <- ncgd
} else {cat("Invalid variable: did you mean plant_day / maty-day / grow-days ?")}

# Maps diff 
difflim <- 100
iyears <- which(YEARS%in%SYs)
pdf(paste0(pldir, fn, "_", VAR, ".pdf"), width = 7, height = 3)

layout(matrix(1:12, nrow = 2, byrow = T))
par(mar = c(0, 0, 0, 0))
image.plot(nc[,,iyears[1]], horizontal = T, axes = F, x = lons, y = lats)
text(x = 0, y = -50, labels = paste(VAR, IRRIG, CROP))
for (yy in 2:length(iyears)) {
  #yy <- which(YEARS%in%SYs)[1]
  mp <- nc[,,iyears[yy]]-nc[,,1]
  mp[mp<(-difflim)] <- (-difflim)
  mp[mp>difflim] <- difflim
  par(mar = c(0, 0, 0, 0))
  image.plot(mp, horizontal = T, axes = F, x = lons, y = lats,
             col = brewer.pal(11,'Spectral'))
  text(x = 0, y = -50, labels = EYs[yy])
}

dev.off()


plons <- as.character(-91.75)
plats <- as.character(seq(30.25, 45.25, by = 3))

#plot(YEARS, nc["-92.25", "30.25",], type = "l", ylab = "DOY")
#lines(YEARS, nc["-92.25", "32.25",], type = "l", ylab = "DOY")
pdf(paste0(pldir, "pix_", fn, "_", VAR, ".pdf"), width = 7, height = 6)
lo <- 1; la <- 1

plot(YEARS, nc[plons[lo], plats[la],], type = "l", ylab = VAR,
     xlim = c(YEARS[1], 2140), ylim = c(1,365), main = paste(CROP, IRRIG))
#text(2060, 355, paste(CROP, IRRIG))
text(2130, nc[plons[lo], plats[la], NYEARS],
     paste(plons[lo], plats[la]))

for (la in 2:length(plats)) {
  lines(YEARS, nc[plons[lo], plats[la],], type = "l", ylab = "DOY", col = la)
  text(2130, nc[plons[lo], plats[la], NYEARS],
       paste(plons[lo], plats[la]), col = la)
}

dev.off()

