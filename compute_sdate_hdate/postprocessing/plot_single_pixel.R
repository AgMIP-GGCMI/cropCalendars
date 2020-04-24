#-----------------------------------------------------------#
# Plot single pixel #
#-----------------------------------------------------------#
print(Sys.time())

rm(list=ls(all=TRUE))

oncluster <- TRUE

if(oncluster==TRUE) {
  working.dir <- "/p/projects/macmit/users/minoli/PROJECTS/CROP_PHENOLOGY_v01/SCRIPTS/GROWING_PERIODS_PACKAGE/"
} else {
  working.dir <- "D:/PROJECTS/GROWING_PERIODS_PACKAGE/"
}


# READ CONFIGURATION ----
#___________________________________________________________#
source(paste0(working.dir, "CODE/configuration/configuration.R"))

# IMPORT FUNCTIONS ----
#___________________________________________________________#
source(paste0(working.dir, "CODE/src/compute_sdate_hdate_functions.R"))
source(paste0(working.dir, "CODE/src/ggplot.map.general.R"))

# SELECT SCENARIO ----
#___________________________________________________________#

# if(oncluster==TRUE) {
#   # import argument from bash script
#   options(echo=FALSE) # if you want see commands in output file
#   args <- as.numeric(commandArgs(trailingOnly = TRUE))
# } else {
  args <- 3  
# }
print(args)

# args <- seq(1, 72, 12)[6]

# select variable, crop, model
dbatch1 <- subset(dbatch, subset = (step1!=0)) # remove scenarios that are not needed in step1 pre-lpjml
rownames(dbatch1) <- NULL

repcrops  <- rep(crops, each = nrow(dbatch1))
CROPIDX      <- which(crops==repcrops[args])
ARG       <- args-((CROPIDX-1)*nrow(dbatch1))

dbatch2 <- dbatch1[ARG,]
GCM     <- dbatch2$gcm
SCgp    <- dbatch2$scenario.growp
SYgp    <- dbatch2$syear.growp
EYgp    <- dbatch2$eyear.growp
SCclm   <- dbatch2$scenario.clm
SYclm   <- dbatch2$syear.clm
EYclm   <- dbatch2$eyear.clm

cat("\n", paste("Scenario", ARG),
    "-------------------------------------------------------", sep = "\n")
print(dbatch2)


# IMPORT CLIMATE DATA ----
#___________________________________________________________#
# get monthly climate data
cat("\n", paste("Importing climate data of:", GCM, SCgp, SYgp, EYgp),
    "-------------------------------------------------------", sep = "\n")
DTclm <- get(load(file = paste0(climate.dir, "DT_average_monthly_climate_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata")))

print(DTclm)



# IMPORT CROP CALENDAR ----
#___________________________________________________________#
CROP <- "Maize"
DTcrop <- get(load(file = paste0(output.dir, "DT_output_crop_calendars_", CROP, "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata")))

rm(DT)


crop_parameters <- subset(crop_parameters_all, subset = crop_name %in% CROP)

# SELECT PIXEL ----
#___________________________________________________________#

#DT[lon>(-75) & lon<(-50) & lat>(-30) & lat<(-20) & harvest_reason=="GPmin" & seasonality_type=="PRECTEMP"]
# coord <- COORD <- 17395
dtclm <- DTclm
dtcrop <- DTcrop

LON <- 79.25
LAT <- 17.25

PIX <- dtcrop[lon==LON & lat==LAT]

COORD <- PIX$pixelnr[1]

SD <- PIX$sowing_doy[1]
HD <- PIX$maturity_doy[1]

TBASE <- crop_parameters$temp_base_rphase
TOPT  <- crop_parameters$temp_opt_rphase


# Get weather data of the grid cell
MONTH <- dtclm[lon==LON & lat==LAT]$month
MTEMP <- dtclm[lon==LON & lat==LAT]$tas
MPREC <- dtclm[lon==LON & lat==LAT]$pr
MPPET <- dtclm[lon==LON & lat==LAT]$ppet
MPPET_DIFF <- MPPET-c(MPPET[-1], MPPET[1])


DTPIX <- data.table(DOY = midday[MONTH],
                    MTEMP,
                    MPREC,
                    MPPET,
                    MPPET_DIFF)

dt <- DTPIX



#Walter-Lieth climatic diagram ####

plotWLClimate <- function(t, p, e, thresholds = c(TBASE, TOPT), sd = NA, hd = NA) {
  # x axis
  midday <- c(15,43,74,104,135,165,196,227,257,288,318,349)
  NDAYMONTH <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  month_name <- c("", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "")
  endday <- cumsum(NDAYMONTH)
  xaxis <- c(midday[1]-NDAYMONTH[12], midday, midday[12]+NDAYMONTH[1])
  xticks <- c(endday[1]-NDAYMONTH[12], endday, endday[12]+NDAYMONTH[1])
  
  # y axes
  padj <- ifelse(p > 100, (p-100)*0.1+100, p)
  tt <- c(t[12], t[1:12], t[1])
  pp <- c(padj[12], padj[1:12], padj[1])
  ee <- c(e[12], e[1:12], e[1])*100
  eadj <- ifelse(ee > 100, (ee-100)*0.1+100, ee)
  

  # plot
  plot(xaxis, tt, type = "o", lwd = 2, axes = FALSE, col = "red", xlim = c(0,365), ylim = c(-10,70), xlab="", ylab="", xaxs="i", yaxs="i")
  axis(1, pos = 0, at = xticks, labels = paste(month_name, xticks, sep = " - "), las = 2)
  mtext(side = 1, text = "Month - DOY", line = 2.5, cex = 1.5) #x axis label
  abline(h = c(0, 50))
  
  # grid
  abline(h = seq(10, 40, by = 10), col = "grey", lty = 2)
  
  # temperature thresholds
  abline(h = thresholds, col = "black", lwd = 1, lty = 1)
  
  # sdate and hdate
  #segments(x0=sd,y0=0,x1=sd,y1=50,col="#1b7021")
  #segments(x0=hd,y0=0,x1=hd,y1=50,col="#701b6a")
  
  # growing period
  if (sd > hd) {
    rect(0,  0, hd,  50, col = "grey", density = 50)
    rect(sd, 0, 365, 50, col = "grey", density = 50)
  } else {
    rect(sd, 0, hd,  50, col = "grey", density = 50) 
  }
  
  axis(2, pos = 0, at = seq(-10, 50, 10))
  mtext(side = 2, text = "Temperature (°C)", line = 3, cex = 1.5) #y axis label
  par(new = TRUE)
  #plot(0:13 - 0.5, pp, xlim = c(0.5,12), type = "o", lwd = 2, axes = FALSE, col = "blue", ylim = c(-20,140),  ylab="")
  plot(xaxis, pp, xlim = c(0,365), type = "o", lwd = 2, axes = FALSE, col = "blue", ylim = c(-20,140), xlab="", ylab="", xaxs="i", yaxs="i")
  #points(0:13 - 0.5, ee, type = "o", lwd = 2, col = "orange", ylim = c(-20,140),  ylab="")
  points(xaxis, ee, type = "o", lwd = 2, col = "orange", ylim = c(-20,140),  ylab="")
  axis(4, pos = 365, at = c(seq(-20, 100, 20), 120, 140), labels = c(seq(-20, 100, 20), 200, 300))
  mtext(side = 4, text = "Precipitation, ET0 (mm)", line = 3, cex = 1.5) #y axis label
}

pdf(paste0(figure.dir, "PIXEL/", "pixel_crop_calendar_", CROP,  "_", COORD, "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".pdf"))

plotWLClimate (t = MTEMP, p = MPREC, e = NA, thresholds = c(TBASE, TOPT), sd = SD, hd = HD)

dev.off()
