###########################################################

# Get cropland area from MIRCA, separating winter and spring wheat

###########################################################

rm(list=ls(all=TRUE))

oncluster <- TRUE

if(oncluster==TRUE) {
  working.dir <- "/home/minoli/crop_calendars_gitlab/crop_phen_paper/compute_sdate_hdate/"
} else {
  working.dir <- "D:/PROJECTS/GROWING_PERIODS_PACKAGE/"
}


# PACKAGES ----
library(data.table)
library(ggplot2)

# FUNCTIONS ----
source(paste0(working.dir, "src/units.R"))
source(paste0(working.dir, "src/ggplot.map.general.R"))
source(paste0(working.dir, "configuration/configuration.R"))
source(paste0(working.dir, "configuration/graphics.R"))

#PATHS ----
mirca.dir   <- "/p/projects/macmit/users/minoli/DATA/MIRCA2000/"
output.dir <- figure.dir <- paste0(project.dir, "DATA/MASKS/")
# Crop naming MIRCA
crop_mirca_ir <- c(1, 2, 3, 7, 8)
crop_mirca_rf <- crop_mirca_ir+26 # mirca has 26 crops
crop_names <- c("Wheat", "Maize", "Rice", "Sorghum", "Soybean")
names(crop_mirca_ir) <- names(crop_mirca_rf) <- crop_names


# Extract data from MIRCA ----
fn.mirca <- paste0(mirca.dir, "CELL_SPECIFIC_CROPPING_CALENDARS_30MN.TXT")
DTmirca <- data.table(read.table(fn.mirca, header = TRUE))
DTmirca <- DTmirca[, c("lon", "lat", "crop", "subcrop", "area", "start", "end")]

# Select simulated crops only
DTmirca_ir <- DTmirca[crop %in% crop_mirca_ir]
DTmirca_rf <- DTmirca[crop %in% crop_mirca_rf]

# Define irrigation type
DTmirca_ir$irrigation <- "Irrigated"
DTmirca_rf$irrigation <- "Rainfed"

DTmirca_ir$crop <- factor(DTmirca_ir$crop, levels = crop_mirca_ir, labels = crop_names)
DTmirca_rf$crop <- factor(DTmirca_rf$crop, levels = crop_mirca_rf, labels = crop_names)

DTmirca <- rbind(DTmirca_rf, DTmirca_ir)
DTmirca$irrigation <- factor(DTmirca$irrigation)

rm(DTmirca_ir, DTmirca_rf)



# Get climate AgMERRA (MTEMP) ----
#DTclm <- DT <- get(load(file = paste0(climate.dir, "DT_AgMERRA_monthly_average_1980_2010", ".Rdata")))
DTclm <- DT <- get(load(file = paste0(climate.dir, "DT_average_monthly_climate_WFDEI_historical_1986_2005", ".Rdata")))
rm(DT)

colnames(DTclm)[colnames(DTclm)=="mtemp"] <- "tas"



# MAIN ----
# Add spring to all
DTmirca$sowing_season <- "spring"

# Subset wheat
DTwheat <- DTmirca[crop=="Wheat"]

# Compute Temperature statistics (monthly minimum, month of minimum temperature)
DTtemp <- DTclm[,.(mintemp       = min(tas),
                   month_mintemp = which.min(tas)),
                by = c("lon", "lat")]
DTwheat2 <- merge(DTwheat, DTtemp, by = c("lon", "lat"), all.x = TRUE)

# Determine if is winter type growing period
determineSowingSeason <- function(START, END, MINTEMP, MONTH_MINTEMP) {
  SEQ1  <- seq(START, ifelse(START>END, END+12, END), by = 1)
  SEQ2  <- ifelse(SEQ1>12, SEQ1-12, SEQ1)
  # is Winter type if Tmin<10 & month[Tmin] within growing period
  sowing_season <- ifelse( MINTEMP<10 & MONTH_MINTEMP%in%SEQ2, "winter", "spring" )
  return(sowing_season)
}

# Assign winter or spring sowing season
DTwheat3 <- DTwheat2[,
                     .(area  = area,
                       start = start,
                       end   = end,
                       sowing_season = determineSowingSeason(start, end, mintemp, month_mintemp)),
                     by = c("lon", "lat", "crop", "subcrop", "irrigation")]
DTwheat3

# Bind wheat with other crops
DTmirca2 <- rbind(DTmirca[crop!="Wheat"],
                  DTwheat3)

save(DTmirca2, file = paste0(output.dir, "DT_mirca2000_adjusted", ".Rdata"))

# PLOT MAPS ----
#___________________________________________________________#
cat("\n", paste("Plotting outputs ..."),
    "-------------------------------------------------------", sep = "\n")
irrigation <- c("Rainfed", "Irrigated")

for (IR in irrigation) {
  DT <- DTmirca2[irrigation==IR & subcrop%in%c(1,2)]
  
  pdf(paste0(figure.dir, "map_crop_calendars_MIRCA2000_", IR, ".pdf"), width = a4w, height = a4h)
  
  p <- ggplot.map.general(DT,
                          Value = "sowing_season",
                          scale_type = "categorical")
  p <- p + facet_grid(crop~subcrop)
  plot(p)
  
  p <- ggplot.map.general(DT,
                          Value = "area",
                          color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(crop~subcrop)
  plot(p)
  
  
  p <- ggplot.map.general(DT,
                          Value = "start",
                          color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(crop~subcrop)
  plot(p)
  
  p <- ggplot.map.general(DT,
                          Value = "end",
                          color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(crop~subcrop)
  plot(p)
  
  dev.off()
}

