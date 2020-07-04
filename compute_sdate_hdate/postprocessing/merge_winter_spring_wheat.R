###########################################################

# Separate winter and spring wheat

###########################################################


rm(list=ls(all=TRUE))

oncluster <- TRUE

if(oncluster==TRUE) {
  working.dir <- "/home/minoli/crop_calendars_gitlab/crop_phen_paper/compute_sdate_hdate/"
} else {
  working.dir <- "D:/PROJECTS/GROWING_PERIODS_PACKAGE/"
}

# PACKAGES ----
library(ncdf4)
library(data.table)
library(ggplot2)

# FUNCTIONS ----
source(paste0(working.dir, "src/units.R"))
source(paste0(working.dir, "src/ggplot.map.general.R"))
source(paste0(working.dir, "configuration/configuration.R"))
source(paste0(working.dir, "configuration/graphics.R"))

#climate.dir <- paste0(project.dir, "DATA/CLIMATE/")

# import arguments from the job script ----
options(echo=FALSE) # if want see commands in output file
args <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
print(args)
# args <- seq(1, 72, by = 12)[1]


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


if (crops[CROPIDX]=="Winter_Wheat") {
  
  CROP <- "Spring_Wheat"
  DTswh <- get(load(file = paste0(output.dir, "DT_output_crop_calendars_", CROP, "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata")))
  
  CROP <- "Winter_Wheat"
  DTwwh <- get(load(file = paste0(output.dir, "DT_output_crop_calendars_", CROP, "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata")))
  
  rm(DT)
  
  cat("wwh & swh DTs", "\n")
  
  irrlev <- c("Rainfed", "Irrigated")
  DT <- NULL
  for (ir in 1:length(irrlev)) {
    pixels_wwh <- DTwwh[irrigation==irrlev[ir] & sowing_season=="winter"]$pixelnr
    
    DTwwh1 <- DTwwh[irrigation==irrlev[ir] & pixelnr%in%pixels_wwh]
    DTswh1 <- DTswh[irrigation==irrlev[ir] & !pixelnr%in%pixels_wwh]
    
    DT <- rbind(DT, DTswh1, DTwwh1)  
  }
  DT <- DT[order(crop, pixelnr)]
  
  
  DT$crop <- "Wheat"
  CROP <- "Wheat"
  save(DT, file = paste0(output.dir, "DT_output_crop_calendars_", CROP, "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata"))
  
  cat("Saved Wheat DT", "\n")
  
  pdf(paste0(figure.dir, "map_crop_calendars_", CROP, "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".pdf"),
      width = a4w, height = a4h*0.4)
  
  p <- ggplot.map.general(DT,
                          Value = "seasonality_type",
                          scale_type = "categorical",
                          color_scale = seasonality_types_cols)
  p <- p + facet_grid(.~irrigation)
  plot(p)
  
  p <- ggplot.map.general(DT,
                          Value = "sowing_season",
                          scale_type = "categorical")
  p <- p + facet_grid(.~irrigation)
  plot(p)
  
  p <- ggplot.map.general(DT,
                          Value = "sowing_month",
                          color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(.~irrigation)
  plot(p)
  
  p <- ggplot.map.general(DT,
                          Value = "sowing_doy",
                          color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(.~irrigation)
  plot(p)
  
  p <- ggplot.map.general(DT,
                          Value = "maturity_doy",
                          color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(.~irrigation)
  plot(p)
  
  p <- ggplot.map.general(DT,
                          Value = "growing_period",
                          color_scale = brewer.pal(11,"Spectral"))
  p <- p + facet_grid(.~irrigation)
  plot(p)
  
  p <- ggplot.map.general(DT,
                          Value = "harvest_rule",
                          scale_type = "categorical",
                          color_scale = harvest_rule_cols)
  p <- p + facet_grid(.~irrigation)
  plot(p)
  
  p <- ggplot.map.general(DT,
                          Value = "harvest_reason",
                          scale_type = "categorical",
                          color_scale = harvest_reason_cols)
  p <- p + facet_grid(.~irrigation)
  plot(p)
  
  dev.off()
  
  cat("Plotted", "\n")
  
}


cat("\n", paste("Merging spring and winter wheat ended!"),
    "-------------------------------------------------------", sep = "\n")
print(Sys.time())

