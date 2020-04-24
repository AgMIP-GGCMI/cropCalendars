#-----------------------------------------------------------#
# Climate-driven sowing and harvest dates                   #
# R-Code                                                    #
# Written by Sara Minoli                                    #
# Based on Minoli et al. (2019) Global and Planetary Change #
#-----------------------------------------------------------#

#-----------------------------------------------------------#
# MAIN SCRIPT #
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

if(oncluster==TRUE) {
# import argument from bash script
options(echo=FALSE) # if you want see commands in output file
args <- as.numeric(commandArgs(trailingOnly = TRUE))
} else {
  args <- 15  
}
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

# MAIN ----
#___________________________________________________________#
mainfunction <- function(coord, dtclm) {
  # Get coordinates of the grid cell
  LON <- coord.df$lon[coord]
  LAT <- coord.df$lat[coord]
  
  # Get weather data of the grid cell
  MTEMP <- dtclm[lon==LON & lat==LAT]$tas
  MPREC <- dtclm[lon==LON & lat==LAT]$pr
  MPPET <- dtclm[lon==LON & lat==LAT]$ppet
  MPPET_DIFF <- MPPET-c(MPPET[-1], MPPET[1])
  
  #print(DTclm[lon==LON & lat==LAT])
  
  SEASONALITY <- calcSeasonality(MTEMP, MPREC, TEMPMIN)
  
  SOWING <- calcSowingDate(crop_parameters, MTEMP, MPPET, SEASONALITY, LAT)
  SOWING_MONTH  <- SOWING[["sowing_month"]]
  SOWING_DAY    <- SOWING[["sowing_doy"]]
  SOWING_SEASON <- SOWING[["sowing_season"]]
  
  HARVEST_RULE  <- calcHarvestRule(crop_parameters, MTEMP, MPPET, SEASONALITY)
  
  HARVEST_VECTOR  <- calcHarvestDateVector(crop_parameters, SOWING_DAY, SOWING_SEASON, MTEMP, MPPET, MPPET_DIFF)
  
  HARVEST <- calcHarvestDate(crop_parameters, MTEMP, SOWING_DAY, SOWING_MONTH, SOWING_SEASON, SEASONALITY, HARVEST_RULE, HARVEST_VECTOR)
  HARVEST_DAY_RF  <- HARVEST[["hd_rf"]]
  HARVEST_DAY_IR  <- HARVEST[["hd_ir"]]
  HARVEST_REAS_RF <- HARVEST[["harvest_reason_rf"]]
  HARVEST_REAS_IR <- HARVEST[["harvest_reason_ir"]]
  
  GROWPERIOD_RF <- computeGrowingPeriod(SOWING_DAY, HARVEST_DAY_RF, 365)
  GROWPERIOD_IR <- computeGrowingPeriod(SOWING_DAY, HARVEST_DAY_IR, 365)
  
  PIXEL_DF <- data.frame("pixelnr" = COORD,
                         "lon" =    rep(LON, 2),
                         "lat" =    rep(LAT, 2),
                         "cft_id" = rep(crop_parameters$cft_id, 2),
                         "crop" =   rep(crop_parameters$crop_name, 2),
                         "irrigation" = c("Rainfed", "Irrigated"),
                         "seasonality_type" = rep(SEASONALITY, 2),
                         "sowing_season" =    rep(SOWING_SEASON, 2),
                         "sowing_month" =     rep(SOWING_MONTH, 2),
                         "sowing_doy" =       rep(SOWING_DAY, 2),
                         "harvest_rule" =     rep(HARVEST_RULE, 2),
                         "harvest_reason" = c(HARVEST_REAS_RF, HARVEST_REAS_IR),
                         "maturity_doy" =   c(HARVEST_DAY_RF, HARVEST_DAY_IR),
                         "growing_period" = c(GROWPERIOD_RF, GROWPERIOD_IR)  )
  
  return(PIXEL_DF)
  
}

# LOOP CROPS ----
#___________________________________________________________#
CROP <- crops[CROPIDX]

cat("\n", paste("Start computing crop calendar for", CROP, "..."),
           "-------------------------------------------------------", sep = "\n")

  crop_parameters <- subset(crop_parameters_all, subset = crop_name %in% CROP)
  
  OUTPUT_DF <- NULL
  
   #CELL_SUBSET <- as.numeric(rownames(subset(coord.df, lon > 140 & lon < 155 & lat > -45 & lat < -15)))
   #for (COORD in CELL_SUBSET) { # NCELLS
  for (COORD in c(1:NCELLS)) { # NCELLS
    PIXEL_DF <- mainfunction(coord = COORD, dtclm = DTclm)
    OUTPUT_DF <- rbind(OUTPUT_DF, PIXEL_DF)
    cat(COORD, "\t")
  }

# Convert dataframe to datatable
DT <- data.table(OUTPUT_DF)
rm(OUTPUT_DF)

# Re-order and label factors
DT$irrigation       <- factor(DT$irrigation, c("Rainfed", "Irrigated"), c("Rainfed", "Irrigated"))
DT$seasonality_type <- factor(DT$seasonality_type, seasonality_types, seasonality_types)
DT$harvest_rule     <- factor(DT$harvest_rule, c(1:9), harvest_rule_labels)
DT$harvest_reason   <- factor(DT$harvest_reason, c(1:6), harvest_reason_labels)

print(str(DT))


save(DT, file = paste0(output.dir, "DT_output_crop_calendars_", CROP, "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata"))


# PLOT MAPS ----
#___________________________________________________________#
cat("\n", paste("Plotting outputs ..."),
    "-------------------------------------------------------", sep = "\n")

pdf(paste0(figure.dir, "map_crop_calendars_", CROP, "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".pdf"), width = a4w, height = a4h*0.4)

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

cat("\n", paste("Simulation ended!"),
    "-------------------------------------------------------", sep = "\n")
print(Sys.time())
