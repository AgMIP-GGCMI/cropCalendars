# DEBUGGING

oncluster <- TRUE

if(oncluster==TRUE) {
  working.dir <- "/p/projects/macmit/users/minoli/PROJECTS/CROP_PHENOLOGY_v01/SCRIPTS/GROWING_PERIODS_PACKAGE/"
} else {
  working.dir <- "D:/PROJECTS/GROWING_PERIODS_PACKAGE/"
}

climate.dir <- paste0(working.dir, "DATA/CLIMATE/")

# FUNCTIONS ----
source(paste0(working.dir, "CODE/src/units.R"))
source(paste0(working.dir, "CODE/src/ggplot.map.general.R"))
source(paste0(working.dir, "CODE/configuration/configuration.R"))
source(paste0(working.dir, "CODE/configuration/graphics.R"))

# import arguments from the job script ----
args <- seq(1, 72, by = 12)[1]

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

CROP <- "Maize"

DT <- get(load(file = paste0(output.dir, "DT_output_crop_calendars_", CROP, "_", GCM, "_", SCgp, "_", SYgp, "_", EYgp, ".Rdata")))

DT[lon>(-75) & lon<(-50) & lat>(-30) & lat<(-20) & harvest_reason=="GPmin" & seasonality_type=="PRECTEMP"]

coord <- COORD <- 17395
# coord <- COORD <- as.numeric(row.names(subset(coord.df, lon==-74.75 & lat==-44.75)))
# COORD <- 27425
# COORD <- 25003
# COORD <- 34981

# calcDoyCrossThreshold
monthly_value = MTEMP
threshold = 14

#calcDoyWetMonth
monthly_value <- MPPET


# calcSowingDate
croppar = crop_parameters
monthly_temp = MTEMP
monthly_prec = MPREC
monthly_ppet = MPPET
seasonality = SEASONALITY
lat = LAT

# calcHarvestDateVector
croppar = crop_parameters
sowing_date = SOWING_DAY
sowing_season = SOWING_SEASON
monthly_temp = MTEMP
monthly_ppet = MPPET
monthly_ppet_diff = MPPET_DIFF

# calcHarvestDate
croppar = crop_parameters
monthly_temp = MTEMP
sowing_date = SOWING_DAY
sowing_month = SOWING_MONTH
sowing_season = SOWING_SEASON
seasonality = SEASONALITY
harvest_rule = HARVEST_RULE
hd_vector = HARVEST_VECTOR
