#-----------------------------------------------------------#
# Climate-driven sowing and harvest dates                   #
# R-Code                                                    #
# Written by Sara Minoli                                    #
# Based on Minoli et al. (2019) Global and Planetary Change #
#-----------------------------------------------------------#

#-----------------------------------------------------------#
# MAIN SCRIPT #
#-----------------------------------------------------------#

rm(list=ls(all=TRUE))

starttime <- Sys.time() # Track run-time
print(starttime)

oncluster <- TRUE
parallel  <- TRUE

if(oncluster==TRUE) {
working.dir <- "/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate/"
} else {
  #working.dir <- "C:/Users/minoli/Documents/Work/crop_calendars_gitlab/crop_phen_paper/compute_sdate_hdate/"
}

# PARALLEL MODE SET UP ----
#___________________________________________________________#

if(parallel==T) {
library(foreach)
library(doParallel)
# The size of our cluster must match the number of CPUs allocated to us
# by SLURM. 
# By default, R can see all CPUs, including those not allocated to us.
ncpus <- as.integer(Sys.getenv("SLURM_JOB_CPUS_PER_NODE"))
#ncpus <- 4
cl <- makeCluster(ncpus)
registerDoParallel(cl)
getDoParName()
getDoParWorkers()
}


# READ CONFIGURATION ----
#___________________________________________________________#
source(paste0(working.dir, "configuration/configuration_ggcmi_ph3.R"))

# IMPORT FUNCTIONS ----
#___________________________________________________________#
source(paste0(working.dir, "src/compute_sdate_hdate_functions.R"))
source(paste0(working.dir, "src/ggplot.map.general.R"))

# SELECT SCENARIO ----
#___________________________________________________________#

if(oncluster==TRUE) {
# import argument from bash script
options(echo=FALSE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
} else {
  args <- c(2,1)
}
print(args)

# select variable, crop, model
GCM  <- args[1]
SC   <- args[2]
SY   <- as.numeric(args[3])
EY   <- as.numeric(args[4])
CROP <- args[5]

# If no overlap historical / future, read only one file
# If overlap historical / future need to read two files
SC <- ifelse(EY <= 2014, "historical", SC)

scen.dir <- paste0(output.dir, "/", SC, "/")
if (!dir.exists(scen.dir)) dir.create(scen.dir, recursive = T)

# IMPORT CLIMATE DATA ----
#___________________________________________________________#
# get monthly climate data
cat("\n", paste("Importing climate data of:", GCM, SC, SY, EY),
    "-------------------------------------------------------", sep = "\n")
DTclm <- get(load(file = paste0(climate.dir, "DT_average_monthly_climate_", GCM, "_", SC, "_", SY, "_", EY, ".Rdata")))

print(DTclm)

# MAIN ----
#___________________________________________________________#
mainfunction <- function(coord, dtclm, dfgrid, crop_parameters) {
  # Get coordinates of the grid cell
  LON <- dfgrid$lon[coord]
  LAT <- dfgrid$lat[coord]

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
  
  PIXEL_DF <- data.frame("pixelnr" = coord,
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
  row.names(PIXEL_DF) <- NULL  
  return(PIXEL_DF)
  
}

# LOOP CROPS ----
#___________________________________________________________#
# CROP <- crops[arg2]

cat("\n", paste("Start computing crop calendar for", CROP, "..."),
           "-------------------------------------------------------", sep = "\n")

  crop_pars <- subset(crop_parameters_all, subset = crop_name %in% CROP)
 
print(t(crop_pars))
 
  OUTPUT_DF <- NULL
  
  # For testing on a subset of cells
  # CELL_SUBSET <- as.numeric(rownames(subset(coord.df, lon == -100.25 & lat > 30 & lat < 35)))
  # CELL_SUBSET <- as.numeric(rownames(subset(coord.df, lon == -100.25 & lat > 30 & lat < 60)))
  # CELL_SUBSET <- as.numeric(rownames(subset(coord.df, lon> (-110.25) & lon<(-90.25) & lat>30 & lat<60)))
  # COORD <- 47992

# Initialize log file to monitor parallel foreach loop
logfile <- paste0(working.dir, "out_err/",CROP,"_",GCM,"_",SC,"_",SY,"_",EY, "_log.txt")
# logfile <- paste0(working.dir, "out_err/", arg1, "_", arg2, "_log.txt")
cat("Monitoring foreach iterations:", "\n", file=logfile, append=F)

if (parallel==TRUE) {   # Parallel Mode

   #OUTPUT_DF <- foreach(COORD = CELL_SUBSET, .combine="rbind", .packages="data.table", .inorder=F, .verbose=F) %dopar% {
   OUTPUT_DF <- foreach(COORD = 1:NCELLS, .combine="rbind", .packages="data.table", .inorder=F, .verbose=F) %dopar% {
    
    if (COORD%%1000==0) cat(COORD, "\n", file=logfile, append=T)
    PIXEL_DF <- mainfunction(coord = COORD, dtclm = DTclm, dfgrid = coord.df, crop_parameters=crop_pars)

  } # foreach

} else {   # Sequential Mode

  #for (COORD in CELL_SUBSET) { # NCELLS
  for (COORD in c(1:NCELLS)) { # NCELLS
    
    if (COORD%%1000==0) cat(COORD, "\n", file=logfile, append=T) 
    PIXEL_DF <- mainfunction(coord = COORD, dtclm = DTclm, dfgrid = coord.df, crop_parameters=crop_pars)
    OUTPUT_DF <- rbind(OUTPUT_DF, PIXEL_DF)
  
  } #for

} #if parallel

# Convert dataframe to datatable
DT <- data.table(OUTPUT_DF[order(OUTPUT_DF$pixelnr),])
rm(OUTPUT_DF)

# Re-order and label factors
DT$irrigation       <- factor(DT$irrigation, c("Rainfed", "Irrigated"), c("Rainfed", "Irrigated"))
DT$seasonality_type <- factor(DT$seasonality_type, seasonality_types, seasonality_types)
DT$harvest_rule     <- factor(DT$harvest_rule, c(1:9), harvest_rule_labels)
DT$harvest_reason   <- factor(DT$harvest_reason, c(1:6), harvest_reason_labels)

print(str(DT))

# Save data table ----
save(DT, file = paste0(scen.dir, "DT_output_crop_calendars_",
                       CROP, "_", GCM, "_", SC, "_", SY, "_", EY, ".Rdata"))


# PLOT MAPS ----
#___________________________________________________________#
cat("\n", paste("Plotting outputs ..."),
    "-------------------------------------------------------", sep = "\n")

pdf(paste0(scen.dir, "map_crop_calendars_", CROP, "_", GCM, "_", SC, "_", SY, "_", EY, ".pdf"), width = a4w, height = a4h*0.4)

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

endtime <- Sys.time()
print(endtime)

print(endtime-starttime)



# End of script, always close worker nodes if run in parallel mode ----
#______________________________________________________________________#
if(parallel==T) {
stopCluster(cl)
} 
