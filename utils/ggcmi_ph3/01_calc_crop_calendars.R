# ---------------------------------------------------------------------------- #
# Calculate crop calendars for GGCMI phase3 (ISIMIP3b climate)
# run via job script (.sh)

# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
# ---------------------------------------------------------------------------- #

rm(list = ls(all = TRUE))

starttime <- Sys.time() # Track run-time
print(starttime)

# ------------------------------------ #
# General settings
work_dir <- setwd(paste(
  "/home/minoli/crop_calendars_gitlab/r_package/cropCalendars/utils/ggcmi_ph3/"
))
source(paste0(work_dir, "/00_config.R"))


# ------------------------------------ #
# Individual-run settings
if (cluster_job == TRUE) {
  # import argument from bash script
  options(echo = FALSE) # if you want see commands in output file
  args <- commandArgs(trailingOnly = TRUE)
} else {
  args <- c('GFDL-ESM4', 'historical', 'Maize', '2011')
}
print(args)

# ------------------------------------ #
# Select variable, crop, model, year
gcm    <- args[1]
scen   <- args[2]
cro    <- args[3]
year   <- as.numeric(args[4])
nnodes <- as.numeric(args[5])
ntasks <- as.numeric(args[6])

ncpus        <- ntasks * nnodes
n_lon_chunks <- 45
# --nodes=1 --ntasks-per-node=16 --exclusive

# Output directory
dfout_dir <- paste0(output_dir, "/crop_calendars/DT/", scen, "/", gcm, "/")
if (!dir.exists(dfout_dir)) dir.create(dfout_dir, recursive = TRUE)
plot_dir  <- paste0(output_dir, "/crop_calendars/DT/", scen, "/", gcm, "/plots/")
if (!dir.exists(plot_dir) & plot_results) dir.create(plot_dir, recursive = TRUE)

# ------------------------------------ #
# Register cluster
if(parallel == TRUE) {
  library(foreach)
  library(doParallel)
  # The size of our cluster must match the number of CPUs allocated to us
  # by SLURM.
  # By default, R can see all CPUs, including those not allocated to us.
  #ncpus <- as.integer(Sys.getenv("SLURM_JOB_CPUS_PER_NODE"))
  if (!exists("ncpus")) ncpus <- 16
  cl <- makeCluster(ncpus)
  registerDoParallel(cl)
  getDoParName()
  getDoParWorkers()
}

# ------------------------------------ #
# Loop through all GCMs and scenarios and list all files needed to
#  calculate crop calendars for one sim configuration.
vars <- c("tas", "pr")
clm_file_list <- list()
for (vv in seq(length(vars))) {
  cat("\n", vars[vv], "\n-----")
  for (gg in seq_len(length(gcms))) {
    for (sc in seq_len(length(scens))) {

      if (length(grep("ssp", scens[sc])) > 0) {
        # If SSP, concatenate also the last two files from historical scenario
        fnames1 <- paste_ggcmi3_clm_fname(
          path1         = climate_dir,
          path2         = "climate3b/",
          clm_scenario  = "historical",
          clm_forcing   = gcms[gg],
          ens_member    = enms[gcms[gg]],
          bias_adj      = "w5e5",
          clm_var       = vars[vv],
          extent        = "global",
          time_step     = "daily",
          start_year    = syears[["historical"]],
          end_year      = eyears[["historical"]],
          file_ext      = ".nc"
        )
      } else {
        fnames1 <- NULL
      }

      fnames2 <- paste_ggcmi3_clm_fname(
        path1         = climate_dir,
        path2         = "climate3b/",
        clm_scenario  = scens[sc],
        clm_forcing   = gcms[gg],
        ens_member    = enms[gcms[gg]],
        bias_adj      = "w5e5",
        clm_var       = vars[vv],
        extent        = "global",
        time_step     = "daily",
        start_year    = syears[[scens[sc]]],
        end_year      = eyears[[scens[sc]]],
        file_ext      = ".nc"
      )

      # Check if all files exist
      cat(paste0("\n", gcms[gg], " ", scens[sc], "\t",
          all(file.exists(c(fnames1, fnames2)))))

      clm_file_list[[vars[vv]]][[gcms[gg]]][[scens[sc]]] <- c(fnames1, fnames2)
    }
  }
}

# Get all climate files needed for this scenario
fnames_tas <- clm_file_list[["tas"]][[gcms[gg]]][[scens[sc]]]
fnames_pr  <- clm_file_list[["pr"]][[gcms[gg]]][[scens[sc]]]

# ------------------------------------ #
# First and last year of crop calendar calculation in this scenario
ccal_first_year <- ccal_years[
  which.min(abs(min(syears[[scens[sc]]]) - ccal_years))
  ]
ccal_last_year  <- ccal_years[
  which.min(min(eyears[[scens[sc]]]) - ccal_years)
  ]
# Index of the years
ccal_y_idx <- (
  which(ccal_years == ccal_first_year):which(ccal_years == ccal_last_year)
  )
cat("\n", gcm, scen, "--- \nCrop calendars: ", ccal_years[ccal_y_idx])

# ------------------------------------ #
# Get first and last year of climate to calculate crop calendar this year
cat("\nNow calculating crop calendars for year: ", year, "\n")
syear <- year - clm_avg_years
eyear <- year - 1
idx_first_file <- grep(syear, fnames_tas)
idx_last_file  <- grep(eyear, fnames_tas)
cat("\n", fnames_tas[idx_first_file:idx_last_file])

if (length(idx_first_file) < 1) {
  stop("Climate file does not exist!")
}

# ------------------------------------ #
#
# In terminal: scontrol show config
# Look for: DefMemPerCPU = 3500 MB
# https://kb.northwestern.edu/page.php?id=81074
# After loading two lon column: print(pryr::mem_used()) 220 MB
# 3500/110

# ------------------------------------ #
# Split lon dimension in chunks to be run in parallel
lons <- seq(min(grid_df$lon), max(grid_df$lon), by = 0.5)
lon_matrix <- matrix(lons, nrow = length(lons)/n_lon_chunks)

#print(rlimit_all())

# ------------------------------------ #
# Loop through lon chunks
output_df <- foreach(lo        = seq_len(ncol(lon_matrix)),
                     .combine  = "rbind",
                     .inorder  = FALSE,
                     .packages = c("ncdf4", "abind", "cropCalendars"),
                     .verbose  = FALSE
                     ) %dopar% {

  # Log file to track foreach parallel loop
  log_file <- paste0(dfout_dir, "log_", lo, "_", syear, "_", eyear, ".txt")
  unlink(log_file)
  sink(log_file, append = TRUE)
  cat("\nDoing ", lo, " of ", ncol(lon_matrix), "\n")

  # Subset grid for lons of this slice
  grid_sub <- subset(grid_df, lon %in% lon_matrix[, lo])

  tas_array <- pr_array <- NULL

  for (i in idx_first_file:idx_last_file) {
    # Track memory used
    print(pryr::mem_used())

    # Read climate data
    tas <- cropCalendars::readNcdf(
      file_name = fnames_tas[i],
      dim_subset = list(lon = lon_matrix[, lo])
      )
    pr  <- cropCalendars::readNcdf(
      file_name = fnames_pr[i],
      dim_subset = list(lon = lon_matrix[, lo])
      )

    # Convert units
    tas <- k2deg(tas)
    pr  <- 60 * 60 * 24 * (pr)

    # Bind array along time dimension
    tas_array <- abind(tas_array, tas, use.dnns = TRUE)
    pr_array  <- abind(pr_array, pr, use.dnns = TRUE)

    print(gc(verbose = TRUE))

    rm(tas, pr)
  } # i

  # Track memory used
  print(pryr::mem_used())

  # Loop through pixels to extract temperature and precipitation
  ccal_df <- NULL
  for (j in seq_len(nrow(grid_sub))) {
    lon_pix <- grid_sub$lon[j]
    lat_pix <- grid_sub$lat[j]

    tas_pix <- tas_array[as.character(lon_pix), as.character(lat_pix), ]
    pr_pix  <- pr_array[as.character(lon_pix), as.character(lat_pix), ]
    if (any(is.na(tas_pix)) | any(is.na(pr_pix))) {
      cat("\nMissing values, skipping ", lon_pix, lat_pix)
      next
    }

    # Assign dates as dimnames of vectors
    dates <- seqDates(
      start_date = paste0(syear, "-01-01"),
      end_date   = paste0(eyear, "-12-31"),
      step = "day"
    )
    names(tas_pix) <- names(pr_pix) <- dates

    # Calculate monthly climate
    mclm <- calcMonthlyClimate(
      lat        = lat_pix,
      temp       = tas_pix,
      prec       = pr_pix,
      syear      = syear,
      eyear      = eyear,
      incl_feb29 = TRUE
      )

    # Calculate crop calendar
    ccal <- calcCropCalendars(
      lon             = lon_pix,
      lat             = lat_pix,
      mclimate        = mclm,
      crop            = cro
      )
    ccal_df <- rbind(ccal_df, ccal)

    print(paste(j, "of", nrow(grid_sub)))

  } # j
  unlink(log_file)
  return(ccal_df)
} # lo

cat("\nFinished foreach loop\n")
# Always close worker nodes, if running in parallel mode
if(parallel==T) {
  stopCluster(cl)
}

# ------------------------------------ #
# Save data table
DT <- data.table(output_df)
fnout <- paste0(dfout_dir, "DT_output_crop_calendars_",
                cro, "_", gcm, "_", scen, "_", syear, "_", eyear, ".Rdata")
cat("\n", fnout)
save(DT, file = fnout)


# Plot maps
if (plot_results) {
  fnout <- paste0(dfout_dir, "DT_output_crop_calendars_",
                  cro, "_", gcm, "_", scen, "_", syear, "_", eyear, ".Rdata")
  fnpdf <- paste0(plot_dir, "map_crop_calendars_",
                  cro, "_", gcm, "_", scen, "_", syear, "_", eyear, ".pdf")
  plotMapCropCalendars(fnDT = fnout, fnPDF = fnpdf)
}

# ------------------------------------ #
# Done!
endtime <- Sys.time()
print(endtime)

print(endtime-starttime)
