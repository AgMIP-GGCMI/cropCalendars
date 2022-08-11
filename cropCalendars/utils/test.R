rm(list=ls(all=TRUE))

library(devtools)
library(ncdf4)
library(abind)
#library(cropCalendars)
load_all()

gcms <- c(
  "GFDL-ESM4",
  "IPSL-CM6A-LR",
  "MPI-ESM1-2-HR",
  "MRI-ESM2-0",
  "UKESM1-0-LL"
    )
enms <- c(
    "GFDL-ESM4"     = "r1i1p1f1",
    "IPSL-CM6A-LR"  = "r1i1p1f1",
    "MPI-ESM1-2-HR" = "r1i1p1f1",
    "MRI-ESM2-0"    = "r1i1p1f1",
    "UKESM1-0-LL"   = "r1i1p1f2"
)
scens <- c(
  "picontrol",
  "historical",
  "ssp126",
  "ssp585",
  "ssp370"
    )
syears <- list(
  "picontrol"  = seq(1601, 2091, by = 10),
  "historical" = seq(1851, 2011, by = 10),
  "ssp126"     = c(2015, seq(2021, 2091, by = 10)),
  "ssp585"     = c(2015, seq(2021, 2091, by = 10)),
  "ssp370"     = c(2015, seq(2021, 2091, by = 10))
)
eyears <- list(
  "picontrol"  = seq(1610, 2100, by = 10),
  "historical" = c(seq(1860, 2014, by = 10), 2014),
  "ssp126"     = seq(2020, 2100, by = 10),
  "ssp585"     = seq(2020, 2100, by = 10),
  "ssp370"     = seq(2020, 2100, by = 10)
)

ccal_years <- seq(1601, 2091, by 10)

vars <- c("tas", "pr")

# Loop through all GCMs and scenarios and list all files needed to
#  calculate crop calendars for one sim configuration.
clm_file_list <- list()
for (vv in seq(length(vars))) {
  cat("\n", vars[vv], "\n-----")
  for (gg in seq_len(length(gcms))) {
    for (sc in seq_len(length(scens))) {

      if (length(grep("ssp", scens[sc])) > 0) {
        # If SSP, concatenate also the last two files from historical scenario
        fnames1 <- paste_ggcmi3_clm_fname(
          path1 = paste0("/p/projects/macmit/data/GGCMI/",
                          "AgMIP.input/phase3/climate_land_only/"),
          path2         = "climate3b/",
          clm_scenario  = "historical",
          clm_forcing   = gcms[gg],
          ens_member    = enms[gcms[gg]],
          bias_adj      = "w5e5",
          clm_var       = vars[vv],
          extent        = "global",
          time_step     = "daily",
          start_year    = syears[["historical"]][16:17],
          end_year      = eyears[["historical"]][16:17],
          file_ext      = ".nc"
        )
      } else {
        fnames1 <- NULL
      }

      fnames2 <- paste_ggcmi3_clm_fname(
        path1 = paste0("/p/projects/macmit/data/GGCMI/",
                        "AgMIP.input/phase3/climate_land_only/"),
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

readGridLPJmL <- function(
  fname  = "/p/projects/lpjml/input/historical/input_VERSION2/grid.bin",
  ncells = NULL,
  header = NULL,
  nbands = NULL,
  dtype  = NULL,
  dsize  = NULL,
  scalar = NULL
  ) {

  print(paste("Reading grid.bin input:", fname))

  # check file size
  check.fs <- (file.size(fname)-header)/ncells/dsize==nbands
  print(paste("File size as expected = ", check.fs))

  # read out data
  fcon <- file(fname, open = "rb")
  seek(con = fcon, where = header, origin = "start")
  x <- array(NA, c(ncells, nbands))
  for (i in 1:ncells){
    x[i,] <- readBin(fcon, what = dtype, size = dsize , n = nbands)*scalar
  }
  close.connection(fcon)
  print(str(x))
  return(x)
}

# Read grid file
grid_df <- as.data.frame(
  readGridLPJmL(
    ncells = 67420, header = 43, nbands = 2, dtype = integer(),
    dsize = 2, scalar = 0.01)
  )
names(grid_df) <- c("lon", "lat")

# Slice the lon-lat-time array by lon to enable parallelization
lon_matrix <- matrix(
  seq(min(grid_df$lon), max(grid_df$lon), by = 0.5), nrow = 30
  )



for (gg in seq_len(length(gcms))) {
  for (sc in seq_len(length(scens))) {
    # gg <- 1; sc <- 5; lo <- 1

    # Generate dates sequence for scenario[sc] for dimnames
    if (length(grep("ssp", scens[sc])) > 0) {
        dates <- seqDates(
          start_date = paste0(syears[["historical"]][16], "-01-01"),
          end_date   = paste0(max(eyears[[scens[sc]]]), "-12-31"),
          step = "day"
          )
    } else {
        dates <- seqDates(
        start_date = paste0(min(syears[[scens[sc]]]), "-01-01"),
        end_date   = paste0(max(eyears[[scens[sc]]]), "-12-31"),
        step = "day"
        )
    }
    # Get all climate files for one scenario
    fnames_tas <- clm_file_list[["tas"]][[gcms[gg]]][[scens[sc]]]
    fnames_pr  <- clm_file_list[["pr"]][[gcms[gg]]][[scens[sc]]]
    if (length(fnames_tas) != length(fnames_pr)) stop("tas and pr file nr. differ")

    # Loop through lon slices
    for (lo in seq_len(ncol(lon_matrix))) {

      # Subset grid for lons of this slice
      grid_sub <- subset(grid_df, lon %in% lon_matrix[, lo])

      # Extract lon-subset data from each file and concatenate along time dim
      tmp_dir <- paste0(getwd(), "/tmp/", gcms[gg], "/", scens[sc], "/")
      if (calc_climate == TRUE) {
        tas_array <- pr_array <- NULL
        for (i in seq_len(length(fnames_tas))) {
          cat("\n", fnames_tas[i])

          tas <- readNcdf(file_name = fnames_tas[i],
                          dim_subset = list(lon = lon_matrix[, lo]))
          pr  <- readNcdf(file_name = fnames_pr[i],
                          dim_subset = list(lon = lon_matrix[, lo]))

          # Convert units
          tas <- k2deg(tas)
          pr  <- pr * 60 * 60 * 24

          # Bind array along time dimension
          tas_array <- abind(tas_array, tas, use.dnns = TRUE)
          pr_array  <- abind(pr_array, pr, use.dnns = TRUE)
        } # i

        dir.create(tmp_dir, recursive = TRUE)
        save(tas_array, file = paste0(tmp_dir, "tas_array_", lo, ".RData"))
        save(pr_array,  file = paste0(tmp_dir, "pr_array_",  lo, ".RData"))
      } else {
        tas_array <- get(load(file = paste0(tmp_dir, "tas_array_", lo, ".RData")))
        pr_array  <- get(load(file = paste0(tmp_dir, "pr_array_",  lo, ".RData")))
      }

      # Loop through pixels to extract temperature and precipitation
      for (j in seq_len(nrow(grid_sub))) {
        print(j)
        lon_pix <- grid_sub$lon[j]
        lat_pix <- grid_sub$lat[j]

        tas_pix <- tas_array[as.character(lon_pix), as.character(lat_pix), ]
        pr_pix  <- pr_array[as.character(lon_pix), as.character(lat_pix), ]

        # Assign dates as dimnames of vectors
        names(tas_pix) <- names(pr_pix) <- dates

        # Loop through 10-years steps and extract climate
        for (yy in seq_len(length(eyears[[scens[sc]]]))) {
          time_idx <- which(
            eyears[[scens[sc]]][yy] - date_to_year(dates) >= 0 &
            eyears[[scens[sc]]][yy] - date_to_year(dates) < 20
            )
          time_years <- date_to_year(names(tas_pix)[time_idx])

          # Calculate monthly climate
          mclm <- calcMonthlyClimate(
            lat        = lat_pix,
            temp       = tas_pix[time_idx],
            prec       = pr_pix[time_idx],
            syear      = min(time_years),
            eyear      = max(time_years),
            incl_feb29 = TRUE
            )
          # Calculate crop calendar
          ccal <- calcCropCalendars(
            lon          = lon_pix,
            lat          = lat_pix,
            mclimate     = mclm,
            crop         = "Maize"
            )
        } # yy
      } # j
    } # lo
  } # sc
} # gg

# Remove tmp dir
#unlink(tmp_dir, recursive = TRUE)