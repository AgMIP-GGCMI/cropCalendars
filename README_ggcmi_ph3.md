# README (28.04.2021)
--------------------
Author: Sara Minoli
Affiliation: Potsdam Institute for Climate Impact Research, Germany
Contact: sara.minoli@pik-potsdam.de

This folder includes R-coded scripts for computing sowing and harvest dates following the approaches from Waha et al. (2012) and Minoli et al. (2019).

## Running the code:

### Crop calendars (sdate & hdate)

1. Adjust crop parameters in `./compute_sdate_hdate/parameters/crop_parameters.csv` (default are taken from Waha 2012 & Minoli 2019)

2. Create a configuration file: `./compute_sdate_hdate/configuration/configuration_ggcmi_ph3.R`
- Define I/O paths (make sure automatically created directories are correct)
- Define crops to simulate
- Grid coordinates (by default LPJmL grid.bin is used, which is converted into a dataframe)

3. Prepare monthly climate files: `./preprocessing/ prepare_monthly_climate_isimip3b.R` and `./preprocessing/ prepare_monthly_climate_isimip3b.sh`
- Compute long-term monthly average of temperature (degC), precipitation (mm), potential evapotranspiration (mm), P/PET ratio (frac), stored in a datatable with header: lon, lat, month, tas, pr, pet, ppet
- pass arguments and submit job via bash script

4.  Set up main script: `./compute_sdate_hdate/main_ggcmi_ph3.R`
- Choose if On Cluster TRUE/FALSE
- Define Working directory

5. `./compute_sdate_hdate/main_ggcmi_ph3.R`
- Define loops for running scenarios and crops in batch
- To run it type: `bash main_ggcmi_ph3.sh` (note: module purge might be needed beforehand, if need to open old Rdata files)

6. `./postprocessing/create_ncdf_sdate_hdate_multiyear_ggcmi_ph3.sh`

### Phenological Heat Units (PHU)

1. `./compute_phu/main_ggcmi_ph3.sh`

2. `./compute_phu/postprocessing/create_lpjml_sdate_hdate_phu_input.sh`

### Accessories

1. Debugging `./compute_sdate_hdate/src/debugging.R` can help troubleshooting
