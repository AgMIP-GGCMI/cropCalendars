README (24.04.2020)
------
Author: Sara Minoli
Affiliation: Potsdam Institute for Climate Impact Research, Germany
Contact: sara.minoli@pik-potsdam.de

This folder includes R-coded scripts for computing sowing and harvest dates following the approaches from Waha et al. (2012) and Minoli et al. (2019).

Running the code:

1) ./compute_sdate_hdate/configuration/configuration.R 
- Define I/O paths
- Define crops to simulate
- Grid coordinates (by default LPJmL grid.bin is used, which is converted into a dataframe)

2) ./compute_sdate_hdate/configuration/climate_scenarios_to_simulate.txt
- Write a table with information on climate scenarios and time slices to simulate

3) ./preprocessing/prepare_montly_climate.R
- Compute long-term monthly average of temperature (degC), precipitation (mm), potential evapotranspiration (mm), P/PET ratio (frac), stored in a datatable with header: lon, lat, month, tas, pr, pet, ppet

3) ./compute_sdate_hdate/main.R
- Choose if On Cluster TRUE/FALSE
- Define Working directory

4) ./compute_sdate_hdate/main.sh
- Define array for running scenarios and crops in batch
- e.g. for 12 climate scenarios & 6 crops use #SBATCH --array=1-72
- To run it type: sbatch main.sh

Output:
- Create a directory: e.g. ./DATA/OUTPUT
- Computed crop calendars are stored in Datatables at ./DATA/OUTPUT
- Maps are plotted and stored in ./DATA/FIGURES

Postprocessing:
- Winter-wheat suitable area can be derived by removing pixels with sowing_month==0 which indicates that the deafault sowing month was chosen
- A script for merging spring and winter wheat is provided: ./postprocessing/merge_winter_spring_wheat.R

Parameters:
- Adjust crop parameters in crop_parameters.txt (default are taken from Waha 2012 & Minoli 2019)

Debugging:
- ./compute_sdate_hdate/src/debugging.R can help troubleshooting
