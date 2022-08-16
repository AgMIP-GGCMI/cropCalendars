#!/bin/bash

# Bash script for calculating crop calendars for GGCMI phase3 (ISIMIP3b climate)

# Working directory, where the .R file is stored
wd=/home/minoli/crop_calendars_gitlab/r_package/cropCalendars/utils/ggcmi_ph3

# GCM, scenario, crops
gcms=('GFDL-ESM4' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'MRI-ESM2-0' 'UKESM1-0-LL')
scens=('ssp585' 'ssp370' 'ssp126' 'historical' 'picontrol')
crops=('Maize' 'Rice' 'Sorghum' 'Soybean' 'Spring_Wheat' 'Winter_Wheat')

# For running only some scenario
gcms=('GFDL-ESM4')
scens=('historical')
crops=('Maize')
#years=('2071')

# sbatch settings
nnodes=1
ntasks=16

# MAIN
for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for cr in "${!crops[@]}";do

      # Select years for each scenario
      if [ ${scens[sc]} = 'picontrol' ]
      then
        years=($(seq 1601 10 2091))
      elif [ ${scens[sc]} = 'historical' ]
      then
        years=($(seq 1851 10 2021))
      else
        years=($(seq 2011 10 2091))
      fi

      for yy in "${!years[@]}";do

echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- CROP: ${crops[cr]} YEARS: ${years[yy]}"

# Submit job to SLURM - for arguments, see https://slurm.schedmd.com/sbatch.html
sbatch --nodes=${nnodes} --ntasks-per-node=${ntasks} --exclusive \
-t 01:00:00 -J crop_cal -A macmit --workdir=${wd} \
R -f 01_calc_crop_calendars.R \
--args "${gcms[gc]}" "${scens[sc]}" "${crops[cr]}" "${years[yy]}" \
"${nnodes}" "${ntasks}"

      done # yy

# To avoid overloading the squeue
# sleep 1h

    done # cr
  done # sc
done # gc