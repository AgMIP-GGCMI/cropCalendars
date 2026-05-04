#!/bin/bash

# Bash script for calculating crop calendars for GGCMI phase3 (ISIMIP3b climate)

# Working directory, where the .R file is stored
wd=/p/projects/macmit/users/cmueller/repos/cropCalendars/utils/ggcmi_ph3

# GCM, scenario, crops
# gcms=('GFDL-ESM4' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'MRI-ESM2-0' 'UKESM1-0-LL')
gcms=('UKESM1-0-LL')
scens=('ssp585' 'ssp370' 'ssp126' 'historical' 'picontrol')
crops=('Maize' 'Rice' 'Millet' 'Sorghum' 'Soybean' 'Spring_Wheat' 'Winter_Wheat')

# For running only some scenario
#gcms=('GFDL-ESM4')
#scens=('historical')
#crops=('Millet')
#years=('2071')

# sbatch settings
nnodes=1
ntasks=120  # throws an error if set to 128 that all connections are in use, but 120 works fine

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
-t 02:00:00 -J crop_cal -A macmit --chdir=${wd} --qos=standby \
R -f 01_calc_crop_calendars.R \
--args "${gcms[gc]}" "${scens[sc]}" "${crops[cr]}" "${years[yy]}" \
"${nnodes}" "${ntasks}"
#Rscript --no-save --no-restore \
#01_calc_crop_calendars.R \
#"${gcms[gc]}" "${scens[sc]}" "${crops[cr]}" "${years[yy]}" \
#"${nnodes}" "${ntasks}"
      done # yy

# To avoid overloading the squeue
# sleep 1h

    done # cr
  done # sc
done # gc