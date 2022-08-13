#!/bin/bash

wd=/home/minoli/crop_calendars_gitlab/r_package/cropCalendars/utils
echo ${wd}

gcms=('GFDL-ESM4' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'MRI-ESM2-0' 'UKESM1-0-LL')
scens=('ssp585' 'ssp370' 'ssp126' 'historical')
crops=('Maize' 'Rice' 'Sorghum' 'Soybean' 'Spring_Wheat' 'Winter_Wheat')
years=($(seq 1601 10 2091))

gcms=('GFDL-ESM4')
scens=('ssp370')
crops=('Maize')
years=('2091')


for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for cr in "${!crops[@]}";do
      for yy in "${!crops[@]}";do

    echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- CROP: ${crops[cr]} YEARS: ${years[yy]}"

    # OK
    sbatch --qos=standby --nodes=3 --ntasks-per-node=16 -J crop_cal -A macmit --workdir=${wd} \
    R -f test.R --args "${gcms[gc]}" "${scens[sc]}" "${crops[cr]}" "${years[yy]}"

    # to avoid overloading the squeue
    #sleep 1h
      done
    done
  done
done


  # sbatch --qos=standby --ntasks=72 --mem-per-cpu=3G -J crop_cal -A macmit --workdir=${wd} \
  # R -f test.R --args "${gcms[gc]}" "${scens[sc]}" "${crops[cr]}" "${years[yy]}"