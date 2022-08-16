#!/bin/bash

# Run time per job: 15 min

# Working directory, where the .R file is stored
wd=/home/minoli/crop_calendars_gitlab/r_package/cropCalendars/utils/ggcmi_ph3

# GCM, scenario, crops, irrigations
gcms=('GFDL-ESM4' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'MRI-ESM2-0' 'UKESM1-0-LL')
scens=('ssp585' 'ssp370' 'ssp126' 'historical' '2015gs' 'picontrol')
crops=('wwh' 'swh' 'mai' 'ri1' 'ri2' 'soy' 'mil' 'sor' 'pea' 'sgb' 'cas' 'rap' 'sun' 'nut' 'sgc')
irrigs=('rf' 'ir')

gcms=('GFDL-ESM4')
scens=('ssp585')
crops=('mai')
irrigs=('rf')

for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for cr in "${!crops[@]}";do
      for ir in "${!irrigs[@]}";do

        echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- CROP: ${crops[cr]}"
        echo "------------------------------------------------------------------"

sbatch --ntasks=1 --cpus-per-task=4 -J nc_${gc}_${sc}_${cr}_${ir} -A macmit \
-t 00:30:00 --workdir=${wd} \
R -f 02_generate_crop_cal_timeseries.R \
--args "${gcms[gc]}" "${scens[sc]}" "${crops[cr]}" "${irrigs[ir]}"

      done
    done
    #sleep 5m
  done
done

# -o out_err/nc_${gcms[gc]}_${scens[sc]}_${crops[cr]}_${irrigs[ir]}_3b.out -e out_err/nc_${gcms[gc]}_${scens[sc]}_${crops[cr]}_${irrigs[ir]}_3b.err