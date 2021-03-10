#!/bin/bash

wd=/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate

gcms=('UKESM1-0-LL')
scens=('ssp585')
crops=('Maize' 'Rice' 'Sorghum' 'Soybean' 'Spring_Wheat' 'Winter_Wheat')


for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for cr in "${!crops[@]}";do
        
        echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- CROP: ${crops[cr]}"
        echo "------------------------------------------------------------------"

sbatch -J nc_cropcal -A macmit -t 03:00:00 --workdir=${wd} -o out_err/nc_${gc}_${sc}_${crops[cr]}.out -e out_err/nc_${gc}_${sc}_${crops[cr]}.err R -f postprocessing/create_ncdf_sdate_hdate_multiyear_ggcmi_ph3.R --args "${crops[cr]}" "${gcms[gc]}" "${scens[sc]}"

    done
  done
done

