#!/bin/bash

wd=/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_phu

gcms=('UKESM1-0-LL')
scens=('historical')
crops=('mai' 'ri1' 'sor' 'soy' 'swh' 'wwh')
irrigs=('rf' 'ir')


for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for cr in "${!crops[@]}";do
      for ir in "${!irrigs[@]}";do

        echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- CROP: ${crops[cr]}"
        echo "------------------------------------------------------------------"

sbatch --qos=standby --ntasks=1 --cpus-per-task=6 -J nc_${gc}_${sc}_${cr}_${ir} -A macmit -t 01:00:00 --workdir=${wd} -o out_err/nc_${gc}_${sc}_${crops[cr]}_${ir}.out -e out_err/nc_${gc}_${sc}_${crops[cr]}_${cr}.err R -f main_ggcmi_ph3_obs-calendars.R --args "${gcms[gc]}" "${scens[sc]}" "${crops[cr]}" "${irrigs[ir]}"

      done
    done
  done
done
