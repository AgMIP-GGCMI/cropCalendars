#!/bin/bash

# Run time per job about 30 min

wd=/home/minoli/crop_calendars_gitlab/r_package/cropCalendars/utils/ggcmi_ph3

gcms=('GFDL-ESM4' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'MRI-ESM2-0' 'UKESM1-0-LL')
scens=('historical' 'ssp585' 'ssp370' 'ssp126' '2015gs')
crops=('wwh' 'swh' 'mai' 'ri1' 'ri2' 'soy' 'mil' 'sor' 'pea' 'sgb' 'cas' 'rap' 'sun' 'nut' 'sgc')
irrigs=('rf' 'ir')

#gcms=('GFDL-ESM4')
scens=('2015gs')
crops=('ri2')
#irrigs=('rf')

for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for cr in "${!crops[@]}";do
      for ir in "${!irrigs[@]}";do

        echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- CROP: ${crops[cr]}"
        echo "------------------------------------------------------------------"

sbatch --qos=standby --ntasks=1 --cpus-per-task=4 \
-J nc_${gcms[gc]}_${scens[sc]}_${crops[cr]}_${irrigs[ir]} \
-A macmit -t 01:00:00 --workdir=${wd} \
R -f 03_calc_phu_for_lpjml.R \
--args "${gcms[gc]}" "${scens[sc]}" "${crops[cr]}" "${irrigs[ir]}"

      done
    done
  done
#sleep 10m
done
