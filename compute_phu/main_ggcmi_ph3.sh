#!/bin/bash

# Run time per job about 30 min

wd=/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_phu

gcms=('GFDL-ESM4' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'MRI-ESM2-0') #('GFDL-ESM4' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'MRI-ESM2-0' 'UKESM1-0-LL')
scens=('historical' 'ssp585' 'ssp370' 'ssp126')
crops=('wwh' 'swh' 'mai' 'ri1' 'ri2' 'soy' 'mil' 'sor' 'pea' 'sgb' 'cas' 'rap' 'sun' 'nut' 'sgc')
irrigs=('rf' 'ir')


for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for cr in "${!crops[@]}";do
      for ir in "${!irrigs[@]}";do

        echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- CROP: ${crops[cr]}"
        echo "------------------------------------------------------------------"

sbatch --ntasks=1 --cpus-per-task=4 -J nc_${gc}_${sc}_${crops[cr]}_${irrigs[ir]} -A macmit -t 01:00:00 --workdir=${wd} -o out_err/nc_${gc}_${sc}_${crops[cr]}_${irrigs[ir]}.out -e out_err/nc_${gc}_${sc}_${crops[cr]}_${irrigs[ir]}.err R -f main_ggcmi_ph3.R --args "${gcms[gc]}" "${scens[sc]}" "${crops[cr]}" "${irrigs[ir]}"

      done
    done
  done
done
