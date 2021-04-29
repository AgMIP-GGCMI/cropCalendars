#!/bin/bash

# Run time per each job 15 min 

wd=/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate

gcms=('GFDL-ESM4' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'MRI-ESM2-0' 'UKESM1-0-LL')
scens=('historical' 'ssp585' 'ssp370' 'ssp126')
#crops=('Maize' 'Rice' 'Sorghum' 'Soybean' 'Spring_Wheat' 'Winter_Wheat')
#irrigs=('Rainfed' 'Irrigated')
crops=('wwh' 'swh' 'mai' 'ri1' 'ri2' 'soy' 'mil' 'sor' 'pea' 'sgb' 'cas' 'rap' 'sun' 'nut' 'sgc')
irrigs=('rf' 'ir')
#crops=('Maize')
#irrigs=('Rainfed')


for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for cr in "${!crops[@]}";do
      for ir in "${!irrigs[@]}";do

        echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- CROP: ${crops[cr]}"
        echo "------------------------------------------------------------------"

sbatch --ntasks=1 --cpus-per-task=4 -J nc_${gc}_${sc}_${cr}_${ir} -A macmit -t 00:30:00 --workdir=${wd} -o out_err/nc_${gc}_${sc}_${crops[cr]}_${ir}.out -e out_err/nc_${gc}_${sc}_${crops[cr]}_${cr}.err R -f postprocessing/create_ncdf_sdate_hdate_multiyear_ggcmi_ph3.R --args "${crops[cr]}" "${irrigs[ir]}" "${gcms[gc]}" "${scens[sc]}"

      done
    done
  done
done
