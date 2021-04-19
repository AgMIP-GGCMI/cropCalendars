#!/bin/bash

wd=/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_phu

gcms=('UKESM1-0-LL')
scens=('historical' 'ssp585' 'ssp370' 'ssp126')

for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do

        echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]}"
        echo "-------------------------------------------"

sbatch --qos=standby --ntasks=1 --cpus-per-task=4 -J clm_${gc}_${sc} -A macmit -t 01:00:00 --workdir=${wd} -o out_err/clm_${gc}_${sc}.out -e out_err/clm_${gc}_${sc}.err R -f postprocessing/create_lpjml_sdate_hdate_phu_input.R --args "${gcms[gc]}" "${scens[sc]}"

  done
done
