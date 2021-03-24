#!/bin/bash

module load piam

gcms=('UKESM1-0-LL')
scens=('ssp126') #'ssp858' 'ssp370' 'ssp126'
syears=('1961' '1971' '1981' '1991' '2001' '2011' '2021' '2031' '2041' '2051' '2061' '2071')
eyears=('1990' '2000' '2010' '2020' '2030' '2040' '2050' '2060' '2070' '2080' '2090' '2100')
#syears=('1961' '1971' '1981')
#eyears=('1990' '2000' '2010')

wd=/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate

for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for yy in "${!syears[@]}";do

        echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- YEAR: ${syears[yy]}"

        sbatch --ntasks=1 --cpus-per-task=12 -t 01:00:00 -J mclm -A macmit --workdir=${wd} -e out_err/mclm.%j.err -o out_err/mclm.%j.out R --file=preprocessing/prepare_monthly_climate_isimip3b.R --args "${gcms[gc]}" "${scens[sc]}" "${syears[yy]}" "${eyears[yy]}"

    done
  done
done
