#!/bin/bash

wd=/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate

gcms=('UKESM1-0-LL')
scens=('ssp585' 'ssp370' 'ssp126') #'ssp585' 'ssp370' 'ssp126'
crops=('mai' 'ric' 'sor' 'soy' 'swh' 'wwh')
irrigs=('rf' 'ir')
vars=('plant-day' 'maty-day' 'grow-days')
#crops=('Maize')
#irrigs=('Rainfed')


for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for cr in "${!crops[@]}";do
      for ir in "${!irrigs[@]}";do
        for vr in "${!vars[@]}";do
        
        echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- CROP: ${crops[cr]}"
        echo "------------------------------------------------------------------"

R -f ${wd}/postprocessing/plot_ncdf_sdate_hdate_multiyear_ggcmi_ph3.R --args "${crops[cr]}" "${irrigs[ir]}" "${gcms[gc]}" "${scens[sc]}" "${vars[vr]}"

        done
      done
    done
  done
done

