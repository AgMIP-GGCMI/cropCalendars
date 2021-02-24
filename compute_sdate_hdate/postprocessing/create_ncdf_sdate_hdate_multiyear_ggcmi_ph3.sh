#!/bin/bash

wd=/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate/postprocessing/

gcms="UKESM1-0-LL"
ssps="ssp585"
crops="Maize Rice Sorghum Soybean Spring_Wheat Winter_Wheat"
# crops="Maize Rice"

for ssp  in ${ssps}; do
    for gcm  in ${gcms}; do
        for crop in ${crops}; do

        echo "-----------------------"
        echo "Creating ncdf for $crop"
        echo "-----------------------"

        Rscript ${wd}create_ncdf_sdate_hdate_multiyear_ggcmi_ph3.R "${crop}" "${gcm}" "${ssp}" 

        done
    done
done
