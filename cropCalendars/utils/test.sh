#!/bin/bash

wd=./utils
echo ${wd}

gcms=('GFDL-ESM4' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'MRI-ESM2-0' 'UKESM1-0-LL')
scens=('ssp585' 'ssp370' 'ssp126')
crops=('Maize' 'Rice' 'Sorghum' 'Soybean' 'Spring_Wheat' 'Winter_Wheat')

gcms=('GFDL-ESM4')
scens=('ssp585')
crops=('Maize')


for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for cr in "${!crops[@]}";do

    echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- CROP: ${crops[cr]}"

    sbatch --nodes=1 --ntasks-per-node=32 -J crop_cal -A macmit -t 02:30:00 \
    --workdir=${wd} R -f test.R --args "${gcms[gc]}" "${scens[sc]}" "${crops[cr]}"

    # to avoid overloading the squeue
    #sleep 1h

    done
  done
done
