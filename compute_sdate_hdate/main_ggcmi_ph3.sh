#!/bin/bash

module load piam


module load piam

gcms=('UKESM1-0-LL')
scens=('ssp585')
syears=('1995' '2001' '2011' '2021' '2031' '2041' '2051' '2061' '2071' '2081')
eyears=('2014' '2020' '2030' '2040' '2050' '2060' '2070' '2080' '2090' '2100')
crops=('Maize' 'Rice' 'Sorghum' 'Soybean' 'Spring_Wheat' 'Winter_Wheat')

#gcms=('UKESM1-0-LL')
#scens=('ssp585')
#syears=('1995')
#eyears=('2014')
#crops=('Maize')


wd=/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate

for gc in "${!gcms[@]}";do
  for sc in "${!scens[@]}";do
    for yy in "${!syears[@]}";do
      for cr in "${!crops[@]}";do

        echo "GCM: ${gcms[gc]} --- SCENARIO: ${scens[sc]} --- YEAR: ${syears[yy]} --- CROP: ${crops[cr]}"

sbatch --nodes=1 --ntasks-per-node=16 -J crop_cal -A macmit -t 02:30:00 --workdir=${wd} -o out_err/cropcal_%j_${gc}_${syears[yy]}_${crops[cr]}.out -e out_err/cropcal_%j_${gc}_${syears[yy]}_${crops[cr]}.err R -f main_ggcmi_ph3.R --args "${gcms[gc]}" "${scens[sc]}" "${syears[yy]}" "${eyears[yy]}" "${crops[cr]}"

      done
    done
  done
done


#------

# wd="/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate"


# export I_MPI_PMI_LIBRARY=/p/system/slurm/lib/libpmi.so

# for scenario in `seq 1 10`; do
#   for crop in `seq 1 6`; do

#    echo "SCENARIO: $scenario --- CROPS: $crop"

    #sbatch --nodes=1 --ntasks=16 --qos=standby -J crop_cal -A macmit -t 02:00:00 -o ${wd}/out_err/cropcal_%j_${scenario}_${crop}.out -e ${wd}/out_err/cropcal_%j_${scenario}_${crop}.err R -f ${wd}/main_ggcmi_ph3.R --args "${scenario}" "${crop}" 
    
    #sbatch --qos=standby --nodes=1 --ntasks-per-node=16 --cpus-per-task=1 -J crop_cal -A macmit -t 02:00:00 -o ${wd}/out_err/cropcal_%j_${scenario}_${crop}.out -e ${wd}/out_err/cropcal_%j_${scenario}_${crop}.err R -f ${wd}/main_ggcmi_ph3.R --args "${scenario}" "${crop}"

#     sbatch --qos=standby --nodes=1 --ntasks-per-node=16 -J crop_cal -A macmit -t 02:30:00 -o ${wd}/out_err/cropcal_%j_${scenario}_${crop}.out -e ${wd}/out_err/cropcal_%j_${scenario}_${crop}.err R -f ${wd}/main_ggcmi_ph3.R --args "${scenario}" "${crop}"

#   done
# done


#-----

# R -f ./postprocessing/merge_winter_spring_wheat.R --args $SLURM_ARRAY_TASK_ID




# R -f ./postprocessing/mirca2000.R

# Array 1-12 or 13
# R -f ./postprocessing/create_lpjml_sdate_hdate_input.R --args  $SLURM_ARRAY_TASK_ID
