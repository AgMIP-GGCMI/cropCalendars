#!/bin/bash

module load piam


wd="/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate"


# export I_MPI_PMI_LIBRARY=/p/system/slurm/lib/libpmi.so

for scenario in `seq 1 2`; do
  for crop in `seq 1 2`; do

    echo "SCENARIO: $scenario --- CROPS: $crop"

    #sbatch --nodes=1 --ntasks=16 --qos=standby -J crop_cal -A macmit -t 02:00:00 -o ${wd}/out_err/cropcal_%j_${scenario}_${crop}.out -e ${wd}/out_err/cropcal_%j_${scenario}_${crop}.err R -f ${wd}/main_ggcmi_ph3.R --args "${scenario}" "${crop}" 
    
    #sbatch --qos=standby --nodes=1 --ntasks-per-node=16 --cpus-per-task=1 -J crop_cal -A macmit -t 02:00:00 -o ${wd}/out_err/cropcal_%j_${scenario}_${crop}.out -e ${wd}/out_err/cropcal_%j_${scenario}_${crop}.err R -f ${wd}/main_ggcmi_ph3.R --args "${scenario}" "${crop}"

    sbatch --qos=standby --nodes=1 --ntasks-per-node=16 -J crop_cal -A macmit -t 02:30:00 -o ${wd}/out_err/cropcal_%j_${scenario}_${crop}.out -e ${wd}/out_err/cropcal_%j_${scenario}_${crop}.err R -f ${wd}/main_ggcmi_ph3.R --args "${scenario}" "${crop}"

  done
done


# R -f ./postprocessing/merge_winter_spring_wheat.R --args $SLURM_ARRAY_TASK_ID




# R -f ./postprocessing/mirca2000.R

# Array 1-12 or 13
# R -f ./postprocessing/create_lpjml_sdate_hdate_input.R --args  $SLURM_ARRAY_TASK_ID
