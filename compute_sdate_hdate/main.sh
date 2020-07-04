#!/bin/bash

#SBATCH --qos=short
#SBATCH --time=03:30:00
#SBATCH --partition=standard
#SBATCH --job-name=grow_periods
#SBATCH --account=macmit
#SBATCH --mail-user=sara.minoli
#SBATCH --workdir=/home/minoli/crop_calendars_gitlab/crop_phen_paper/compute_sdate_hdate
#SBATCH --output=out_err/gp_%j_%a.out
#SBATCH --error=out_err/gp_%j_%a.err

# #SBATCH --array=1-12 # (12 scenarios for preprocessing climate)
#SBATCH --array=1-2 # 1-72 (12 scenarios * 6 crops)

module load intel/2018.1

module load R/3.4.4

echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID

#R -f ./preprocessing/prepare_monthly_climate.R --args $SLURM_ARRAY_TASK_ID

R -f main.R --args $SLURM_ARRAY_TASK_ID
wait

#R -f ./postprocessing/merge_winter_spring_wheat.R --args $SLURM_ARRAY_TASK_ID

#R -f ./postprocessing/mirca2000.R
