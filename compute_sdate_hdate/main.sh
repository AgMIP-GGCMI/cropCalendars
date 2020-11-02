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

# #SBATCH --array=1-12 # (12 scenarios for: prepare_monthly_climate.R; create_lpjml_sdate_hdate_input.R)
# #SBATCH --array=13     # (1 additional scenario for prepare_monthly_climate_WFDEI.R)
# #SBATCH --array=1-78 # 1-78 (13 scenarios * 6 crops)
#SBATCH --array=13,26,39,52,65,78 (WFDEI only)

module load intel/2018.1

module load R/3.4.4

echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID

# Array 1-12
#R -f ./preprocessing/prepare_monthly_climate.R --args $SLURM_ARRAY_TASK_ID

# Array 13
#R -f ./preprocessing/prepare_monthly_climate_WFDEI.R --args $SLURM_ARRAY_TASK_ID

# Array 1-72
# R -f main.R --args $SLURM_ARRAY_TASK_ID

# Array 1-72
R -f ./postprocessing/merge_winter_spring_wheat.R --args $SLURM_ARRAY_TASK_ID

#R -f ./postprocessing/mirca2000.R

# Array 1-12 or 13
# R -f ./postprocessing/create_lpjml_sdate_hdate_input.R --args  $SLURM_ARRAY_TASK_ID
