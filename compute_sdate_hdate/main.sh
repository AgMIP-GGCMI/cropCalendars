#!/bin/bash

#SBATCH --qos=short
#SBATCH --time=03:30:00
#SBATCH --partition=standard
#SBATCH --job-name=grow_periods
#SBATCH --account=macmit
#SBATCH --mail-user=sara.minoli
#SBATCH --workdir=/p/projects/macmit/users/minoli/PROJECTS/CROP_PHENOLOGY_v01/SCRIPTS/GROWING_PERIODS_PACKAGE/CODE
#SBATCH --output=out_err/gp_%j_%a.out
#SBATCH --error=out_err/gp_%j_%a.err

#SBATCH --array=1-72 # 1-72 (12 scenarios * 6 crops)

module load intel/2018.1

module load R/3.4.4

echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID

R -f main.R --args $SLURM_ARRAY_TASK_ID
wait

R -f ./postprocessing/merge_winter_spring_wheat.R --args $SLURM_ARRAY_TASK_ID

R -f ./postprocessing/mirca2000.R
