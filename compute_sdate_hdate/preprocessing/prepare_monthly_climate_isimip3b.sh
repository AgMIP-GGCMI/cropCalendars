#!/bin/bash

#SBATCH --time=01:00:00
#SBATCH --cpus-per-task=4 #request 16GB
#SBATCH --job-name=mclm
#SBATCH --account=macmit
#SBATCH --mail-user=minoli@pik-potsdam.de
#SBATCH --workdir=/home/minoli/crop_calendars_gitlab/ggcmi_ph3/compute_sdate_hdate
#SBATCH --output=out_err/mclm_%j_%a.out
#SBATCH --error=out_err/mclm_%j_%a.err

#SBATCH --array=1-2

module load piam

echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID

R -f ./preprocessing/prepare_monthly_climate_isimip3b.R --args $SLURM_ARRAY_TASK_ID
