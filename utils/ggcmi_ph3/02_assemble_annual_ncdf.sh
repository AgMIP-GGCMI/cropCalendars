#!/bin/bash
# Stage 02 (annual): assemble the annual crop-calendar NetCDFs (run AFTER 01b). The
# crops are fast, so ONE job per gcm x scenario builds the whole 15x2 crop-irrigation
# matrix in a single R process (02_assemble_annual_ncdf.R loops it internally).
#
# Matrix mirrors 01b (per available annual calendars):
#   historical : all 5 GCMs            obsclim : GSWP3-W5E5
#   ssp126/245/370/585 : all 5 GCMs    ssp119/460 : IPSL-CM6A-LR, MRI-ESM2-0 only
#   ssp534-over : IPSL-CM6A-LR, MRI-ESM2-0, UKESM1-0-LL (overshoot, 2040-2100)
#
# Env: PARTITION (default standard), QOS (default short -- standard-partition
#      production qos, <=1d wall), DRYRUN=1 (print, don't submit).

source "$(dirname "$(readlink -f "$0")")/settings.sh"

PARTITION=${PARTITION:-standard}
QOS=${QOS:-short}
SCENS=${SCENS:-"historical obsclim ssp126 ssp245 ssp370 ssp585 ssp119 ssp460 ssp534-over"}  # scenario subset
GCMS=${GCMS:-"GSWP3-W5E5 GFDL-ESM4 IPSL-CM6A-LR MPI-ESM1-2-HR MRI-ESM2-0 UKESM1-0-LL"}       # gcm subset
WD="$(dirname "$(readlink -f "$0")")"
ANN="${OUTPUT_DIR}crop_calendars/annual"
mkdir -p "${WD}/logs"

ALL5="GFDL-ESM4 IPSL-CM6A-LR MPI-ESM1-2-HR MRI-ESM2-0 UKESM1-0-LL"
IM="IPSL-CM6A-LR MRI-ESM2-0"
IMU="IPSL-CM6A-LR MRI-ESM2-0 UKESM1-0-LL"
declare -A SC
SC[historical]="$ALL5"
SC[obsclim]="GSWP3-W5E5"
SC[ssp126]="$ALL5"
SC[ssp245]="$ALL5"
SC[ssp370]="$ALL5"
SC[ssp585]="$ALL5"
SC[ssp119]="$IM"
SC[ssp460]="$IM"
SC[ssp534-over]="$IMU"

n=0; skip=0
for s in $SCENS; do
  for g in ${SC[$s]}; do
    case " $GCMS " in *" $g "*) ;; *) continue ;; esac   # gcm subset filter
    if [ ! -d "${ANN}/${s}/${g}" ] || [ -z "$(ls -A "${ANN}/${s}/${g}"/annual_calendar_*.Rdata 2>/dev/null)" ]; then
      echo "SKIP ${g} ${s}: no annual calendars at ${ANN}/${s}/${g}"; skip=$((skip+1)); continue
    fi
    CMD=(sbatch --ntasks=1 --cpus-per-task=1 --mem=16G -t 01:30:00 -J nc_annual_${g}_${s} \
      -A "${ACCOUNT}" --chdir="${WD}" -p "${PARTITION}" --qos=${QOS} \
      -o logs/nc_annual_${g}_${s}-%j.out \
      --wrap="source ${WD}/env.sh; load_r_env; Rscript 02_assemble_annual_ncdf.R ${g} ${s}")
    if [ "${DRYRUN:-0}" = "1" ]; then printf '%q ' "${CMD[@]}"; echo; else "${CMD[@]}"; fi
    echo "submitted nc_annual_${g}_${s} (${PARTITION}/${QOS})"
    n=$((n+1))
  done
done
echo "---- ${n} jobs $([ "${DRYRUN:-0}" = "1" ] && echo "PRINTED (DRYRUN)" || echo "submitted"), ${skip} skipped ----"
