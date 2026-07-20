#!/bin/bash
# Stage 01b (annual): rule-based crop calendars from the 01a climatology cache for the FULL
# GCM x scenario matrix (everything 01a has produced climatology for). The heavy ring buffer
# is NOT live here (only one year's climatology at a time), so the mclapply fork is cheap
# (no OOM with R_GC_MEM_GROW=0) and the stage is re-runnable on every sowing/harvest RULE change.
#
# Production run -> BARE output dirs (OUT_SUFFIX unset). All knobs (incl. the validated wet_window
# 20/40, now locked in 00_config.R) come from 00_config.R; no per-knob env overrides needed here.
#
# Env: CORES (default 128), QOS (default standby), YEARS subset (e.g. "2000:2014"),
#      CROPS (rb_cal names, comma-sep; empty=all), DRYRUN=1 (print sbatch lines, don't submit).
#
# Runnable matrix (per available 01a climatology, 2026-06):
#   historical : all 5 GCMs            obsclim : GSWP3-W5E5
#   ssp126/245/370/585 : all 5 GCMs    ssp119/460 : IPSL-CM6A-LR, MRI-ESM2-0 only
#   ssp534-over : IPSL-CM6A-LR, MRI-ESM2-0, UKESM1-0-LL (overshoot, 2040-2100)

source "$(dirname "$(readlink -f "$0")")/settings.sh"
source "$(dirname "$(readlink -f "$0")")/env.sh"

CORES=${CORES:-128}
QOS=${QOS:-standby}
YEARS=${YEARS:-}
CROPS=${CROPS:-}
SCENS=${SCENS:-"historical obsclim ssp126 ssp245 ssp370 ssp585 ssp119 ssp460 ssp534-over"}  # scenario subset
WD="$(dirname "$(readlink -f "$0")")"
CLIM="${WD%/cropCalendars/*}/output/crop_calendars/annual_climatology"   # 01a output root
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
    if [ ! -d "${CLIM}/${s}/${g}" ] || [ -z "$(ls -A "${CLIM}/${s}/${g}"/climatology_*.Rdata 2>/dev/null)" ]; then
      echo "SKIP ${g} ${s}: no climatology at ${CLIM}/${s}/${g}"; skip=$((skip+1)); continue
    fi
    CMD=(sbatch --ntasks=1 --cpus-per-task=${CORES} -t 04:00:00 -J cc_cal_${g}_${s} \
      -A "${ACCOUNT}" --chdir="${WD}" --qos=${QOS} \
      -o logs/cc_cal_${g}_${s}-%j.out \
      --export=ALL,YEARS="${YEARS}",CROPS="${CROPS}" \
      --wrap="source ${WD}/env.sh; load_r_env; export R_GC_MEM_GROW=0; Rscript 01b_calendars_annual.R ${g} ${s} ${CORES}")
    if [ "${DRYRUN:-0}" = "1" ]; then printf '%q ' "${CMD[@]}"; echo; else "${CMD[@]}"; fi
    echo "submitted cc_cal_${g}_${s} (${CORES} cores, ${QOS}, wet_window=20/40, YEARS='${YEARS}')"
    n=$((n+1))
  done
done
echo "---- ${n} jobs $([ "${DRYRUN:-0}" = "1" ] && echo "PRINTED (DRYRUN)" || echo "submitted"), ${skip} skipped ----"
