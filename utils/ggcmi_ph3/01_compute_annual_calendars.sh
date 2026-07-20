#!/bin/bash
# Stage 01 (annual): compute the annual sliding-window crop calendars for the full
# GCM x scenario matrix. One job per (GCM, scenario). Each reads its raw climate
# once, maintains a 30-yr ring, and writes per-crop annual calendars.
#
# Future scenarios (2015-2100) seed the ring from historical climate (the driver
# concatenates the historical + scenario file lists and anchors at the data start),
# so 2015's calendar uses 1985-2014 historical climate and the ring then slides
# into scenario climate.

source "$(dirname "$(readlink -f "$0")")/settings.sh"
source "$(dirname "$(readlink -f "$0")")/env.sh"

gcms=('GFDL-ESM4' 'IPSL-CM6A-LR' 'MPI-ESM1-2-HR' 'MRI-ESM2-0' 'UKESM1-0-LL')
scens=('historical' 'ssp126' 'ssp370' 'ssp585')   # add 'picontrol' (1601-2100) if needed
CORES=${CORES:-64}
QOS=${QOS:-priority}                                # priority (<=64 cores, immediate) or standby
# R_GC_MEM_GROW=0 (set in the wrap below): keep R's heap arena/GC-trigger tight so the
# 64 mclapply workers collect aggressively instead of inheriting an inflated trigger and
# each ballooning ~2x via copy-on-write of the large parent heap (ring buffer + OUT) ->
# 340 GB OOM. Tight growth restores the ~168 GB profile. See computeYear note in the .R.
WD="$(dirname "$(readlink -f "$0")")"
mkdir -p "${WD}/logs"

for g in "${gcms[@]}"; do
  for s in "${scens[@]}"; do
    sbatch --ntasks=1 --cpus-per-task=${CORES} -t 08:00:00 -J cc_ann_${g}_${s} \
      -A "${ACCOUNT}" --chdir="${WD}" --qos=${QOS} \
      -o logs/cc_annual_${g}_${s}-%j.out \
      --export=ALL,EMIT_STEP=1 \
      --wrap="source ${WD}/env.sh; load_r_env; export R_GC_MEM_GROW=0; Rscript 01_compute_annual_calendars.R ${g} ${s} ${CORES}"
    echo "submitted cc_ann_${g}_${s} (${CORES} cores, ${QOS})"
  done
done
