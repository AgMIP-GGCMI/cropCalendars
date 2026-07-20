#!/bin/bash
# Stage 01a (annual): build the per-year sliding-window climatology cache for the
# GCM x scenario matrix. I/O + RAM heavy (the ring buffer), but single-threaded and
# fork-free, so memory is bounded (~25 GB). 01b then computes the calendars from this
# cache cheaply and re-runnably.
#
# Env: CORES (default 16; ring only, cpus mainly buy memory budget), QOS (default
#      priority), YEARS subset (e.g. "1990:2014"), SCENS (space-sep scenario subset;
#      default all), DRYRUN=1 (print, don't submit).
#
# Per GCM the historical leg runs with SAVE_SEED=1 (banks the end-of-historical ring
# `seed_ring_<gcm>_w30_end<lastHistYear>.rds`), and each same-GCM SSP leg is submitted
# with a SLURM dependency on that historical job so it LOADS the seed instead of
# re-reading the 30 historical seed years. If historical is NOT in the submitted set
# (e.g. SCENS="ssp534-over"), the SSP legs run standalone and load the on-disk seed.
#
# ssp534-over (SSP5-3.4 overshoot) exists only for IPSL/MRI/UKESM. It is treated as an
# ordinary ssp: seeded from the end-2014 historical ring and written in FULL (2015-2100),
# with ssp585 climate through 2040 then the overshoot's own data (gap-fill in
# 01a_climatology_annual.R). The 2015-2040 calendars come out identical to ssp585, so no
# special downstream handling and no extra seed ring are needed.

source "$(dirname "$(readlink -f "$0")")/settings.sh"
source "$(dirname "$(readlink -f "$0")")/env.sh"

CORES=${CORES:-16}
QOS=${QOS:-priority}
YEARS=${YEARS:-}
WD="$(dirname "$(readlink -f "$0")")"
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

# Canonical scenario order (historical FIRST so the seed wiring picks up its job id).
ORDER="historical obsclim ssp126 ssp245 ssp370 ssp585 ssp119 ssp460 ssp534-over"
SCENS=${SCENS:-$ORDER}

# Union of GCMs across the selected scenarios; looped GCM-major so the per-GCM
# historical -> ssp seed dependency wires up.
gcms=$(for s in $SCENS; do echo ${SC[$s]}; done | tr ' ' '\n' | sort -u)

n=0
for g in $gcms; do
  hist_jid=""
  for s in $ORDER; do
    case " $SCENS " in *" $s "*) ;; *) continue ;; esac      # scenario selected?
    case " ${SC[$s]} " in *" $g "*) ;; *) continue ;; esac   # gcm has this scenario?
    SAVE=0; DEP=""
    case "$s" in
      historical)   SAVE=1 ;;                                  # bank end-of-historical seed (the ssp legs load it)
      ssp*)         [ -n "$hist_jid" ] && DEP="--dependency=afterok:${hist_jid}" ;;
    esac
    CMD=(sbatch --parsable ${DEP} --ntasks=1 --cpus-per-task=${CORES} -t 08:00:00 -J cc_clm_${g}_${s} \
      -A "${ACCOUNT}" --chdir="${WD}" --qos=${QOS} \
      -o logs/cc_clm_${g}_${s}-%j.out \
      --export=ALL,EMIT_STEP=1,YEARS="${YEARS}",SAVE_SEED="${SAVE}" \
      --wrap="source ${WD}/env.sh; load_r_env; Rscript 01a_climatology_annual.R ${g} ${s}")
    if [ "${DRYRUN:-0}" = "1" ]; then printf '%q ' "${CMD[@]}"; echo; jid="DRY"; else jid=$("${CMD[@]}"); fi
    [ "$s" = historical ] && hist_jid=$jid
    echo "submitted cc_clm_${g}_${s} -> ${jid} (SAVE_SEED=${SAVE} ${DEP})"
    n=$((n+1))
  done
done
echo "---- ${n} jobs $([ "${DRYRUN:-0}" = "1" ] && echo PRINTED || echo submitted) ----"
