#!/bin/bash
# Stage 03 (annual): decadal PHU netCDFs for LPJmL from the stage-02 crop calendars.
# ONE compact job per gcm x scenario -- 03_calc_phu_for_lpjml.R loops all 15 crops x 2
# irrigations internally, reading each year's daily temperature ONCE and reusing it across
# crops (temperature is crop-independent). The PHU is DECADAL (constant within a decade),
# read off the decadal-representative dates -- see generatePHUTserie_isimip3().
#
# Stage-02 output existence is the true gcm x scenario gate, so invalid combos (e.g.
# GFDL ssp119, or ssp534-over before its stage 02) auto-skip.
#
# Env: GCMS (space-sep; default all 6), SCENS (default all), CROPS (ggcmi tokens passed
#      through to restrict crops; default all 15), PARTITION (default standard), QOS
#      (default short), DRYRUN=1 (print, don't submit).

source "$(dirname "$(readlink -f "$0")")/settings.sh"

PARTITION=${PARTITION:-standard}
QOS=${QOS:-short}
CORES=${CORES:-4}      # parallelise the per-crop PHU (the few vernal crops are the long poles)
GCMS=${GCMS:-"GSWP3-W5E5 GFDL-ESM4 IPSL-CM6A-LR MPI-ESM1-2-HR MRI-ESM2-0 UKESM1-0-LL"}
SCENS=${SCENS:-"historical obsclim ssp126 ssp245 ssp370 ssp585 ssp119 ssp460 ssp534-over"}
CROPS=${CROPS:-}
WD="$(dirname "$(readlink -f "$0")")"
mkdir -p "${WD}/logs"

# Stage-02 publish dir for a gcm x scenario (mirrors the 02/03 R path logic).
cal_dir() {
  local g="$1" s="$2"
  case "$s" in
    obsclim|spinclim) echo "${OUTPUT_DIR}ISIMIP3a/InputData/socioeconomic/crop_calendar/histsoc" ;;
    counterclim)      echo "${OUTPUT_DIR}ISIMIP3a/InputData/socioeconomic/crop_calendar/countersoc" ;;
    historical)       echo "${OUTPUT_DIR}ISIMIP3b/InputData/socioeconomic/crop_calendar/${g}/historical" ;;
    *)                echo "${OUTPUT_DIR}ISIMIP3b/InputData/socioeconomic/crop_calendar/${g}/${s}soc-adapt" ;;
  esac
}

n=0; skip=0
for g in $GCMS; do
  g_lc=$(echo "$g" | tr 'A-Z' 'a-z')
  for s in $SCENS; do
    CALDIR="$(cal_dir "$g" "$s")"
    # Match the gcm-specific filename, not just the dir: the ISIMIP3a histsoc dir is shared
    # (gcm token is in the filename), so a bare dir check would queue obsclim for every gcm.
    if [ ! -d "$CALDIR" ] || [ -z "$(ls -A "$CALDIR"/ggcmi-crop-calendar_${g_lc}_*.nc 2>/dev/null)" ]; then
      skip=$((skip+1)); continue
    fi
    CMD=(sbatch --ntasks=1 --cpus-per-task=${CORES} --mem=20G -t 08:00:00 -J phu_${g}_${s} \
      -A "${ACCOUNT}" --chdir="${WD}" -p "${PARTITION}" --qos=${QOS} \
      -o logs/phu_${g}_${s}-%j.out \
      --export=ALL,CROPS="${CROPS}" \
      --wrap="source ${WD}/env.sh; load_r_env; export R_GC_MEM_GROW=0; Rscript 03_calc_phu_for_lpjml.R ${g} ${s} ${CORES}")
    if [ "${DRYRUN:-0}" = "1" ]; then printf '%q ' "${CMD[@]}"; echo; else "${CMD[@]}"; fi
    echo "queued ${g} ${s} (all crops x rf/ir, one job)"
    n=$((n+1))
  done
done
echo "---- ${n} jobs $([ "${DRYRUN:-0}" = "1" ] && echo PRINTED || echo submitted), ${skip} gcm/scenario combos skipped (no stage-02 product) ----"
