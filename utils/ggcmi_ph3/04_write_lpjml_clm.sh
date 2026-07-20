#!/bin/bash
# Stage 04 (annual): write LPJmL CLM inputs (sdate, hdate, phu) from the stage-02 crop
# calendars and stage-03 PHU netCDFs. ONE compact job per gcm x scenario writes all three
# 30-band CLM (version-2) binaries -- see 04_write_lpjml_clm.R.
#
# Guarded on BOTH the stage-02 product and the stage-03 PHU output, so a combo runs only
# once its inputs exist (invalid combos and not-yet-computed scenarios auto-skip).
#
# Writes BOTH the 24-band (12 LPJmL CFTs) and 30-band (15 GGCMI crops) CLM by default.
#
# Env: GCMS (space-sep; default all 6), SCENS (default all), BANDS (default "24 30"),
#      PARTITION (default standard), QOS (default short), DRYRUN=1 (print, don't submit).

source "$(dirname "$(readlink -f "$0")")/settings.sh"

PARTITION=${PARTITION:-standard}
QOS=${QOS:-short}
BANDS=${BANDS:-"24 30"}
GCMS=${GCMS:-"GSWP3-W5E5 GFDL-ESM4 IPSL-CM6A-LR MPI-ESM1-2-HR MRI-ESM2-0 UKESM1-0-LL"}
SCENS=${SCENS:-"historical obsclim ssp126 ssp245 ssp370 ssp585 ssp119 ssp460 ssp534-over"}
WD="$(dirname "$(readlink -f "$0")")"
mkdir -p "${WD}/logs"

# Stage-02 publish dir for a gcm x scenario (mirrors the 02/03/04 R path logic).
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
    PHUDIR="${OUTPUT_DIR}crop_calendars/ncdf/${g}/${s}"
    # gcm-specific stage-02 product AND stage-03 PHU must both be present.
    if [ -z "$(ls -A "$CALDIR"/ggcmi-crop-calendar_${g_lc}_*.nc 2>/dev/null)" ] || \
       [ -z "$(ls -A "$PHUDIR"/*_ggcmi_ph3_rule_based_phu.nc4 2>/dev/null)" ]; then
      skip=$((skip+1)); continue
    fi
    CMD=(sbatch --ntasks=1 --cpus-per-task=1 --mem=8G -t 01:00:00 -J clm_${g}_${s} \
      -A "${ACCOUNT}" --chdir="${WD}" -p "${PARTITION}" --qos=${QOS} \
      -o logs/clm_${g}_${s}-%j.out --export=ALL,BANDS="${BANDS}" \
      --wrap="source ${WD}/env.sh; load_r_env; Rscript 04_write_lpjml_clm.R ${g} ${s}")
    if [ "${DRYRUN:-0}" = "1" ]; then printf '%q ' "${CMD[@]}"; echo; else "${CMD[@]}"; fi
    echo "queued ${g} ${s}"
    n=$((n+1))
  done
done
echo "---- ${n} jobs $([ "${DRYRUN:-0}" = "1" ] && echo PRINTED || echo submitted), ${skip} gcm/scenario combos skipped (inputs missing) ----"
