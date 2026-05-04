#!/bin/bash

#module purge
#module load nco cdo/1.9.5-gcc64 netcdf_c/4.3.2-gcc48
source rhel8_stack
module load nco/5.1.9-spack
module load cdo/2.4.0-spack  

BASE_DIR_ROOT=/p/projects/macmit/data/GGCMI/phase3/GGCMI_ph3_adaptation_cropping_calendars/crop_calendars/ncdf
BASE_DIR_OUT=/p/projects/macmit/data/GGCMI/phase3/GGCMI_ph3_adaptation_cropping_calendars/ISIMIP3b/InputData/socioeconomic/crop_calendar  #/home/delvalle/data/ISIMIP3a/InputData/socioeconomic/crop_calendar

#for PERIOD in $periods; do
BASE_DIR=$BASE_DIR_ROOT #/$PERIOD
GCMS="GFDL-ESM4 IPSL-CM6A-LR MPI-ESM1-2-HR MRI-ESM2-0 UKESM1-0-LL"
SPECS_RAW="ssp585 ssp370 ssp126 histsoc"

# Specifier changes
#RCP26-farfuture --> rcp26

# Specifier of files to change
SPEC="tasmax"

for GCM in $GCMS; do

  GCM_LC=$(echo $GCM | tr '[:upper:]' '[:lower:]')
  echo $GCM_LC

for SPEC in $SPECS_RAW; do

  SOC_DIR=""
  SPEC_NEW=""
  if [ "$SPEC" != "histsoc" ]; then
    SPEC_NEW=${SPEC}
    SOC_DIR=${SPEC}soc-adapt
  else
    SPEC_NEW=$SPEC
    SOC_DIR="historical"
    SPEC="historical"
  fi
  echo $SPEC_NEW $SOC_DIR

#  exit
  BASE_DIR=${BASE_DIR_ROOT}/$GCM/$SPEC

  OUT_DIR=""
  OUT_DIR=${BASE_DIR_OUT}/$GCM/${SOC_DIR}

  echo $BASE_DIR
  echo $OUT_DIR

  [ ! -d $OUT_DIR ] && echo "... OUTPUT directory missing" && mkdir -p $OUT_DIR

#  exit
  
  for FILE in $(find $BASE_DIR -maxdepth 3 -type f | grep _crop_calendar.nc | sort );do

    echo "  "
        echo $FILE
    
    VAR=$(basename $FILE | awk -F"_" '{print $1}')
    IRR=$(basename "$FILE" | awk -F"_" '{print $2}' | sed -e 's/^rf$/noirr/' -e 's/^ir$/firr/')
#    echo $VAR $IRR

#    exit

    DATES=$(basename $FILE | awk -F"_" '{print $5}' | sed -e 's/-/_/')
#    echo $DATES

    #FILE_NEW=$OUT_DIR/ggcmi-crop-calendar_${GCM_LC}_${SPEC_NEW}_${VAR}-${IRR}_${DATES}.nc
    FILE_NEW=$OUT_DIR/ggcmi-crop-calendar_${GCM_LC}_${SPEC_NEW}_${VAR}-${IRR}_${DATES}.nc
    echo $FILE_NEW
    
    # Correct file name
    
    if [ ! -f $FILE_NEW ]; then
#      echo "... creating" $FILE_NEW
#      echo $(basename $FILE)
#      echo $(basename $FILE_NEW)
      cp -v $FILE $FILE_NEW
#          exit
    fi

    # Correct variable names
    ncrename -O -v plant-day-mavg,planting_day-mavg \
	  -v plant-day-mavg-window,planting_day-mavg-window \
		-v plant-day,planting_day \
		-v maty-day,maturity_day \
		-v grow-period,growing_period \
		-v harv-reason,harvest_reason \
		-v plant-season,planting_season $FILE_NEW
	
  done
  
done
done

