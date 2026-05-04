#!/bin/bash

#module purge
#module load nco cdo/1.9.5-gcc64 netcdf_c/4.3.2-gcc48
#module load nco/5.1.0 cdo/1.9.10/gnu netcdf-c/4.9.0/gnu/10.2
source rhel8_stack
module load nco/5.1.9-spack
module load cdo/2.4.0-spack

#BASE_DIR_ROOT=/p/projects/macmit/users/minoli/PROJECTS/GGCMI_ph3_adaptation/ISIMIP3bv2/crop_calendars/ncdf
BASE_DIR_ROOT=/p/projects/macmit/data/GGCMI/phase3/GGCMI_ph3_adaptation_cropping_calendars/ISIMIP3b/InputData/socioeconomic/crop_calendar  #/home/delvalle/data/ISIMIP3a/InputData/socioeconomic/crop_calendar

#for PERIOD in $periods; do
#BASE_DIR=$BASE_DIR_ROOT #/$PERIOD
GCMS="GFDL-ESM4 IPSL-CM6A-LR MPI-ESM1-2-HR MRI-ESM2-0 UKESM1-0-LL"
SPECS="ssp585soc-adapt ssp370soc-adapt ssp126soc-adapt historical"

REF_DATE="1601-01-01,00:00:00,1year"
CALENDAR="standard"

for GCM in $GCMS; do

  GCM_LC=$(echo $GCM | tr '[:upper:]' '[:lower:]')
  echo $GCM_LC



for SPEC in $SPECS;do
  
  BASE_DIR=${BASE_DIR_ROOT}/$GCM/$SPEC
  echo $BASE_DIR

for FILE in $(find $BASE_DIR -maxdepth 3 -type f | grep annual | sort );do

  echo "  "
  echo $FILE

  
#  if [ ! -f ${FILE}.tmp ]; then

#    continue

  if [ -z "$(ncdump -hs $FILE | grep missing_value)" ]; then
#    echo $FILE $(ncdump -hs $FILE | grep missing_value)
    ncatted -O -h -a missing_value,,o,f,1e+20 $FILE 2> /dev/null
#    exit
  fi

#  VARS=$(cdo -s showname $FILE)
#  for VAR in $VARS; do 
#    echo $VAR
#    ncatted -O -h -a _FillValue,$VAR,o,f,1e+20 $FILE 2> /dev/null
#  done
#    mv $FILE.tmp $FILE
#      exit
#  fi
done
done
done
