# ------------------------------------------------------# ----
# Project: crop_calendars
#
# Author:  Sara Minoli
# Email:   sara.minoli@pik-potsdam.de
#
# Date Created: 2021-04-20
#
# Description: tests if given season should be classified as winter crop
# # Based on scripts by Jonas Jägermeyr
# ------------------------------------------------------#

# tests if given season should be classified as winter crop
# takes dates in DOY
# this is the rule suggested by Portman et al. 2010,
# slightly changed in that <= 7 instead of 6°C is used
# more details in "/p/projects/waterforce/jonas/R_functions/wintercrop.R"


wintercrop <- function(start, end, tcm, lat) {
  
  # tcm = temp of coldest month
  # start / end = sdate / hdate
  
  growp <- ifelse(start<=end, end-start, 365+end-start)
  wc <- 0
  
  if(!is.na(start) && start>0 && !is.na(lat) && !is.na(tcm)) {
    
    if(lat>0) {
      
      if ( ((start+growp > 365) && (growp >= 150)) &&
           (tcm >= -10 && tcm <= 7) ) {
        wc <- 1
      }
      
    } else {
      
      if ( ((start < 182) && (start+growp > 182) && (growp >= 150)) &&
           (tcm >= (-10) && tcm <= 7) ) {
        wc <- 1
      }
      
    }
  }
  
  return(wc)
  
}

# 
# wintercrop<-function(start, end, cm) {
#   
#   # cm = temp of coldest month
#   # start / end = sdate / hdate
#   wc=start
#   length=ifelse(start<=end, end-start, 365+end-start)
#   
#   wc[which(!is.na(start))]=0
#   
#   for(i in 1:length(wc)) {
#     if(!is.na(start[i]) && start[i]>0 && !is.na(lat[i]) && !is.na(cm[i])) {
#       if(lat[i]>0) {
#         if(((start[i] + length[i] > 365) && (length[i] >= 150)) && (cm[i]>= -10 && cm[i]<= 7)) {
#           wc[i]=1
#         }
#       } else {
#         if(((start[i]<182) && (start[i] + length[i] > 182) && (length[i] >= 150)) && (cm[i]>= -10 && cm[i]<= 7)) {
#           wc[i]=1
#         }
#       }
#     }
#   } # cell
#   return(wc)
# }
