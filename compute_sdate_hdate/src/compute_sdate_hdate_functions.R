# Functions ----

# Units 
doy2month <- function(doy, year = 2015) {strptime(paste(year, doy), format="%Y %j")$mon+1}
k2deg <- function(t_kelvin) {t_kelvin-273.15} #temperature units from K to °C
deg2k <- function(t_deg) {t_deg+273.15} #temperature units from K to °C
ha2kmq <- function(ha) {ha/100}
kmq2ha <- function(kmq) {kmq*100}
deg2rad <- function(deg) {((deg)*M_PI*.00555555555555555555)}



# Calculate days where thresholds are crossed from monthly values
calcDoyCrossThreshold <- function(monthly_value, threshold) {
  
  # Middle day of each month
  midday <- c(15,43,74,104,135,165,196,227,257,288,318,349) #midday[NMONTH+1]
  NDAYYEAR <- 365
  
  # Replicate values twice (two years)
  midday2 <- c(midday[1:12], midday[1:12]+NDAYYEAR)
  monthly_value2 <- rep(monthly_value, 2)
  
  # Interpolate monthly to daily values
  daily_value <- list(x = NULL, y = NULL)
  for (i in 1:length(monthly_value2[-1])) {
    m1 <- i
    m2 <- i+1
    value <- approx(x = c(midday2[m1:m2]),
                    y = monthly_value2[m1:m2],
                    method = "linear",
                    n = midday2[m2]-midday2[m1]+1)
    daily_value[["x"]] <- c(daily_value[["x"]], value[["x"]])
    daily_value[["y"]] <- c(daily_value[["y"]], value[["y"]])
  }
  
  # Find days when value above threshold
  value_above <- daily_value$y >= threshold
  value_cross_threshold <- value_above-c(value_above[length(value_above)], value_above[1:(length(value_above)-1)])
  
    # plot(midday2, monthly_value2)
    # abline(h = threshold)
    # points(daily_value$x, daily_value$y)
    # points(daily_value$x[value_above], daily_value$y[value_above], col = "red")
    
  # Find days when value crosses threshold
  day_cross_up <- daily_value[["x"]][which(value_cross_threshold==1)]
  day_cross_down <- daily_value[["x"]][which(value_cross_threshold==-1)]
    
  # Convert values to 1:365 
  day_cross_up[day_cross_up>NDAYYEAR]     <- day_cross_up[day_cross_up>NDAYYEAR]-NDAYYEAR
  day_cross_down[day_cross_down>NDAYYEAR] <- day_cross_down[day_cross_down>NDAYYEAR]-NDAYYEAR
  day_cross_up <- sort(unique(day_cross_up))[1]
  day_cross_down <- sort(unique(day_cross_down))[1]
  
  if ( is.na(day_cross_up)  ==TRUE ) day_cross_up   <- -9999
  if ( is.na(day_cross_down)==TRUE ) day_cross_down <- -9999

  return(list("doy_cross_up" = day_cross_up, "doy_cross_down" = day_cross_down))
}

# Find first month of four wettest months (sum of their precipitation/PET ratios)
calcDoyWetMonth <- function(monthly_value) {
  # Middle day of each month
  midday <- c(15,43,74,104,135,165,196,227,257,288,318,349) #midday[NMONTH+1]
  NDAYYEAR <- 365
  
  # Replicate values twice (two years)
  midday2 <- c(midday[1:12], midday[1:12]+NDAYYEAR)
  monthly_value2 <- rep(monthly_value, 2)
  
  x <- NULL
  for (i in 0:11) {
    x <- c(x, sum(monthly_value2[c(1:4)+i]))
  }
  
  return(midday[which(x==max(x))[1]])
}

# Calculate variation coefficient
calcVarCoeff <- function(x) {
  ifelse(mean(x, na.rm = T) == 0, 0, sd(x, na.rm = T)/mean(x, na.rm = T))
}

# Calculate seasonality type (Waha et al., 2012)
calcSeasonality <- function(monthly_temp, monthly_prec, temp_min = TEMPMIN) {
  
  var_coeff_prec <- calcVarCoeff(monthly_prec)
  var_coeff_temp <- calcVarCoeff(deg2k(monthly_temp))
  min_temp <- min(monthly_temp)
  
  if      (var_coeff_prec<=0.4 && var_coeff_temp<=0.010) { seasonality <- "NO_SEASONALITY" }
  
  else if (var_coeff_prec>0.4 && var_coeff_temp<=0.010)  { seasonality <- "PREC" }
  
  else if (var_coeff_prec>0.4 && (var_coeff_temp>0.010 &&
                                  min_temp>temp_min))    { seasonality <- "PRECTEMP"}
  
  else if (var_coeff_prec>0.4 && (var_coeff_temp>0.010 &&
                                  min_temp<=temp_min))    { seasonality <- "TEMPPREC" }
  
  else {seasonality <- "TEMP"}
  
  return(seasonality)
}

# Calculate sowing date (Waha et al., 2012)
calcSowingDate <- function(croppar, monthly_temp, monthly_ppet, seasonality, lat) {
  
  # Middle day of each month
  midday <- c(15,43,74,104,135,165,196,227,257,288,318,349) #midday[NMONTH+1]
  
  # extract individual parameter names and values
  for(i in colnames(croppar)) {
    assign(i, croppar[[i]])
  }
  
  # Constrain first possible date for winter crop sowing
  earliest_sdate <- ifelse(lat>=0, initdate.sdatenh, initdate.sdatesh)
  earliest_smonth <- doy2month(earliest_sdate)
  DEFAULT_DOY    <- ifelse(lat>=0, 1, 182)
  DEFAULT_MONTH <- 0
  
  # First day of actual winter (for vernalizing crops)
  firstwinterdoy <- calcDoyCrossThreshold(monthly_temp, temp_fall)[["doy_cross_down"]]
  # First day of "warm winter" (for non-vernalizing winter-sown crops)
  if (min(monthly_temp>basetemp.low) &&
      (seasonality%in%c("TEMP", "TEMPREC", "PRECTEMP", "PREC"))) {
    # 2.5 months before coldest midday
      #(75 days seems a good approximation for both India and South US)
    coldestday <- midday[which(monthly_temp==min(monthly_temp))]
    firstwinterdoy <- ifelse(coldestday-75<=0, coldestday-75+365, coldestday-75)
  }
  firstwintermonth <- ifelse(firstwinterdoy==-9999, DEFAULT_MONTH, doy2month(firstwinterdoy))
  firstwinterdoy <- ifelse(firstwinterdoy==-9999, DEFAULT_DOY, firstwinterdoy)
  
  # First day of spring
  firstspringdoy <- calcDoyCrossThreshold(monthly_temp, temp_spring)[["doy_cross_up"]]
  firstspringmonth <- ifelse(firstspringdoy==-9999, DEFAULT_MONTH, doy2month(firstspringdoy))
  
  # If winter type
  if (calcmethod_sdate=="WTYP_CALC_SDATE") {
    if(firstwinterdoy > earliest_sdate & firstwintermonth!=DEFAULT_MONTH) {
      sowing_month <- firstwintermonth
      sowing_doy <- firstwinterdoy
      sowing_season <- "winter"
    } else {
      sowing_month <- firstspringmonth
      sowing_doy <- firstspringdoy
      sowing_season <- "spring"
    }
  } else {
    
    if (seasonality == "NO_SEASONALITY") {
      
      sowing_month <- DEFAULT_MONTH
      sowing_doy <- DEFAULT_DOY
      sowing_season <- "spring"
      
    } else if (seasonality=="PREC" || seasonality=="PRECTEMP") {
      
      sowing_doy <- calcDoyWetMonth(monthly_ppet)
      sowing_month <- doy2month(sowing_doy)
      sowing_season <- "spring"
      
    } else {
      
      sowing_month <- firstspringmonth
      sowing_doy <- firstspringdoy
      sowing_season <- "spring"
      
    }
  } # STYP_CALC_SDATE
  
  sowing_doy    <- ifelse(sowing_month == DEFAULT_MONTH, DEFAULT_DOY, sowing_doy)
  sowing_month  <- ifelse(calcmethod_sdate == "WTYP_CALC_SDATE" &
                            sowing_season == "spring", DEFAULT_MONTH, sowing_month)
  
  sd_vector <- list("sowing_month" = sowing_month,
                    "sowing_doy" = sowing_doy,
                    "sowing_season" = sowing_season)
  
  return(sd_vector)
}

# Calculate harvest rule (Minoli et al., 2019)
calcHarvestRule <- function(croppar, monthly_temp, monthly_ppet, seasonality) {
  
  # extract individual parameter names and values
  for(i in colnames(croppar)) {
    assign(i, croppar[[i]])
  }
  
  temp_max <- max(monthly_temp)
  temp_min <- min(monthly_temp)
  
  if (seasonality == "NO_SEASONALITY") {
    if (temp_max <= temp_base_rphase) harvest_rule <- 1
    else if (temp_max > temp_base_rphase & temp_max <= temp_opt_rphase) harvest_rule <- 4
    else harvest_rule <- 7
  }
  
  else if (seasonality == "PREC") {
    if (temp_max <= temp_base_rphase) harvest_rule <- 2
    else if (temp_max > temp_base_rphase & temp_max <= temp_opt_rphase) harvest_rule <- 5
    else harvest_rule <- 8
  }
  
  else {
    if (temp_max <= temp_base_rphase) harvest_rule <- 3
    else if (temp_max > temp_base_rphase & temp_max <= temp_opt_rphase) harvest_rule <- 6
    else harvest_rule <- 9
  }
  
  return(harvest_rule)
}

# Calculate vector of possible harvest dates (Minoli et al., 2019)
calcHarvestDateVector <- function(croppar,
                                  sowing_date,
                                  sowing_season,
                                  monthly_temp,
                                  monthly_ppet,
                                  monthly_ppet_diff) {
  
  # extract individual parameter names and values
  for(i in colnames(croppar)) {
    assign(i, croppar[[i]])
  }
  
  # Shortest cycle: Crop lower biological limit                       ----
  hd_first <- sowing_date+min_growingseason
  # Medium cycle: Best trade-off  vegetative and reproductive growth  ----
  hd_maxrp <- sowing_date+maxrp_growingseason
  # Longest cycle: Crop upper biological limit                        ----
  hd_last <- ifelse(sowing_season == "winter",
                    sowing_date+max_growingseason_wt,
                    sowing_date+max_growingseason_st)
  
  # End of wet season ----
  doy_wet1 <- calcDoyCrossThreshold(monthly_ppet,      ppet_ratio)[["doy_cross_down"]]
  doy_wet2 <- calcDoyCrossThreshold(monthly_ppet_diff, ppet_ratio_diff)[["doy_cross_down"]]
  doy_wet_vec <- ifelse(c(doy_wet1, doy_wet2) < sowing_date & c(doy_wet1, doy_wet2)!=-9999,
                        c(doy_wet1, doy_wet2)+NDAYYEAR,
                        c(doy_wet1, doy_wet2))
  # if more than one wet seasons take the first one else -9999
  doy_wet_first <- ifelse(length(doy_wet_vec[doy_wet_vec!=-9999]) > 0,
                          min(doy_wet_vec[doy_wet_vec!=-9999]),
                          -9999)
  
  #doy_wet <- ifelse(doy_wet_first > NDAYYEAR, doy_wet_first-NDAYYEAR, doy_wet_first)
  
  # if does not find harvest date and it is always high rainfall
  #if (doy_wet1==-9999 && min(monthly_ppet)>=ppet_min) {
  if (doy_wet1==-9999) {
    if (min(monthly_ppet)>=ppet_min) {
      hd_wetseas <- hd_last
    } else {
      hd_wetseas <- hd_first
      }
  } else { hd_wetseas <- doy_wet_first+rphase_duration }
  
  # Warmest day of the year ----
  #hd_temp_base <- midday[monthly_temp==max(monthly_temp)][1]+rphase_duration
  warmest_day <- midday[monthly_temp==max(monthly_temp)][1]
  hd_temp_base <- ifelse(sowing_season == "winter", warmest_day, warmest_day+rphase_duration)
  
  # First hot day ----
  doy_exceed_opt_rp <- calcDoyCrossThreshold(monthly_temp, temp_opt_rphase)[["doy_cross_up"]]
  idx <- which(doy_exceed_opt_rp < sowing_date & doy_exceed_opt_rp!=-9999)
  doy_exceed_opt_rp[idx] <- doy_exceed_opt_rp[idx]+NDAYYEAR
  doy_exceed_opt_rp <- sort(doy_exceed_opt_rp)[1]
  #doy_exceed_opt_rp <- ifelse(doy_exceed_opt_rp > NDAYYEAR, doy_exceed_opt_rp-NDAYYEAR, doy_exceed_opt_rp)
  
  # Last hot day ----
  doy_below_opt_rp <- calcDoyCrossThreshold(monthly_temp, temp_opt_rphase)[["doy_cross_down"]]
  idx <- doy_below_opt_rp < sowing_date & doy_below_opt_rp!=-9999
  doy_below_opt_rp[idx] <- doy_below_opt_rp[idx]+NDAYYEAR
  doy_below_opt_rp <- sort(doy_below_opt_rp)[1]
  #doy_below_opt_rp <- ifelse(doy_below_opt_rp > NDAYYEAR, doy_below_opt_rp-NDAYYEAR, doy_below_opt_rp)
  
  # Winter type: Firts hot day; Spring type: Last hot day 
  doy_opt_rp  <- ifelse(sowing_season == "winter", doy_exceed_opt_rp, doy_below_opt_rp)
  if (doy_opt_rp==-9999) {
    hd_temp_opt <- hd_maxrp
  } else { hd_temp_opt <- ifelse(sowing_season == "winter", doy_opt_rp, doy_opt_rp+rphase_duration) }
  
  #if hd < sowing date, it occurs the following year, so add 365 days
  hd_wetseas    <- ifelse(hd_wetseas < sowing_date, hd_wetseas+NDAYYEAR, hd_wetseas)
  hd_temp_base  <- ifelse(hd_temp_base < sowing_date, hd_temp_base+NDAYYEAR, hd_temp_base)
  hd_temp_opt   <- ifelse(hd_temp_opt < sowing_date, hd_temp_opt+NDAYYEAR, hd_temp_opt)
  
  # hd_vector ----
  hd_vector <- c(hd_first, hd_maxrp, hd_last, hd_wetseas, hd_temp_base, hd_temp_opt)
  names(hd_vector) <- c("hd_first", "hd_maxrp", "hd_last", "hd_wetseas", "hd_temp_base", "hd_temp_opt")
  
  return(hd_vector)
}

# Calculate harvest date (Minoli et al., 2019)
calcHarvestDate <- function(croppar,
                            monthly_temp,
                            sowing_date,
                            sowing_month,
                            sowing_season,
                            seasonality,
                            harvest_rule,
                            hd_vector) {
  
  # Extract individual parameter names and values
  for (i in colnames(croppar)) {
    assign(i, croppar[[i]])
  }
  
  # Extract individual individual values from hd_vector
  for (i in names(hd_vector)) {
    assign(i, unname(hd_vector[i]))
  }
  
  hd_vector_rf <- hd_vector_ir <- rep(NA, 6)
  names(hd_vector_rf) <- names(hd_vector_ir) <- c("hd_first", "hd_maxrp", "hd_last",
                                                 "hd_wetseas", "hd_temp_base", "hd_temp_opt")
  
  if (seasonality == "NO_SEASONALITY") {
    
    #hd_vector <- c(hd_first, hd_maxrp, NA, NA, NA, NA)
    hd_vector_rf[c("hd_first", "hd_maxrp")] <- hd_vector[c("hd_first", "hd_maxrp")]
    hd_vector_ir[c("hd_first", "hd_maxrp")] <- hd_vector[c("hd_first", "hd_maxrp")]
    
    if (harvest_rule==1) {
      # If the temperature never reaches the base temperature, ----
      #  the harvest date is set as early as possible (60 d after sowing).
      #  This is a rule to ensure functionality at the global scale
      #  and allow cropping also in environments where crops cannot be grown
      #  The fastest maturing cultivars are choosen
      # ----
      hd_rf <- hd_first
      hd_ir <- hd_first
      
    } else { #harvest_rule[cft]==4 | harvest_rule[cft]==7
      # If the temperature exceeds the base temperature, ----
      #  middle maturing cultivars are choosen,
      #  The harvest date is set 120 d after sowing,
      #  minimum total growing period needed for attaining the maximum "fruit development phase"
      #  to exploit as much as possible the favorable conditions,
      #  for capturing the available solar radiation in grain yield
      # ----
      hd_rf <- hd_maxrp
      hd_ir <- hd_maxrp
      
    }
    # return which harvest reason was choosen
    harvest_reason_rf <- which(hd_vector==hd_rf)[1]
    harvest_reason_ir <- which(hd_vector==hd_ir)[1]
    
  } else if (seasonality == "PREC") {
    
    # hd_vector_rf <- c(hd_first, hd_maxrp, NA, hd_wetseas, NA, NA)
    # hd_vector_ir <- c(hd_first, hd_maxrp, NA, NA, NA, NA)
    hd_vector_rf[c("hd_first", "hd_maxrp", "hd_wetseas")] <- hd_vector[c("hd_first", "hd_maxrp", "hd_wetseas")]
    hd_vector_ir[c("hd_first", "hd_maxrp")] <- hd_vector[c("hd_first", "hd_maxrp")]
    
    if (harvest_rule==2) {
      hd_rf <- hd_first
      hd_ir <- hd_first
    } else { #harvest_rule==5 | harvest_rule==8)
      hd_rf <- min(max(hd_first, hd_wetseas), hd_maxrp)
      hd_ir <- hd_maxrp
    }
    
    # return which harvest reason was choosen
    harvest_reason_rf <- which(hd_vector_rf==hd_rf)[1]
    harvest_reason_ir <- which(hd_vector_ir==hd_ir)[1]
    
  } else {
    
    if (sowing_season == "winter") {
      
      # hd_vector <- c(hd_first, NA, hd_last, NA, hd_temp_base, hd_temp_opt)
      hd_vector_rf[c("hd_first", "hd_last", "hd_temp_base", "hd_temp_opt")] <- hd_vector[c("hd_first", "hd_last", "hd_temp_base", "hd_temp_opt")]
      hd_vector_ir[c("hd_first", "hd_last", "hd_temp_base", "hd_temp_opt")] <- hd_vector[c("hd_first", "hd_last", "hd_temp_base", "hd_temp_opt")]
      
      if (harvest_rule==3) {
        hd_rf <- hd_first
        hd_ir <- hd_first
      } else if (harvest_rule==6) {
        if (sowing_month==0 && max(monthly_temp) < temp_fall) {
          hd_rf <- hd_first
          hd_ir <- hd_first
        } else {
          hd_rf <- min(max(hd_first, hd_temp_base), hd_last)
          hd_ir <- min(max(hd_first, hd_temp_base), hd_last)
        }
      } else { #harvest_rule==9
        hd_rf <- min(max(hd_first, hd_temp_opt), hd_last)
        hd_ir <- min(max(hd_first, hd_temp_opt), hd_last)
      }
      
      # return which harvest reason was choosen
      harvest_reason_rf <- which(hd_vector_rf==hd_rf)[1]
      harvest_reason_ir <- which(hd_vector_ir==hd_ir)[1]
      
    } else {
      
      # hd_vector <- c(hd_first, NA, hd_last, NA, hd_temp_base, hd_temp_opt)
      hd_vector_rf[c("hd_first", "hd_last", "hd_wetseas", "hd_temp_base", "hd_temp_opt")] <- hd_vector[c("hd_first", "hd_last",  "hd_wetseas", "hd_temp_base", "hd_temp_opt")]
      hd_vector_ir[c("hd_first", "hd_last", "hd_temp_base", "hd_temp_opt")] <- hd_vector[c("hd_first", "hd_last", "hd_temp_base", "hd_temp_opt")]
      
      if (harvest_rule==3) {
        hd_rf <- hd_first
        hd_ir <- hd_first
      } else if (harvest_rule==6){
        if (sowing_month==0 && max(monthly_temp) < temp_spring) {
          hd_rf <- hd_first
          hd_ir <- hd_first
        } else if (seasonality=="PRECTEMP") { # T not stressful, only water limitation applys
          hd_rf <- min(max(hd_first, hd_wetseas), hd_last)
          hd_ir <- hd_last
        } else {
          hd_rf <- min(max(hd_first, hd_temp_base), max(hd_first, hd_wetseas), hd_last)
          hd_ir <- min(max(hd_first, hd_temp_base), hd_last)
        }
      } else { #harvest_rule[cft]==9
        hd_rf <- min(max(hd_first, hd_temp_opt), max(hd_first, hd_wetseas), hd_last)
        hd_ir <- min(max(hd_first, hd_temp_opt), hd_last)
      }
      
      # return which harvest reason was choosen
      harvest_reason_rf <- which(hd_vector_rf==hd_rf)[1]
      harvest_reason_ir <- which(hd_vector_ir==hd_ir)[1]
      
    } # spring type
    
  } # MIX_SEAS
  
  #hd_rf <- ifelse(hd_rf < sowing_date, hd_rf+NDAYYEAR, hd_rf)
  #hd_ir <- ifelse(hd_ir < sowing_date, hd_rf+NDAYYEAR, hd_ir)

  hd_rf <- ifelse(hd_rf > NDAYYEAR, hd_rf-NDAYYEAR, hd_rf)
  hd_ir <- ifelse(hd_ir > NDAYYEAR, hd_ir-NDAYYEAR, hd_ir)
  
  hd <- list("hd_rf" = hd_rf,
             "hd_ir" = hd_ir,
             "harvest_reason_rf" = harvest_reason_rf,
             "harvest_reason_ir" = harvest_reason_ir)
  return(hd)
  
}

# Calculate growing-period duration
computeGrowingPeriod <- function(sowing, harvest, max_time_unit) { #max_time_unit (e.g. 365 (days), 12 (month))
  growperiod <- ifelse(sowing<=harvest, harvest-sowing, max_time_unit+harvest-sowing)
  return(growperiod)
}

