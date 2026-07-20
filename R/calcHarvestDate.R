#' @title Calculate harvest date (Minoli et al., 2019)
#'
#' @description Rule-based estimation of the end of the crop growing period
#' (date of physiological maturity), here called harvest date for simplicity.
#' The assumption behind these rules is that farmers select growing
#' seasons based on the mean climatic characteristics of the location in
#' which they operate and on the physiological limitations (base and optimum
#' temperatures for reproductive growth; sensitivity to terminal water stress) of
#' the respective crop species.
#'
#' @param croppar data.frame with crop parematers as returned by getCropParam
#' @param sowing_date numeric value as day of the year (DOY). This can be either
#' caculated with calcSowingDate or prescribed.
#' @param sowing_month numeric value between 0 and 12. 0 represents the "default
#' month" if no sowing date is found by calcSowingDate, 1:12 represent months
#' from January to December.
#' @param sowing_season character value. Can be either "winter" or "spring". See
#' calcSowingDate.
#' @param seasonality seasonality type as calculated by calcSeasonality
#' @param harvest_rule harvest rule as calculated by calcHarvestRule
#' @param hd_vector vector of possible harvest dates as calculated by
#' calcHarvestDateVector
#' @export

calcHarvestDate <- function(croppar,
                            sowing_date,
                            sowing_month,
                            sowing_season,
                            seasonality,
                            harvest_rule,
                            hd_vector
                            ) {

  # Extract individual parameter names and values
  list2env(croppar, environment())  # 1-row data.frame: columns -> scalar params

  ndays_year <- 365

  # NB: the "too cold to grow" guards below key on sowing_month==0 alone, consistent with the sowing
  # decision, so no warmest-window temperature is computed here (hence no monthly_temp / daily_temp /
  # smooth_window arguments in the signature).

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
        if (sowing_month==0) {
          # DEFAULT sowing = calcSowingDate found no real season (no sustained temp_fall/temp_spring
          # crossing) -> too cold/short to grow, harvest as early as possible. The guard keys on the
          # sowing verdict (sowing_month==0) alone rather than re-testing the warmest-window temperature
          # against temp_fall: the sowing decision already made that call, with a sustained daily
          # crossing, so re-testing it here with a bare window mean could disagree in the cold-marginal
          # band and flip the harvest hd_first<->hd_last at otherwise-stable sowing.
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
      hd_vector_rf[c("hd_first", "hd_maxrp", "hd_last", "hd_wetseas", "hd_temp_base", "hd_temp_opt")] <- hd_vector[c("hd_first", "hd_maxrp", "hd_last",  "hd_wetseas", "hd_temp_base", "hd_temp_opt")]
      hd_vector_ir[c("hd_first", "hd_maxrp", "hd_last", "hd_temp_base", "hd_temp_opt")] <- hd_vector[c("hd_first", "hd_maxrp", "hd_last", "hd_temp_base", "hd_temp_opt")]

      if (harvest_rule==3) {
        hd_rf <- hd_first
        hd_ir <- hd_first
      } else if (harvest_rule==6){
        if (sowing_month==0) {
          # DEFAULT sowing -> no real season found -> harvest as early as possible. Keyed on the sowing
          # verdict (sowing_month==0) alone rather than re-testing the warmest-window temperature against
          # temp_spring (the spring analogue of the winter-type guard above), for the same consistency
          # reason.
          hd_rf <- hd_first
          hd_ir <- hd_first
        } else if (seasonality=="PRECTEMP") { # T not stressful, only water limitation applies
          hd_rf <- min(max(hd_first, hd_wetseas), hd_maxrp)
          hd_ir <- hd_maxrp
        } else {
          hd_rf <- min(max(hd_first, hd_temp_base), max(hd_first, hd_wetseas), hd_last)
          hd_ir <- min(max(hd_first, hd_temp_base), hd_last)
        }
      } else { #harvest_rule[cft]==9
        if (seasonality=="PRECTEMP") { # T not stressful, only water limitation applies
          hd_rf <- min(max(hd_first, hd_wetseas), hd_maxrp)
          hd_ir <- hd_maxrp
        } else {
          hd_rf <- min(max(hd_first, hd_temp_opt), max(hd_first, hd_wetseas), hd_last)
          hd_ir <- min(max(hd_first, hd_temp_opt), hd_last)
        }
      }

      # return which harvest reason was choosen
      harvest_reason_rf <- which(hd_vector_rf==hd_rf)[1]
      harvest_reason_ir <- which(hd_vector_ir==hd_ir)[1]

    } # spring type

  } # MIX_SEAS

  #hd_rf <- ifelse(hd_rf < sowing_date, hd_rf+ndays_year, hd_rf)
  #hd_ir <- ifelse(hd_ir < sowing_date, hd_rf+ndays_year, hd_ir)

  hd_rf <- ifelse(hd_rf > ndays_year, hd_rf-ndays_year, hd_rf)
  hd_ir <- ifelse(hd_ir > ndays_year, hd_ir-ndays_year, hd_ir)

  hd <- list("hd_rf" = hd_rf,
             "hd_ir" = hd_ir,
             "harvest_reason_rf" = harvest_reason_rf,
             "harvest_reason_ir" = harvest_reason_ir)
  return(hd)

}
