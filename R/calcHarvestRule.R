#' @title Calculate harvest rule (Minoli et al., 2019)
#'
#' @export
calcHarvestRule <- function(croppar,
                            monthly_temp,
                            monthly_ppet,
                            seasonality
                            ) {

  # extract individual parameter names and values
  for(i in colnames(croppar)) {
    assign(i, croppar[[i]])
  }

  temp_max <- max(monthly_temp)
  temp_min <- min(monthly_temp)

  if (seasonality == "NO_SEASONALITY") {
    if (temp_max <= temp_base_rphase) {
      harvest_rule <- 1
      names(harvest_rule) <- "t-low_no-seas"
    } else if (temp_max > temp_base_rphase & temp_max <= temp_opt_rphase) {
      harvest_rule <- 4
      names(harvest_rule) <- "t-mid_no-seas"
    } else {
      harvest_rule <- 7
      names(harvest_rule) <- "t-high_no-seas"
    }
  }

  else if (seasonality == "PREC") {
    if (temp_max <= temp_base_rphase) {
      harvest_rule <- 2
      names(harvest_rule) <- "t-low_prec-seas"
    } else if (temp_max > temp_base_rphase & temp_max <= temp_opt_rphase) {
      harvest_rule <- 5
      names(harvest_rule) <- "t-mid_prec-seas"
    } else {
      harvest_rule <- 8
      names(harvest_rule) <- "t-high_prec-seas"
    }
  }

  else {
    if (temp_max <= temp_base_rphase) {
      harvest_rule <- 3
      names(harvest_rule) <- "t-low_mix-seas"
    } else if (temp_max > temp_base_rphase & temp_max <= temp_opt_rphase) {
      harvest_rule <- 6
      names(harvest_rule) <- "t-mid_mix-seas"
    } else {
      harvest_rule <- 9
      names(harvest_rule) <- "t-high_mix-seas"
    }
  }

  return(harvest_rule)
}
