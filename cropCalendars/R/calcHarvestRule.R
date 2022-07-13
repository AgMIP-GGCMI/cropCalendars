#' @title Calculate harvest rule (Minoli et al., 2019)
#'
#' @export
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
