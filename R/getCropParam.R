#' @title Import crop parameter table
#'
#' @param crops Character vector of crop names. Names should correspond to
#' those in the crop_name column of the cropparam_file. If "all", returns all
#' crops present in the table.
#' @param cropparam_file CSV file containing a table with the crop parameters
#' necessary for calculating climate-driven crop calendars. If NULL, the default
#' file ./inst/extdata/crop_parameters.csv is read.
#' @param verbose If TRUE it prints the parameters.
#'
#' @return Crop parameters as a data.frame.
#'
#' @export

getCropParam <- function(crops          = "all",
                         cropparam_file = NULL,
                         verbose        = FALSE
                         ) {
  # If not specified, read default parameter file
  if (is.null(cropparam_file)) {
    cropparam_file <- system.file("extdata", "crop_parameters.csv",
                                package = "cropCalendars", "mustWork" = TRUE)
  }
  # Import crop parameters
  crop_parameters_all <- read.csv(cropparam_file,
                                  header = T,
                                  stringsAsFactors = F)
  if (crops != "all") {
    crop_parameters_all <- subset(crop_parameters_all,
                                  crop_name %in% crops)
  }
  if (verbose) {
    cat("Importing crop-parameter table ...",
        "----------------------------------",
        sep = "\n")
    print(t(crop_parameters_all))
  }
  return(crop_parameters_all)
}
