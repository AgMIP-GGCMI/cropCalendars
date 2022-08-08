#' @title Import crop parameter table
#'
#' @export

getCropParam <- function(crops          = NULL,
                         cropparam_file = NULL,
                         print_all      = FALSE
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
  crop_parameters_all <- subset(crop_parameters_all,
                                crop_name %in% crops)
  if (print_all) {
    cat("Importing crop-parameter table ...",
        "----------------------------------",
        sep = "\n")
    print(t(crop_parameters_all))
  } else {
    cat("Importing crop-parameter table for ---> ",
        crop_parameters_all$crop_name,
        sep = " ")
  }

  return(crop_parameters_all)
}
