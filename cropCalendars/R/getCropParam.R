#' @title Import crop parameter table
#'

getCropParam <- function(cropparam_file,
                         crops) {
  # Parameters ----
  cat("Importing parameter table for all crops ...",
      "-------------------------------------------",
      sep = "\n")
  crop_parameters_all <- read.csv(cropparam_file, header = T, stringsAsFactors = F)
  crop_parameters_all <- subset(crop_parameters_all, crop_name%in%crops)
  print(crop_parameters_all[,c("crop_id", "crop_name")])

  return(crop_parameters_all)
}
