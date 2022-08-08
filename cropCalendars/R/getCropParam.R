#' @title Import crop parameter table
#'
#' @export

getCropParam <- function(cropparam_file,
                         crops,
                         print_all = FALSE
                         ) {
  # Parameters ----
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
