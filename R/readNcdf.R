#' @title Read ncdf file
#'
#' @description Read ncfd file with ncfd4 package.
#'
#' @param file_name File name of ncdf file with path.
#' @param dim_subset (Optional) List of dimensions to be subset, typically
#' lon, lat and time. The list's element names should correspond
#' to the names of the dimensions as defined in the file. If unknown,
#' use verbose = TRUE to print them out. If a dimension is note specified, it is
#' read entirely.
#' @param var_name (Optional) Variable name (e.g. "tas"). Needed if the file
#' contains more than one variable (e.g. temperature, precipitation).
#' @param verbose (Optional) If TRUE, it prints some useful meta-data
#' (e.g. dimension names).
#' @import ncdf4
#' @examples
#' x1 <- readNcdf(file_name = fn_tas,
#' dim_subset = list(lon = 90.25, lat = 45.25, time = 0:3652),
#' verbose = TRUE)
#' str(x1)
#'
#' x <- readNcdf(file_name = fn_tas)
#' str(x)
#'
#' @export


readNcdf <- function(file_name  = NULL,
                     dim_subset = NULL,
                     var_name   = NULL,
                     verbose    = FALSE
                     ) {

  # Open ncdf file
  nf <- nc_open(file_name)
  if (verbose) {
    print(nf)
  }

  # Get variable name
  if (length(nf$var) > 1 & is.null(var_name)) {
    stop("File contains more than one variable, specify var_name!")
  } else {
    var_name <- nf$var[[1]]$name
  }

  # Get dimension names and values
  dims     <- unlist(lapply(nf$var[[var_name]]$dim, `[[`, "name"))
  dim_list <- list()
  for (i in seq_len(length(dims))) {
    dname             <- dims[i]
    dvals             <- ncvar_get(nf, dims[i])
    dim_list[[dname]] <- dvals
  }
  if (verbose) {
    cat("\nvar_name: ", var_name, "\ndimensions: ", dims, "\n")
    print(
      lapply(
        dim_list,
        function(x) paste("min", min(x), "max", max(x), "length", length(x)))
    )
  }

  # Read data
  if (is.null(dim_subset)) {
    warning("Reading entire dataset! If file is large, consider subsetting.")

    nc           <- ncvar_get(nf, var_name)
    dimnames(nc) <- dim_list

  } else {
    warning("Reading a subset of data.")

    # Get dimensions subset
    idim_list <- dim_list_sub <- list()
    # Loop through the dimensions
    for (i in seq_len(length(dims))) {
      # Which indices should be extracted for dims[i]?
      dname              <- dims[i]
      idim_list[[dname]] <- which(dim_list[[dname]] %in% dim_subset[[dname]])
      if (length(idim_list[[dname]]) == 0) {
        # if dims[i] is not specified in dim_subset, read it entirely
        idim_list[[dname]] <- seq_len(length(dim_list[[dname]]))
      }
      dim_list_sub[[dname]] <- dim_list[[dname]][idim_list[[dname]]]
    }

    # Define start and count for ncvar_get
    st <- unlist(lapply(idim_list, min))
    ct <- unlist(lapply(idim_list, max)) - st + 1

    # Read data from ncdf and assign dimnames to array
    nc           <- ncvar_get(nf, var_name, start = st, count = ct)
    dim(nc)      <- ct
    dimnames(nc) <- dim_list_sub
  }
  return(nc)
}
