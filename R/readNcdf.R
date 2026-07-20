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
#' @param index_dims (Optional) Character vector of dimension names whose
#' \code{dim_subset} entries are 1-based POSITIONS (indices) rather than coordinate
#' values. Use for axes whose stored coordinates are not 0-based -- e.g. a time axis
#' with a 0.5 mid-day offset or a fixed epoch -- where positional selection is what is
#' meant. Default \code{character(0)} = all dims matched by coordinate value.
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
                     verbose    = FALSE,
                     index_dims = character(0)
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
    # (subsetting is the normal path — no warning; the entire-dataset read above
    # keeps its warning since that one can be an accidental large read)

    # Get dimensions subset
    idim_list <- dim_list_sub <- list()
    # Loop through the dimensions
    for (i in seq_len(length(dims))) {
      # Which indices should be extracted for dims[i]?
      dname              <- dims[i]
      if (dname %in% index_dims && !is.null(dim_subset[[dname]])) {
        # Subset by 1-based POSITION, not coordinate value. ISIMIP climate files use
        # inconsistent time-axis conventions across models (0-based integer days,
        # 0.5 mid-day offset, or a fixed epoch such as 'days since 1860'), but every
        # file's daily slices are positional and on the same Gregorian calendar, so
        # the caller passes day positions and we select those slices directly.
        idim_list[[dname]] <- as.integer(dim_subset[[dname]])
        n <- length(dim_list[[dname]])
        if (any(idim_list[[dname]] < 1L | idim_list[[dname]] > n))
          stop("readNcdf: index_dims position out of range for '", dname, "' (1..", n, ").")
      } else {
        idim_list[[dname]] <- which(dim_list[[dname]] %in% dim_subset[[dname]])
        if (length(idim_list[[dname]]) == 0) {
          if (!is.null(dim_subset[[dname]])) {
            # Requested values exist but match NONE of this dimension's coordinate
            # values. Reading the whole dimension here would silently return the
            # wrong slice, so fail loudly instead. (For a time axis whose values are
            # not 0-based, pass index_dims = "time" and 1-based positions.)
            stop("readNcdf: requested values for dimension '", dname,
                 "' match none of its coordinate values (range [",
                 min(dim_list[[dname]]), ", ", max(dim_list[[dname]]),
                 "]). Check that the subset uses coordinate values, not indices ",
                 "(or pass index_dims for positional subsetting).")
          }
          # Dimension not specified in dim_subset: read it entirely.
          idim_list[[dname]] <- seq_len(length(dim_list[[dname]]))
        }
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
