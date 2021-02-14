# Melt matrix or array to data.table

# https://raw.githubusercontent.com/rstudio/cheatsheets/master/datatable.pdf
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/

array2dataTable <- function(x) {
  
  # if is matrix, add third dimension (as.data.table does not melt matrices)
  x.is.matrix <- FALSE
  if (length(dim(x))==2) {
    x.is.matrix <- TRUE
    cat("\nNote: x is a matrix, converting it to array with 3rd dim==3 ..")
    dimns <- list(dimnames(x)[[1]], dimnames(x)[[2]], "1")
    dim(x) <- c(dim(x), 1L)
    dimnames(x) <- dimns
  }
  # Add dimnames
  if (is.null(dimnames(x))) {
    cat("\nNote: Array has no dimnames, using seq of integers ..\n")
    dimnames(x) <- lapply(dim(x), function(X) as.character(seq.int(1, X)))
  }
  # Melt to DT
  DT <- as.data.table(x,  na.rm = FALSE, keep.rownames = T)
  cnames <- colnames(DT)[colnames(DT)!="value"]
  # Convert dimensions columns to numeric
  DTout <- DT[, lapply(.SD, as.numeric), by=value, .SDcols=cnames]
  setcolorder(DTout, c(cnames, "value"))
  setorderv(DTout, cnames)
  # Remove 3rd column if is 2D matrix
  if (x.is.matrix==TRUE) DTout[,V3:=NULL]
  #print(str(DTout))
  return(DTout)
}




# AR1 <- array(rnorm(24), dim=c(3,4,2))
# DT <- array2dataTable(AR1)
# AR2 <- array(rnorm(12), dim = c(3,4))
# DT <- array2dataTable(AR2)

# Melt list of array into DT
arrayList2dataTable <- function(x) {
  x.ls <- list()
  for (i in 1:length(x)) {
    DT <- array2dataTable(x[[i]])
    x.ls[[i]] <- cbind(DT, list.element = i)
  }
  return(rbindlist(x.ls))
}

# AR2 <- array(rnorm(12), dim = c(3,4))
# LS <- list(AR2, AR2)
# DT <- list2dataTable(LS)
