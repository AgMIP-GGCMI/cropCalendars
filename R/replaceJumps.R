#' @title Replace jumps in a time series
#'
#' @description Function to find jumps in seasonality/harv.rule and remove them
#' from time series of sowing or harvest dates.
#' If marking = T, boolean to mark which values are replaced:
#' 0 = original, 1 = replaced.

replaceJumps <- function(vec.class        = rep(1:4, each = 20),
                         vec.date         = rep(1:8, each = 10),
                         replacing.value  =              "mean",
                         exclue.last.jump =                TRUE,
                         min.period       =                  20,
                         marking          =               FALSE,
                         mark.value       =                  1L) {

  # vec.class: vector of seasonality or harvest reason
  # vec.date:  vector of sowing or harvest dates

  vec.date.out    <- vec.date
  mark.replaced   <- vec.date
  mark.replaced[] <- 0L

  # When is there a change in vec.class?
  # Exclude last change in vector class, as we can't know if this is going
  #  to be temporary or continue in the future, where we do not have data
  #  Note: +1 because diff gives index previous value
  if (exclue.last.jump == TRUE) {
    x  <- c(which(diff(vec.class) != 0) + 1)
  } else {
    x  <- c(which(diff(vec.class) != 0) + 1, length(vec.class))
  }

  # Which class changes are temporary (jumps), e.g. less 20 y?
  x1s <- x[which(diff(x) <= min.period)    ]       # start of temporary change
  x1e <- x[which(diff(x) <= min.period) + 1]       # end of temporary change

  # Which temporary class changes return to previous class?
  x2s <- x1s[vec.date[x1s-1] - vec.date[x1e] == 0] # start
  x2e <- x1e[vec.date[x1s-1] - vec.date[x1e] == 0] # end

  # If tmp changes exist, replace with mean value of neighboring time periods
  if (length(x2s)>0) {

    # loop through actual jumps only (odd indices), skip returns to "normal" value
    # note: x%%2==1 finds odd numbers
    for (i in c( which( (1:(length(x2s)-1))%%2 == 1), length(x2s) ) ) {

      vprev <- vec.date.out[x2s[i]-1] # value of previous time slice
      vnext <- vec.date.out[x2e[i]+1] # value next time slice

      if (replacing.value        == "mean"    ) {

        # Use average value of neighbor periods
        vec.date.out[x2s[i]:(x2e[i]-1)] <- round(mean(c(vprev, vnext), na.rm = T))

      } else if (replacing.value == "previous") {

        # Use value of previous neighbor periods (e.g. if categorical variable)
        vec.date.out[x2s[i]:(x2e[i]-1)] <- vprev

      } else if (replacing.value == "next"    ) {

        # Use value of next neighbor periods  (e.g. if categorical variable)
        vec.date.out[x2s[i]:(x2e[i]-1)] <- vnext

      } else {

        stop("Error in replace.jumps(): undefined replacing.value.")

      }

      # Mark value to flag values that have been replaced
      mark.replaced[x2s[i]:(x2e[i]-1)] <- mark.value

      if (i == length(x2s)) {

        vec.date.out[x2e[i]]  <- vec.date.out[(x2e[i]-1)]  # replace last value
        mark.replaced[x2e[i]] <- mark.replaced[(x2e[i]-1)] # replace last value

      } # if i

    } # for i

  } # if (length(x2s)>0)


  if (!marking) return(vec.date.out) else return(mark.replaced)
}


# ------------------------------------------------------#
# E.g.

# vec.date.in <- c(rep(100, 10), rep(200, 10), rep(250, 10), rep(300, 40),
#                  rep(350, 10), rep(300, 10), rep(200, 20), rep(100, 10))
# vec.class.in <- c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 40),
#                   rep(5, 10), rep(4, 10), rep(3, 20), rep(2, 10))
# vec.date.out <- replace.jumps(vec.class.in, vec.date.in)
#
# plot(vec.date.in, type = "l", ylim = c(0, 500))
# lines(vec.class.in*10, col = "blue")
# lines(vec.date.out, col = "red")
#
#
# vec.date  <- vec.date.in
# vec.class <- vec.class.in
