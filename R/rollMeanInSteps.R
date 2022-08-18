#' @title Step-wise rolling mean
#'
#' @description Apply step-wise rolling mean on vec.date, within windows
#' defined by vec.class.
#' Remove fluctuations around December and January.

rollMeanInSteps <- function(vec.class,
                            vec.date,
                            kk      = 10,
                            marking = F
) {

  # vec.class: vector of seasonality or harvest reason
  # vec.date:  vector of sowing or harvest dates
  # kk: years for rollmean (k parameter)

  require(zoo) # for rolling mean

  vec.date.out    <- vec.date
  mark.replaced   <- vec.date
  mark.replaced[] <- 0L

  # indices of start, end and change in vec.class
  x  <- c(1, which(diff(vec.class) != 0) + 1, length(vec.class) + 1 )

  for (i in 1:(length(x) - 1) ) {
    # i <- 1

    iyears <- x[i]:(x[i + 1] - 1) # indices of years to roll-average

    # If dates fluctuate around Dec. and Jan (range > 11 months), set it to Jan
    if (abs(diff(range(vec.date[iyears]))) > 330) {

      vec.date.out[iyears] <- min(vec.date[iyears])

    # Else apply rolling mean
    } else {

      vec.date.out[iyears] <- round(rollapply(vec.date[iyears],
                                              width   = kk,
                                              FUN     = mean,
                                              partial = T))

    }

    mark.replaced[iyears] <- i

  } # i

  if (!marking) return(vec.date.out) else return(mark.replaced)
}

# Example ----
# ------------------------------------------------------#
# vc <- c(rep(1,20), rep(2,30), rep(3,70))
# #vd <- round(runif(120, 1,365))
# #vd <- round(c(runif(20, 1,60), runif(30,60,200), runif(70,200,365)))
# vd <- c(rep(60,20), rep(300,30), rep(225,70))
# vr <- rollmean.in.steps(vc, vd, 10)
# vm <- rollmean.in.steps(vc, vd, 10, marking = T)
#
# plot(vd, type = "l", ylim = c(0, 365))
# lines(vr, type = "l", col = "red")
# lines(vc*10, type = "l", col = "blue")
# lines(vm*10, type = "l", col = "orange")

#
# # Testing
# vec.class = vc
# vec.date = vd
# kk = 10
# marking = T
