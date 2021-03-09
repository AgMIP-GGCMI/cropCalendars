# Function to find jumps in seasonality/harv.rule and remove them from
# time series of sowing or harvest dates.
# If marking = T, boolean to mark which values are replaced: 0 = original, 1 = replaced

replace.jumps <- function(vec.class, vec.date, min.period = 20, marking = F) {
  # vec.class: vector of seasonality or harvest reason
  # vec.date:  vector of sowing or harvest dates
  vec.date.out <- vec.date
  mark.replaced <- vec.date; mark.replaced[] <- 0L
  
  # when is there a change in vec.class?
  x  <- which(diff(vec.class)!=0)+1 # +1 bcz diff gives index previous value
  # which changes are temporary (jumps), e.g. less 20 y?
  y1 <- x[which(diff(x)<=min.period)]   # start of temporary change
  y2 <- x[which(diff(x)<=min.period)+1] # end of temporary change
  
  # if tmp changes exist, replace with mean value of neighboring time periods
  if (length(y1)>0)
    # loop through actual jumps only (odd indices), skip returns to "normal" value
    for (i in which( (1:length(y1))%%2==1 ) ) { # x%%2==1 finds odd nr.
      vec.date.out[y1[i]:(y2[i]-1)] <- round(mean(c(vec.date.out[y1[i]-1],
                                                    vec.date.out[y2[i]+1])))
      mark.replaced[y1[i]:(y2[i]-1)] <- 1L
    }
  if (!marking) return(vec.date.out) else return(mark.replaced)
}
