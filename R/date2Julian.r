#' Function to convert dates to Julian days
#'@author Martin Jung
#'@param A date object
#'\code{FALSE}
#'@export
 
date2Julian <- function(dat) {
  #converts date to julian day, so it can be easily plotted
  for (row in 1:nrow(dat)){
    line <- dat[row,]
    dat$julian[row] <- julian(line$Month, line$Day, line$Year,
                              origin. <- c(month=1, day=1, year=line$Year))
  }
  return(dat$julian)
}
