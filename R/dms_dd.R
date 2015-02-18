#' Convert degrees minutes seconds to decimal degrees:
#'@author Andy Teucher
#'@param x: a vector containing the lat or long with elements separated by single character
#'@param sep: the character separating the degrees, minutes, seconds (default ":")
#'@param hem: the hemisphere ("N","S","E","W"). Assumes all coords in the same hemisphere
#'\code{FALSE}
#'@export

dms_dd <- function(x, sep=":", hem) {
  if (hem %in% c("N","S","E","W")) {
    x <- lapply(strsplit(x,sep), as.numeric)
    x <- unlist(lapply(x, function(y) (y[1]+y[2]/60+y[3]/3600)))
    ifelse(hem %in% c("N","E"),
           ifelse(x>0,mult <- 1, mult <- -1),
           ifelse(x<0,mult <- 1, mult <- -1))
    x <- x*mult
    x
  } else {
    print("Error: 'hem' must be N,S,E, or W")
  }
}