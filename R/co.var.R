#' Calculates the coeffiecient of variation
#' 
#' Calculate the CV of a given vector
#' @param x A vector
#' @keywords subset
#' @export
#' @usage co.var(...)
#' @author Martin Jung

co.var <- function(x,na.rm=T) {
  if(is.vector(x)){  
    cv = (100*sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm) )
    } else {
    stop("Input is not a vector")
  }
  return(cv)
}
