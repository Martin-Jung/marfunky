#' Normalize a numeric vector
#'
#' Normalizes an input numeric vector to a proportional scale (0 - 1) 
#'
#' @author Martin Jung 
#' @param x Input numeric vector
#'
#' @examples
#'  raw <- vector(2,5,2,6,11,6,4,3)
#'
#'  normalize(raw)               
#'
#' @return returns a normalized vector
#' @export
normalize <- function(x) {  
  # Only one value
  if(length(x)== 1) return(1)
  y <- suppressWarnings( (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE)) )
  # All proportions equal
  if(any(is.nan(y)))return(x) else return(y) 
}