#' Calculates the Standard error of the mean
#' 
#' Calculates the standarderror of the mean
#' @param values Input numerical vector
#' @keywords standard-error
#' @export
#' @usage stderr(x)
#' @author Martin Jung

stderr <- function(x) {
  sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
}