#' Removes all rows with NA out of input 
#' 
#' Makes a subset of the input
#' @param x Input R object
#' @keywords subset
#' @export
#' @usage remNa(yourDatafram)
#' @author Martin Jung

# Alle NA-Werte aus Liste entfernen
remNa <- function(x) {
  return(subset(x,complete.cases(x)))
}
