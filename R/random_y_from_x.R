#' Samples a number of values from y
#' 
#' Makes a subset of the input
#' @param x Input R vector
#' @param y Number of samples
#' @keywords subset
#' @export
#' @usage random_y_from_x(trees$Volume,10)
#' @author Martin Jung

random_y_from_x <- function(x,y) {
  sample(x, y, replace=F)
}
