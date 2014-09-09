#' Show difference between two vectors
#' 
#' returns a list of the elements of x that are not in y 
#' and the elements of y that are not in x (not the same thing...)
#' @param x vector x
#' @param y vector y
#' @keywords difference
#' @export
#' @usage setdiff2(x,y)
#' @author Martin Jung



setdiff2 <- function(x,y) {
  Xdiff = setdiff(x,y)
  Ydiff = setdiff(y,x)
  list(X_not_in_Y=Xdiff, Y_not_in_X=Ydiff)
}