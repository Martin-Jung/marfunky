#' Interface to the Matrix in R
#' 
#' Possible use? Non. But 100% awesomeness ;)
#' @param x How many x points
#' @param y How many y numbers
#' @keywords 1337
#' @export
#' @usage TheMatrix()
#' @author Anonymous


TheMatrix <- function(x_num=100,y_num=80){
  # get maximum number of x and y points
  
  # x-axis locations in random order
  x = sample(x=1:x_num, size=90, replace=TRUE)
  # y-axis locations (from -1 to -80)
  y = seq(-1, -y_num, length=90)
  
  # set graphical parameters
  op = par(bg="black", mar=c(0, 0.2, 0, 0.2))
  
  # plotting window
  plot(1:x_num, seq(-1,-x_num), type="n",
       xlim = c(1,x_num), ylim = c(-y_num+10,0))
  # plot letters
  for (i in seq_along(x))
  {
    # sample to get vertical length
    aux = sample(1:y_num, 1)
    # x and y coordinates
    x_coords = rep(x[i],aux)
    y_coords  = y[1:aux]
    # add characters of different size and hues
    points(x_coords, y_coords,
           pch = sample(c(letters, toupper(letters)), aux, replace=TRUE),
           col = hsv(0.35, runif(aux,0.25,0.9), 1, runif(aux,.3)),
           cex = runif(aux,.3))
  }
  
  # reset graphical parameters
  par(op)
}
