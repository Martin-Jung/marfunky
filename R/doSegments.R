#' Add segments to a standard scatterplot
#' 
#' Uses the segments function to add segments to a point scatter plot.
#' @param x x coordinate of scatter points
#' @param y y coordinates of scatter points
#' @param ll line length of segments. For instance SD or the standard error
#' @param eps hat width
#' @keywords distribution
#' @export
#' @author Martin Jung
#' 
#' 
 
# Do segments on top of Plot.
# Requires a transmitted data.frame with x-y values and a vector with line-length
doSegments <- function(x,y,ll,eps=0.05,...){
  # Further Arguments are transmitted to segments
  segments(x,y-ll,x,y+ll,...) # Build lines
  segments(x-eps,y-ll,x+eps,y-ll,...) # Do the segments on top
  segments(x-eps,y+ll,x+eps,y+ll,...) # and below
}
