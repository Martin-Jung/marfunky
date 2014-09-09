#' Extract numbers from string or character
#' 
#' Good for data mining
#' @param x Input string
#' @keywords subset
#' @export
#' @usage numbers_from_string("Today we ate 15 apples")
#' @author Martin Jung

# Extract numbers from string or character
numbers_from_string <- function(x) as.numeric(gsub("\\D", "", x))
