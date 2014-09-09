#' One of those dynamite-ignition-device plots
#' 
#' Plain simple. Just a barplot with arrows and optionally significance on top ;)
#' @param height Height of the barplot
#' @param error The length of the error line
#' @param names (Optional) vector containing names for x
#' @param significance (Optional) vector containingy significance stars for labeling
#' @param ylim (Optional) Length of y-axis
#' @keywords distribution
#' @export
#' @author Martin Jung


# Schöner bargraph mit Standard-Fehler
Dynamite <- function(height, error, names = NA,significance = NA, ylim = c(0,maxLim), ...){
  maxLim <- 1.1* max(mapply(sum, height, error))
  bp <- barplot(height, names.arg = names, ylim = ylim, ...)
  arrows(x0 = bp, y0 = height, y1 = height + error, angle = 90)
  text(x = bp, y = 0.2 + height + error, labels = significance)
}