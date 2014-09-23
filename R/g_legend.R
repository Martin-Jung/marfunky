#' Extracts the legend of a ggplot objects as grob
#' @param a ggplot object
#' @keywords ggplot,plot,legend
#' @export
#' @usage summarySE(data,Value)
#' @author Anonymous


# On one page with combined legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}