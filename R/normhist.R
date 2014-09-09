#' Display a histogram with different density curves
#' 
#' This functions takes use of the fitdistr(..) function in the MASS package
#' and is able to display density curves on a standard histogramm as well as
#' their cumulative distribution.
#' @param values Input numerical vector
#' @param d1 Enter the name of a distribtion you want to fit your data to
#' @param d2 Enter the name of a second distribtion you want to fit your data to
#' @keywords distribution
#' @export
#' @usage normhist(yourvector)
#' @author Martin Jung
#' 

# Display fit curves on histogramm with log and log-normal-distribution
normhist <- function(values,d1="normal",d2="lognormal"){
  if (!require(MASS)){print("MASS is not installed. Will install it now");install.packages(MASS, dependencies = TRUE)}
  par2 <- par(no.readonly=T)
  fit1 <- fitdistr(values,d1)
  fit2 <- fitdistr(values,d2)
  # 
  par(mfrow=(c(2,2)))
  par(pty="s", bty="l",cex.main=0.8, cex=1.0, tcl=0.2, mar=c(3,1,2,0)+0.5, mgp=c(2,0.5,0))
  hist(values, freq=F, las=1,main=paste("Histogramm with ",d1,"(red)\n and ",d2,"(blue) curves",sep=""))
  curve(dnorm(x, fit1$estimate[1], fit1$estimate[2]),add=T, lwd=2,col="red")
  curve(dlnorm(x, fit2$estimate[1], fit2$estimate[2]), add=T,lwd=2, col="blue")
  # ECDF cumulative distribution
  plot(ecdf(values), pch="", verticals=T, las=1, main=paste(as.character(d1),"distribution",sep=" "))
  curve(pnorm(x, mean=fit1$estimate[1], sd=fit1$estimate[2]), add=T, lwd=3,col="red")
  lines(ecdf(values), pch="", verticals=T)
  plot(ecdf(values), pch="", verticals=T, las=1, main=paste(as.character(d2),"distribution",sep=" "))
  curve(dlnorm(x, fit2$estimate[1], fit2$estimate[2]), add=T, lwd=3,col="blue")
  lines(ecdf(values), pch="", verticals=T)
  # QQplot
  qqnorm(values,pch=19);qqline(values,lwd=2,col="blue")
  
  par(par2);rm(par2)
}