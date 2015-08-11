#'  Diagnostics for an lme4 merMod Model
#' @author Martin Jung
#' @param mod A mixed effects model object
#' @return returns Graphs and stats
#' @export

glmerDiag <- function(mod){
  if(!require(lme4)) install.packages("lme4");library(lme4)
  if(!require(gridExtra)) install.packages("gridExtra");library(gridExtra)
  if(!require(aods3)) install.packages("aods3");library(aods3)
  grid.arrange(plot(mod,type=c("p","smooth"),main="Homogeneity of variance?"),
               plot(mod,sqrt(abs(resid(.)))~fitted(.),
                    type=c("p","smooth"),ylab=expression(sqrt(abs(resid))))
  )

  x <- readline("Normality - Press <ENTER>")
  
  qqnorm(resid(mod),main="Q-Q plot for residuals \n Normality of residuals?",pch=22)
  qqline(resid(mod),lwd=2,col="black")  
  
  x <- readline("Random effects - Press <ENTER>")
  
  # Plot all Random effects
  dd <- dotplot(ranef(mod,condVar=TRUE))
  do.call(grid.arrange,c(dd,list(nrow=1)))
  
  # Autocorrelation structure
  if(class(mod)=="lme"){
    require(nlme)
    x <- readline("Autocorrelation structure - Press <ENTER>")
    grid.arrange(plot(ACF(mod,resType="response"),alpha=0.05,main="'raw' residuals"),
                 plot(ACF(mod,resType="normalized"),alpha=0.05,main="Normalized residuals"),
                 nrow=1)
  } 
  
  
  x <- readline("Overdispersion - Press <ENTER>")
  
  suppressWarnings(gof(mod))
  print("Ratio between df and residuals should be approx. 1")

}