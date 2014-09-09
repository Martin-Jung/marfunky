#' Attemptes to fit input values to different distributions. 
#' 
#' This functions takes use of the fitdistr(..) function in the MASS package
#' Returns a sorted table containing AIC/BIC values. Might give you some indication 
#' of the underlying value distribution, but please do a visual expectation as well!
#' @param values Input numerical vector
#' @keywords distribution
#' @export
#' @usage testdist(yourvector)
#' @author Martin Jung


# Displays AIC/BIC for various distribution models
testdist <- function(values){
  require(MASS)
  distributions <- c("normal","lognormal","exponential","logistic","cauchy","gamma","geometric","weibull")
  res <- data.frame(cbind(distributions)); res[,c("AIC","BIC")] <- NA
  for(i in seq(1:nrow(res))){
    fit <- fitdistr(values,densfun=as.character(res$distributions[i]),)
    res$AIC[i] <- AIC(fit);res$BIC[i] <- BIC(fit)
  }
  res <- res[order(res$BIC,decreasing=F,na.last=T),]
  return(res)
}
