#' Calculates Nakagawa conditional and marginal coefficient of determination for a lme4 model 
#' Marginal r² gives the variance explained by fixed factors
#' Conditional r² gives the variance explained by fixed and random factors (entire model)
#'@author Martin Jung
#'@param A lme4 model object
#'@param Variance Type (default 2)
#'\code{FALSE}
#'@export

r2.nakagawa.lme4<-function(m, varType=2){
  VarF<-var(as.vector(model.matrix(m) %*% lme4::fixef(m)))
  VarR<-c(det(lme4::VarCorr(m)[[1]]), sum(diag(lme4::VarCorr(m)[[1]])))
  list(marginal=c(VarF/(VarF+VarR+pi^2/3), VarF/(var(fitted(m))+pi^2/3))[varType],
       conditional=c((VarF+VarR)/(VarF+VarR+pi^2/3),var(fitted(m))/(var(fitted(m))+pi^2/3))[varType])
}
