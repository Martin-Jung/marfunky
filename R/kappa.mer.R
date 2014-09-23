#' Calculates the kappa value for a given merMod model
#' Condition matrix of a models parameters using Laplace approximations
#' 
#' @param fit A merMod object
#' @param scale standardize Input (default: T) 
#' @param center center to the mean (default: F) 
#' @param add.intercept Add intercept to kappa matrix (default: T) 
#' @param exact Exact kappa values
#' @keywords vif, lme4
#' @export
#' @usage vif.mer(modelfit)
#' @author Austin F. Frank (https://github.com/aufrank/R-hacks)

kappa.mer <- function (fit,
                       scale = TRUE, center = FALSE,
                       add.intercept = TRUE,
                       exact = FALSE) {
  X <- fit@pp$X
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  nrp <- sum(1 * (nam == "(Intercept)"))
  if (nrp > 0) {
    X <- X[, -(1:nrp), drop = FALSE]
    nam <- nam[-(1:nrp)]
  }
  
  if (add.intercept) {
    X <- cbind(rep(1), scale(X, scale = scale, center = center))
    kappa(X, exact = exact)
  } else {
    kappa(scale(X, scale = scale, center = scale), exact = exact)
  }
}
