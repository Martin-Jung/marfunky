#' Calculates the variance inflation factor (VIF) of a model
#' 
#' @param fit A merMod object
#' @keywords vif, lme4
#' @export
#' @usage vif.mer(modelfit)
#' @author Austin F. Frank (https://github.com/aufrank/R-hacks)

vif.mer <- function (fit) {
  require(lme4)
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
