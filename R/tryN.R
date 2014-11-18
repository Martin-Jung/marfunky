#' Wrapper for the default tryCatch call
#'@author Martin Jung
#'@param A function
#'\code{FALSE}
#'@export

tryN<-function(f) tryCatch(f, error=function(e) return(NA))