#' Trims the whitespace from a string
#' 
#' Removes all whitespace from a string
#' @param s Input string
#' @keywords string manipulation
#' @export
#' @author Martin Jung

trim_whitespace <-function(s) gsub("^[[:space:]]+|[[:space:]]+$","",s)