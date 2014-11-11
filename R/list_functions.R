#'List all functions in a package
#'@author Noam Ross 
#'@param package Which package to list functions from
#'@param all.names Include hidden functions (starting with ".")? Defaults to
#'\code{FALSE}
#'@param pattern A regex pattern to limit the results
#'@export
list_functions <-function(package, all.names = FALSE, pattern)
{
  namesp <- deparse(substitute(package))
  ls(
    name = paste("package", namesp, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}