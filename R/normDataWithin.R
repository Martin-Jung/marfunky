#' Norms the data within specified groups in a data frame
#' Norms the data within specified groups in a data frame; it normalizes each
#'  subject (identified by idvar) so that they have the same mean, within each group
#'  specified by betweenvars.

#' @param data A data frame
#' @param idvar: the name of a column that identifies each subject (or matched subjects)
#' @param measurevar: the name of a column that contains the variable to be summarized
#' @param betweenvars: a vector containing names of columns that are between-subjects variables
#' @param na.rm: a boolean that indicates whether to ignore NA's
#' @keywords package loading
#' @export
#' @usage normDataWithin(data,Value)
#' @author R Cookbook (http://www.cookbook-r.com/Manipulating_data/Summarizing_data/)


normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}
