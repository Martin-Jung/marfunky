#' Save a list of data.frames as new sheet in a new excel file
#' 
#' This functions require the xlsx package to be installed and working.
#' You can save any number of dataframes into a new excel sheet
#' @param file output file name
#' @keywords export excel
#' @export
#' @usage save.xlsx("myworkbook.xlsx", dataframe1, dataframe2, ...)
#' @author Martin Jung

save.xlsx <- function (file, ...)
{
  xlsx = require(xlsx, quietly = TRUE)
  if(xlsx==F) {print("xlsx not working. Will try to install it again now");install.packages("xlsx", dependencies = TRUE)}
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}
