#' Print status messages tracking run time for an R script
#'
#' @author Alex Zvoleff
#' @export
#' @import raster
#' @param rastsize the dimension of a side of a square matrix to write to disk 
#' as a \code{RasterLayer}
#' @param filext the file extension to use for the temporary filename passed to 
#' the \code{writeRaster} function
#' @param tmpdir the directory to write the \code{RasterLayer} in
#' @param datatype the data type to use when writing the \code{RasterLayer}.  
#' See \code{\link{raster::dataType}}
#' @param reps
#' @examples
#' #TODO: Add examples
testRasterWriteSpeed <- function(rastsize=8000, fileext='.envi', tmpdir='.', 
                                 datatype='FLT8S', reps=10) {
  rast <- raster(matrix(runif(rastsize^2), nrow=rastsize))
  tempname <- tempfile(pattern='testRasterWriteSpeed', tmpdir=tmpdir, fileext=fileext)
  
  timings <- vector('numeric', reps)
  message('Testing raster write speed...')
  pb <- pbCreate(reps, 'text')
  for (i in 1:reps) {
    timings[i] <- system.time(writeRaster(rast, tempname, datatype=datatype))[3]
    # remove the temporary raster file
    unlink(paste0(sub('[.][[:alnum:]]+$', '', tempname), '.*'))
    pbStep(pb)
  }
  pbClose(pb)
  
  return(mean(timings))
}
