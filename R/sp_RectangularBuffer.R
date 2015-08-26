#' Creates a rectangular buffer around each point
#' 
#' @author Martin Jung
#' @import sp
#' @import rgdal
#' @param points A spatialPoints
#' @param radius The radius according to the projection (Default: 20)
#' @return Returns a rectangular polygon around each point
#' @export


sp_recbuf <- function(points,radius=20) {
  # Load Packages
  if(!require(sp)) install.packages("sp");library(sp)
  if(!require(rgdal)) install.packages("rgdal");library(rgdal)
  
  # Points CRS
  points_crs <- proj4string(points)
  points_data <- as.data.frame(points)

  #define the plot boundaries based upon the radius. NOTE: this assumes that plots are oriented North and are not rotated
  yPlus <- points$northing+radius
  xPlus <- points$easting+radius
  yMinus <- points$northing-radius
  xMinus <- points$easting-radius
  
  #Extract the plot ID information
  ID <- points$Plot_ID
  
  #calculate polygon coordinates for each point. NOTE: the first vertex coordinate is repeated (xMinus,yPlus) to close the polygon.
  square <- cbind(xMinus,yPlus, xPlus,yPlus, xPlus,yMinus, xMinus,yMinus,xMinus,yPlus,xMinus,yPlus)
 
  polys <- SpatialPolygons(mapply(function(poly, id) {
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  }, split(square, row(square)), ID),proj4string=CRS(as.character(points_crs)))
  
  # Create SpatilPolygonDataFrame -- this step is required to output multiple polygons.
  new_data <- cbind(  data.frame(id=ID, row.names=ID),points_data)
  polys.df <- SpatialPolygonsDataFrame(polys, new_data )
  
  return(polys.df)
}