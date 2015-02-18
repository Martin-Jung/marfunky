#' Simplifies a polygon by removing every n vertices
#' 
#' @author Jeffrey W Hollister
#' @param sp_obj A spatial object
#' @param rep Which n nodes should be removed
#' @export
#' 
rm_polygonnodes<-function(sp_obj,rep){
  
  exist_pts<-coordinates(as(sp_obj,"SpatialLines"))
  
  create_Polygon<-function(coords,every){
    coords[[1]]<-matrix(coords[[1]],ncol=2)
    coords[[1]]<-coords[[1]][1:nrow(coords[[1]])%%every>0,]
    coords[[1]]<-rbind(coords[[1]],coords[[1]][1,])
    return(Polygon(coords[[1]]))
  }
  
  create_Polygons<-function(polygon){
    Srl_list<-list()
    for(i in 1:length(polygon)){
      Srl_list[[i]]<-Polygons(list(polygon[[i]]),ID=as.character(i))
    }
    return(Srl_list)
  }
  
  reduced_Polygon <- lapply(exist_pts,function(x) create_Polygon(x,every))
  reduced_Polygons <- create_Polygons(reduced_Polygon)
  reduced_SpatialPolygons <- SpatialPolygons(reduced_Polygons,proj4string=CRS(proj4string(sp_obj)))
  return(reduced_SpatialPolygons)
}