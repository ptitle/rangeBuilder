##' Get extent of list of SpatialPolygons
##' 
##' Returns the extent that encompasses all SpatialPolygons in a list
##' 
##' 
##' @param shapes a list of SpatialPolygons
##' @return an object of class extent
##' @author Pascal Title
##' @examples
##' 
##' data(crotalus)
##' 
##' # create some polygons, in this case convex hulls
##' sp <- split(crotalus, crotalus$genSp)
##' sp <- lapply(sp, function(x) x[,c('decimallongitude','decimallatitude')])
##' sp <- lapply(sp, function(x) x[chull(x),])
##' poly <- lapply(sp, function(x) 
##' 	SpatialPolygons(list(Polygons(list(Polygon(x)), ID = 1))))
##' 
##' getExtentOfList(poly)
##' 
##' @export

getExtentOfList <- function(shapes) {	
	x <- lapply(shapes, function(x) bbox(x))
	minLong <- min(sapply(x, function(x) x[1], simplify = TRUE))
	maxLong <- max(sapply(x, function(x) x[3], simplify = TRUE))
	minLat <- min(sapply(x, function(x) x[2], simplify = TRUE))
	maxLat <- max(sapply(x, function(x) x[4], simplify = TRUE))
	
	res <- raster::extent(shapes[[1]])
	res@xmin <- minLong
	res@xmax <- maxLong
	res@ymin <- minLat
	res@ymax <- maxLat
	
	return(res)
}	
