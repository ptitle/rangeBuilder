##' @title Get extent of list
##'
##' @description Given a list of SpatialPolygons or sf objects, return 
##' a bounding box object that encompasses all items. 
##'
##' @param shapes a list of SpatialPolygons or simple features
##'
##' @return An object of class \code{bbox}. 
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
##' 	sf::st_convex_hull(sf::st_combine(sf::st_as_sf(x, coords = 1:2, crs = 4326))))
##' 
##' getExtentOfList(poly)
##' 
##' @export

getExtentOfList <- function(shapes) {
	
	if (inherits(shapes, c('sf', 'sfc')) & !inherits(shapes, 'list')) {
		shapes <- list(shapes)
	}

	if (inherits(shapes[[1]], c('SpatialPolygons', 'SpatialPolygonsDataFrame'))) {	
		shapes <- lapply(shapes, function(x) sf::st_as_sf(x))
	} 
	
	if (inherits(shapes[[1]], c('sf', 'sfc'))) {
		x <- lapply(shapes, function(x) sf::st_bbox(x))
	} else {
		stop('shapes object not recognized.')
	}

	minLong <- min(sapply(x, function(x) x$xmin, simplify = TRUE), na.rm = TRUE)
	maxLong <- max(sapply(x, function(x) x$xmax, simplify = TRUE), na.rm = TRUE)
	minLat <- min(sapply(x, function(x) x$ymin, simplify = TRUE), na.rm = TRUE)
	maxLat <- max(sapply(x, function(x) x$ymax, simplify = TRUE), na.rm = TRUE)

	res <- x[[1]]
	res[[1]]<- minLong
	res[[2]] <- minLat
	res[[3]] <- maxLong
	res[[4]] <- maxLat
	
	return(res)
}	
