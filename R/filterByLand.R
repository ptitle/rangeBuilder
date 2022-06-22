##' Filter occurrences based on land vs ocean
##' 
##' Identifies occurrence records that do not occur on land.
##' 
##' This function uses a rasterized version of the GSHHG (global
##' self-consistent, hierarchical, high-resolution geography database,
##' \url{https://www.soest.hawaii.edu/pwessel/gshhg/}), that has been buffered
##' by 2 km.
##' 
##' @param coords coordinates in the form of a 2 column numeric matrix,
##' data.frame, numeric vector, or SpatialPoints object. If Spatial object,
##' proj4string must be specified.
##' @param proj proj4string of input coords. Ignored if input coords are
##' spatial object.
##' @return returns a logical vector where \code{TRUE} means the point falls on
##' land.
##' @author Pascal Title
##' @examples
##' 
##' data(crotalus)
##' 
##' #identify points that fall off land
##' filterByLand(crotalus[,c('decimallongitude','decimallatitude')])
##' 
##' @export

filterByLand <- function(coords, proj = '+proj=longlat +datum=WGS84') {

	# if vector, convert to matrix
	if (is.null(dim(coords))) {
		coords <- matrix(coords, nrow = 1, ncol = 2)
	}

	if (inherits(coords, 'data.frame')) {
		coords <- as.matrix(coords)
	}

	if (any(inherits(coords, c('SpatialPoints', 'SpatialPointsDataFrame')))) {
		
		if (is.na(proj4string(coords))) {
			stop('proj4string must be specified for spatial input.')
		}

		# transform if needed
		if (proj4string(worldRaster) != proj) {
			coords <- sp::spTransform(coords, CRS(proj4string(worldRaster)))
		}

		# convert to matrix
		coords <- as.matrix(as.data.frame(coords))
	}

	if (inherits(coords, 'matrix') & proj4string(worldRaster) != proj) {

		#transform
		coords <- SpatialPoints(coords, CRS(proj))
		coords <- sp::spTransform(coords, CRS(proj4string(worldRaster)))
		coords <- as.matrix(as.data.frame(coords))
	}

	if (!inherits(coords, 'matrix') | mode(coords) != 'numeric') {
		stop('coords must be a numeric matrix.')
	}

	#extract worldRaster values
	e <- raster::extract(worldRaster, coords)
	e <- as.logical(e)

	return(e)	
}