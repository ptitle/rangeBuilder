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
##' data.frame, numeric vector, or spatial points object (sf or sp). 
##' If spatial object, crs must be defined.
##' @param crs crs of input coords. Ignored if input coords are
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
##' 
##' # testing different input options
##' samp <- sample(1:nrow(crotalus), 10)
##' xy <- crotalus[samp, c('decimallongitude', 'decimallatitude')]
##' sfpts <- sf::st_as_sf(xy, coords = c('decimallongitude', 'decimallatitude'), crs = 4326)
##' sfptsEA <- sf::st_transform(sfpts, crs = '+proj=eqearth')
##' spPts <- as(sfpts, 'Spatial')
##' filterByLand(xy)
##' filterByLand(sfpts)
##' filterByLand(sfptsEA)
##' filterByLand(spPts)
##' 
##' @export

filterByLand <- function(coords, crs = 4326) {

	# if vector, convert to matrix
	if (is.null(dim(coords))) {
		coords <- matrix(coords, nrow = 1, ncol = 2)
	}
	
	if (!inherits(coords, c('SpatialPoints', 'SpatialPointsDataFrame', 'sf', 'sfc'))) {
		if (is.na(crs)) stop('If coords is not a spatial object, then crs must be provided.')
		
		# if vector, convert to matrix
		if (is.null(dim(coords))) {
			coords <- matrix(coords, nrow = 1, ncol = 2)
		}
		
		coords <- sf::st_as_sf(as.data.frame(coords), coords = 1:2, crs = crs)
	}

	if (any(inherits(coords, c('SpatialPoints', 'SpatialPointsDataFrame')))) {
		coords <- sf::st_as_sf(coords)
	}
	
	if (is.na(sf::st_crs(coords))) {
		stop('crs must be specified for spatial input.')
	}
	
	# transform to longlat
	coords <- sf::st_transform(coords, crs = 4326)
	
	coords <- sf::st_coordinates(coords)[, 1:2]
	
	#extract worldRaster values
	e <- terra::extract(terra::rast(worldRaster), coords)[,1]
	e <- as.logical(e)

	return(e)
}

