##' @title Return country from point
##' @description Determines which country a given point falls in.
##' 
##' @param pt longitude and latitude, as a numeric vector, 2-column table,
##' 	or spatial points object.
##' @param crs the CRS of the coordinate. If \code{pt} is a spatial object,
##' 	this argument is ignored. The default 4326 indicates longlat unprojected.
##' 
##' @details 	
##' 	Based on a predetermined set of global points, this function finds the country of 
##' 	occurrence.
##' 	This can be useful for checking the validity of a point by comparing the returned country 
##' 	to the country listed with the occurrence record.
##' 	If a point falls close to the boundary between two countries, the names of the nearby 
##' 	countries are returned.
##' 	This function will not be of much value if the point falls in the ocean, as it will 
##' 	return the country that is closest, regardless of how far away it is. 
##' 
##' @return
##' If one point is provided, a character vector is returned.
##' If multiple points are provided, a list of character vectors is returned. 
##' 
##' @author Pascal Title
##' 
##' @examples
##' #point near a country border
##' closestCountry(c(-115.436, 32.657))
##'
##' # testing different input options
##' samp <- sample(1:nrow(crotalus), 10)
##' xy <- crotalus[samp, c('decimallongitude', 'decimallatitude')]
##' sfpts <- sf::st_as_sf(xy, coords = c('decimallongitude', 'decimallatitude'), crs = 4326)
##' sfptsEA <- sf::st_transform(sfpts, crs = '+proj=eqearth')
##' spPts <- as(sfpts, 'Spatial')
##' closestCountry(xy)
##' closestCountry(sfpts)
##' closestCountry(sfptsEA)
##' closestCountry(spPts)
##' 
##' @export


closestCountry <- function(pt, crs = 4326) {

	# convert from sp to sf
	if (inherits(pt, c('SpatialPoints', 'SpatialPointsDataFrame'))) {
		pt <- sf::st_as_sf(pt)
	}	
	# if spatial object
	if (any(inherits(pt, c('sf', 'sfc')))) {
		crs <- sf::st_crs(pt)
		if (is.na(crs)) {
			stop('If pt is a spatial object, it must have a crs.')
		}
		pt <- sf::st_coordinates(pt)
	}
	
	if (is.numeric(pt) & !is.matrix(pt)) {
		pt <- matrix(pt, nrow = 1, ncol = 2)
	} else {
		pt <- as.matrix(pt)
	}
	
	if (!sf::st_is_longlat(sf::st_crs(crs))) {
		pt <- sf::st_multipoint(pt)
		pt <- sf::st_sfc(pt, crs = sf::st_crs(crs))
		pt <- sf::st_transform(pt, crs = 4326)
		pt <- sf::st_coordinates(pt)[, c('X', 'Y')]
	}
		
	# # if single row table, convert to vector
	# if (is.matrix(pt) | is.data.frame(pt)) {
		# if (nrow(pt) == 1) {
			# pt <- as.numeric(pt)
		# }
	# }

	# if multiple points
	if (is.matrix(pt) | is.data.frame(pt)) {
		w <- shortDistInd(as.matrix(pt), as.matrix(worldPoints))
		ret <- worldPointCountries[w]
		names(ret) <- NULL
		return(ret)
	} else {
		w <- shortDistInd(as.matrix(pt), as.matrix(worldPoints))
		return(worldPointCountries[[w]])
	} 
}

