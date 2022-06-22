##' @title Return country from point
##' @description Determines which country a given point falls in.
##' 
##' @param pt longitude and latitude, as a numeric vector, 2-column table,
##' 	or SpatialPoints object.
##' @param proj the proj4string of the coordinate. If \code{pt} is a SpatialPoints
##' 	object, proj is ignored. 
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
##' @export


closestCountry <- function(pt, proj = "+proj=longlat +datum=WGS84") {

	# if spatial object
	if (any(inherits(pt, c('SpatialPoints', 'SpatialPointsDataFrame')))) {
		proj <- proj4string(pt)
		if (is.na(proj)) {
			stop('If pt is a SpatialPoints object, it must have a proj4string.')
		}
		pt <- coordinates(pt)
	}

	# check that provided proj4string is valid
	CRS(proj)

	if (proj != "+proj=longlat +datum=WGS84") {
		pt <- SpatialPoints(pt, proj4string=CRS(proj))
		pt <- spTransform(pt, CRS('+proj=longlat +datum=WGS84'))
	}
		
	# if single row table, convert to vector
	if (is.matrix(pt) | is.data.frame(pt)) {
		if (nrow(pt) == 1) {
			pt <- as.numeric(pt)
		}
	}

	# if multiple points
	if (is.matrix(pt) | is.data.frame(pt)) {
		w <- shortDistInd(as.matrix(pt), as.matrix(worldPoints))
		ret <- worldPointCountries[w]
		names(ret) <- NULL
		return(ret)
	} else {
		d <- spDistsN1(worldPoints, pt, longlat = FALSE)
		w <- which.min(d)
		return(worldPointCountries[[w]])
	} 
}

