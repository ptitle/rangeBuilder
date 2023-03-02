##' Flip sign of coordinates
##' 
##' Checks for coordinate sign mistakes by checking all possibilities against
##' country occupancy.
##' 
##' This function generates all possible coordinates with different signs, and
##' runs \code{\link{closestCountry}} on each, returning the coordinates that
##' lead to a country match. It ignores coordinate options that do not pass
##' \code{\link{filterByLand}}.
##' 
##' If a point falls close to the boundary between two countries, it is still
##' considered a match.
##' 
##' @param coordVec numeric vector of length 2: longitude, latitude
##' @param country the country that is associated with the record
##' @param returnMultiple if multiple sign flips lead to the correct country,
##' return all options.  If \code{FALSE}, returns the coords with the fewest
##' needed sign flips.
##' @param filterByLand if \code{TRUE}, alternative coords will be tested for
##' whether or not they fall on land.
##' @param crs the crs of the coordinate.
##' @return list with 2 elements \item{matched}{ logical: Was the country
##' matched } \item{newcoords}{ matrix of coordinates that were successful.  }
##' @author Pascal Title
##' @examples
##' 
##' #correct coordinates
##' flipSign(c(4.28, 39.98), country = 'Spain')
##' 
##' #mistake in coordinate sign
##' flipSign(c(115.436, 32.657), country = 'United States')
##' 
##' #incorrect sign on both long and lat, but not possible to distinguish for longitude
##' #except when we consider which alternative coords fall on land.
##' flipSign(c(-4.28, -39.98), country = 'Spain', filterByLand = FALSE, returnMultiple = TRUE)
##' flipSign(c(-4.28, -39.98), country = 'Spain', returnMultiple = TRUE)
##' 
##' #coordinates are incorrect
##' flipSign(c(4.28, 59.98), country = 'Spain')
##' 
##' @export


flipSign <- function(coordVec, country, returnMultiple = FALSE, filterByLand = TRUE, crs = 4326) {
#coordVec is a vector: long, lat
# country is the name of the country associated with those coordinates
# returnMultiple: if multiple sign flips lead to the correct country, return all options. If FALSE, returns the coords with the fewest needed sign flips.

	if (is.matrix(coordVec) | is.data.frame(coordVec)) {
		coordVec <- as.numeric(coordVec)
	}

	country <- toupper(country)

	if (!country %in% unlist(countryList)) {
		stop('Country is not recognized.')
	}

	if (country %in% closestCountry(coordVec, crs = crs)) {
		cat('\tCoordinates already match country.\n')
		names(coordVec) <- c('long','lat')
		return(list(matched = TRUE, newcoords = coordVec))
	}

	long <- coordVec[1]
	lat <- coordVec[2]

	# create alternative signs
	allcoords <- matrix(ncol = 2, nrow = 7)
	colnames(allcoords) <- c('long','lat')
	allcoords[1,] <- c(long*-1, lat)
	allcoords[2,] <- c(long, lat*-1)
	allcoords[3,] <- c(lat, long)
	allcoords[4,] <- c(lat*-1, long)
	allcoords[5,] <- c(lat, long*-1)
	allcoords[6,] <- c(long*-1, lat*-1)
	allcoords[7,] <- c(lat*-1, long*-1)
	
	#but remove nonsensical coordinates
	bb <- sf::st_as_sf(data.frame(matrix(c(-180, -90, 180, 90), nrow=2, ncol=2, byrow=TRUE)), coords = 1:2, crs = 4326)
	bb <- sf::st_transform(bb, crs = crs)
	bb <- sf::st_bbox(bb)

	drop <- union(which(allcoords[,'lat'] > bb['ymax']), which(allcoords[,'lat'] < bb['ymin']))
	if (length(drop) > 0) allcoords <- allcoords[-drop,]
	
	# also, remove options that don't fall on land
	if (filterByLand) {
		allcoords <- allcoords[filterByLand(allcoords, crs = crs),]
	}
	
	if (inherits(allcoords, 'numeric')) {
		allcoords <- matrix(allcoords, nrow = 1)
	}

	if (nrow(allcoords) == 0) {
		return(list(matched = FALSE, newcoords = NA))
	}

	allcountries <- lapply(1:nrow(allcoords), function(x) closestCountry(allcoords[x,], crs = crs))

	if (country %in% unlist(allcountries)) {
		match <- lapply(allcountries, function(x) country %in% x)
		newcoords <- allcoords[which(match == TRUE),]
		newcoords <- matrix(newcoords, ncol = 2, byrow = FALSE)
		colnames(newcoords) <- c('long','lat')

		if (nrow(newcoords) == 2 & returnMultiple == FALSE) {
			if (identical(as.numeric(c(newcoords[1,1] * (-1), newcoords[1,2])), coordVec) | identical(as.numeric(c(newcoords[1,1], newcoords[1,2] * (-1))), coordVec)) {
				newcoords <- newcoords[1,]
			} else if (identical(as.numeric(c(newcoords[2,1] * (-1), newcoords[2,2])), coordVec) | identical(as.numeric(c(newcoords[2,1], newcoords[2,2] * (-1))), coordVec)) {
				newcoords <- newcoords[2,]
			}
		}

		return(list(matched = TRUE, newcoords = newcoords))
	} else {
		return(list(matched = FALSE, newcoords = NA))
	}
}

# if two coordinates return the correct country, we will return the one with the fewest changes needed