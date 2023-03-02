##' Filter by proximity
##' 
##' Filter occurrence records by their proximity to each other.
##' 
##' This function will discard coordinates that fall within a certain distance
##' from other points.
##' 
##' @param xy longitude and latitude in decimal degrees, either as a matrix,
##' dataframe, or spatial points object.
##' @param dist minimum allowed distance in km
##' @param returnIndex if \code{TRUE}, will return indices of points that would
##' be dropped, if \code{FALSE}, returns the points that satisfy the distance
##' filter.
##' @return If \code{returnIndex = TRUE}, returns a numeric vector of indices.
##' If \code{returnIndex = FALSE}, returns coordinates of the same class as the
##' input.
##' @author Pascal Title
##' @examples
##' 
##' data(crotalus)
##' 
##' # within the first 100 points in the dataset, identify the set of points to 
##' # drop in order to have points no closer to each other than 20 km
##' 
##' subset <- crotalus[1:100,]
##' tooClose <- filterByProximity(xy= subset[ ,c('decimallongitude','decimallatitude')], 
##' 	dist=20, returnIndex = TRUE)
##' 
##' plot(subset[ ,c('decimallongitude','decimallatitude')], pch=1, col='blue', cex=1.5)
##' points(subset[tooClose, c('decimallongitude','decimallatitude')], pch=20, col='red')
##' 
##'
##' # testing different input options
##' samp <- sample(1:nrow(crotalus), 100)
##' xy <- crotalus[samp, c('decimallongitude', 'decimallatitude')]
##' sfpts <- sf::st_as_sf(xy, coords = c('decimallongitude', 'decimallatitude'), crs = 4326)
##' sfptsEA <- sf::st_transform(sfpts, crs = '+proj=eqearth')
##' spPts <- as(sfpts, 'Spatial')
##' filterByProximity(xy, dist=20, returnIndex = TRUE)
##' filterByProximity(sfpts, dist=20, returnIndex = TRUE)
##' filterByProximity(sfptsEA, dist=20, returnIndex = TRUE)
##' filterByProximity(spPts, dist=20, returnIndex = TRUE)
##'

##' @export

filterByProximity <- function(xy, dist, returnIndex = FALSE) {
	
	# xy can be either a spatial object, or a matrix
	# returnIndex = TRUE will return indices of points that would be dropped
	
	if (inherits(xy, c('SpatialPoints', 'SpatialPointsDataFrame'))) {
		xy <- sf::st_as_sf(xy)
	}
	
	if (!any(inherits(xy, c('sf', 'sfc')))) {
		if (ncol(xy) != 2) {
			stop('xy must be a spatial points object or a 2-column table.')
		}
		
		xy <- sf::st_as_sf(as.data.frame(xy), coords = 1:2, crs = 4326)
	} 
	
	d <- sf::st_distance(xy, xy) # in meters
	d <- units::drop_units(d)
	d <- d / 1000 # in km
	
	diag(d) <- NA
	close <- d <= dist
	diag(close) <- NA
	closePts <- which(close, arr.ind = TRUE)
	discard <- matrix(nrow = 2, ncol=2)
	if (nrow(closePts) > 0) {
		while (nrow(closePts) > 0) {
			if ((!paste(closePts[1,1],closePts[1,2],sep='_') %in% paste(discard[,1],discard[,2],sep='_')) & (!paste(closePts[1,2],closePts[1,1],sep='_') %in% paste(discard[,1],discard[,2],sep='_'))) {
				discard <- rbind(discard, closePts[1,])
				closePts <- closePts[-union(which(closePts[,1] == closePts[1,1]), which(closePts[,2] == closePts[1,1])),]
			}
		}
		discard <- discard[stats::complete.cases(discard),]
		if (inherits(discard, 'matrix')) {
			if (returnIndex) {
				return(discard[,1])
			} else {
				return(xy[-discard[,1],])
			}
		} else {
			if (returnIndex) {
				return(discard[1])
			} else {
				return(xy[-discard[1],])
			}
		}
	}
	if (nrow(closePts) == 0) {
		if (returnIndex) {
			return(NA)
		} else {
			return(xy)
		}
	}
}

