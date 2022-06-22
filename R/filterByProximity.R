##' Filter by proximity
##' 
##' Filter occurrence records by their proximity to each other.
##' 
##' This function will discard coordinates that fall within a certain distance
##' from other points.
##' 
##' @param xy longitude and latitude in decimal degrees, either as class
##' matrix, SpatialPoints or SpatialPointsDataFrame.
##' @param dist minimum allowed distance
##' @param mapUnits if \code{TRUE}, distance is interpreted in map units,
##' distance in kilometers if \code{FALSE}
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
##' 	dist=20, mapUnits = FALSE, returnIndex = TRUE)
##' 
##' plot(subset[ ,c('decimallongitude','decimallatitude')], pch=1, col='blue', cex=1.5)
##' points(subset[tooClose, c('decimallongitude','decimallatitude')], pch=20, col='red')
##' 
##' @export

filterByProximity <- function(xy, dist, mapUnits = FALSE, returnIndex = FALSE) {

	#xy can be either a SpatialPoints or SPDF object, or a matrix
	#dist is in km if mapUnits=F, in mapUnits otherwise
	#returnIndex = TRUE will return indices of points that would be dropped
	if (inherits(xy, 'data.frame')) {
		xy <- as.matrix(xy)
	}
	if (!mapUnits) {
		d <- spDists(xy,longlat = TRUE)
	}
	if (mapUnits) {
		if (is.matrix(xy)) {
			d <- as.matrix(dist(xy, method = 'euclidian'))
		} else {
			d <- spDists(xy, longlat = FALSE)
		}
	}
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
		discard <- discard[complete.cases(discard),]
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

