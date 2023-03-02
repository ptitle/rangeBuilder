##' @title Coordinate error

##' @description Calculates the potential error in coordinates due to lack of coordinate precision. 

##' @param coords longitude and latitude in decimal degrees, either as a long/lat vector, or as 
##' 	a 2-column table. Can be either as numeric or character format

##' @param nthreads
##' 	number of threads to use for parallelization of the function. 
##' 	The R package \code{parallel} must be loaded for \code{nthreads > 1}.
##'
##' @details
##' 	This function assumes that the true precision of the coordinates is equivalent to the 
##' 	greatest number of decimals in either the longitude or latitude that are not trailing 
##' 	zeroes. In other words: \cr
##' 	\code{(-130.45670, 45.53000)} is interpreted as \code{(-130.4567, 45.5300)} \cr
##' 	\code{(-130.20000, 45.50000)} is interpreted as \code{(-130.2, 45.5)} \cr
##' 
##' 	If we use \code{(-130.45670, 45.53000)} as an example, these coordinates are interpreted 
##' 	as \code{(-130.4567, 45.5300)} and the greatest possible error is inferred as two 
##' endpoints:
##' 	\code{(-130.45670, 45.53000)} and \code{(-130.45679, 45.53009)} \cr
##' 
##' The distance between these two is then calculated and returned. 
##' 
##' @return Returns a vector of coordinate error in meters.

##' @author	Pascal Title

##' @examples
##' data(crotalus)
##'
##' xy <- crotalus[1:100, c('decimallongitude','decimallatitude')]
##'
##' coordError(xy)
##' 
##' @export
 
coordError <- function(coords, nthreads = 1) {
			
	scipenVal <- getOption('scipen')
	options('scipen' = 999)
	
	# vector of coordinates
	if (is.vector(coords)) {
		if (!all(is.na(coords))) {
			if (!any(inherits(coords, c('numeric', 'character')))) {
				stop('coords must be of class numeric or character.')
			}
			if (length(coords) != 2) {
				stop('coords must be of length 2: long, lat.')
			}
			if (inherits(coords, 'numeric')) {
				coords <- as.character(coords)
			}
			# if either coordinate is invalid, replace with NA to trip that check later
			if (abs(as.numeric(coords[1])) > 180 | abs(as.numeric(coords[2])) > 90) {
				coords[1] <- NA
			}
		}
		res <- calcError(coords)
	}
	
	#table of coordinates
	if (is.data.frame(coords)) {
		coords <- as.matrix(coords)
	}
	if (is.matrix(coords)) {
		if (ncol(coords) != 2) {
			stop('coords must be 2 columns: long and lat.')
		}
		mode(coords) <- 'character'
	
		# to simplify handling, convert any NAs to long 190 and lat 100 for now
		if (any(is.na(coords[,1]))) {
			coords[which(is.na(coords[,1])),1] <- "190"
		}
		if (any(is.na(coords[,2]))) {
			coords[which(is.na(coords[,2])),2] <- "100"
		}
		
		#now all bad records have invalid coordinates, no NA
		# switch back to NA
		coords[which(abs(as.numeric(coords[,1])) > 180), 1] <- NA
		coords[which(abs(as.numeric(coords[,2])) > 90), 2] <- NA
		
		op <- pbapply::pboptions(type = "timer")
		if (nrow(coords) < 10) {
			pbapply::pboptions(type = "none")
		}	
		
		if (nthreads > 1) {
			cl <- parallel::makePSOCKcluster(nthreads)
			parallel::clusterExport(cl = cl, varlist = c('coords', 'calcError'), envir = environment())
			parallel::clusterExport(cl = cl, varlist = 'st_as_sf', envir = environment(sf::st_as_sf))
			parallel::clusterExport(cl = cl, varlist = 'st_distance', envir = environment(sf::st_distance))
			res <- pbapply::pbapply(coords, 1, calcError, cl = cl)
			parallel::stopCluster(cl)
		} else {
			res <- pbapply::pbapply(coords, 1, calcError)
		}
		names(res) <- NULL
	}
	
	options('scipen' = scipenVal)
	pbapply::pboptions(op)
	
	return(res)
}
	
	

calcError <- function(xy) {
	# determine number of decimal places
	if (anyNA(xy)) {
		return(NA)
	} else {
		if (grepl('\\.', xy[1])) {
			longDecPlaces <- nchar(strsplit(xy[1], '\\.')[[1]][2])
		} else {
			xy[1] <- paste0(xy[1], '.')
			longDecPlaces <- 0
		}
		if (grepl('\\.', xy[2])) {
			latDecPlaces <- nchar(strsplit(xy[2], '\\.')[[1]][2])
		} else {
			xy[2] <- paste0(xy[2], '.')
			latDecPlaces <- 0
		}
		
		maxDec <- max(longDecPlaces, latDecPlaces)
		
		# min coords
		minCoords <- vector('character', length = 2)
		if (maxDec - longDecPlaces > 0) {
			minCoords[1] <- paste0(xy[1], paste(rep('0', maxDec - longDecPlaces), collapse = ''), '0')
		} else {
			minCoords[1] <- paste0(xy[1], '0')
		}
	
		if (maxDec - latDecPlaces > 0) {
			minCoords[2] <- paste0(xy[2], paste(rep('0', maxDec - latDecPlaces), collapse = ''), '0')
		} else {
			minCoords[2] <- paste0(xy[2], '0')
		}
	
		# max coords
		maxCoords <- vector('character', length = 2)
		if (maxDec - longDecPlaces > 0) {
			maxCoords[1] <- paste0(xy[1], paste(rep('0', maxDec - longDecPlaces), collapse = ''), '9')
		} else {
			maxCoords[1] <- paste0(xy[1], '9')
		}
	
		if (maxDec - latDecPlaces > 0) {
			maxCoords[2] <- paste0(xy[2], paste(rep('0', maxDec - latDecPlaces), collapse = ''), '9')
		} else {
			maxCoords[2] <- paste0(xy[2], '9')
		}
		
		# calculate distance
		pts <- sf::st_as_sf(rbind.data.frame(as.numeric(minCoords), as.numeric(maxCoords)), coords = 1:2, crs = 4326)

		# returns meters for longlat
		return(sf::st_distance(pts[1,], pts[2,]))
	}
}








