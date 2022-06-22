##' Generate polygon based on alpha hulls
##' 
##' Generates an apha hull polygon, where the alpha parameter is determined by
##' the spatial distribution of the coordinates.
##' 
##' 
##' From a set of coordinates, this function will create an alpha hull with
##' \code{alpha = initialAlpha}, and will then increase \code{alpha} by
##' \code{alphaIncrement} until both the \code{fraction} and \code{partCount}
##' conditions are met.
##' 
##' If the conditions cannot be satisfied, then a minimum convex hull is
##' returned.
##' 
##' If \code{clipToCoast} is set to "terrestrial" or "aquatic", the resulting
##' polygon is clipped to the coastline, using the \code{\link{gshhs}} dataset
##' provided with this package.
##' 
##' @param x dataframe of coordinates in decimal degrees, with a minimum of 3
##' rows.
##' @param fraction the minimum fraction of occurrences that must be included
##' in polygon.
##' @param partCount the maximum number of disjunct polygons that are allowed.
##' @param buff buffering distance in meters
##' @param initialAlpha the starting value for alpha
##' @param coordHeaders the column names for the longitude and latitude
##' columns, respectively.  If x has two columns, these are assumed to be
##' longitude and latitude, and \code{coordHeaders} is ignored.
##' @param clipToCoast Either "no" (no clipping), "terrestrial" (only
##' terrestrial part of range is kept) or "aquatic" (only non-terrestrial part
##' is clipped). See Details.
##' @param proj the projection information for x. The default is currently the
##' only supported option.
##' @param alphaIncrement the amount to increase alpha with each iteration
##' @param verbose prints the alpha value to the console, intended for
##' debugging.
##' @param alphaCap Max alpha value before function aborts and returns a
##' minimum convex hull.
##' @return a list with 2 elements: \item{hull}{ a SpatialPolygons object }
##' \item{alpha}{ the alpha value that was found to satisfy the criteria.  If a
##' convex hull was returned, this will list MCH.  }
##' @author Pascal Title
##' @seealso Alpha hulls are created with \code{\link{ahull}}.
##' @examples
##' 
##' data(crotalus)
##' 
##' # create a polygon range for Crotalus atrox
##' x <- crotalus[which(crotalus$genSp == 'Crotalus_atrox'),]
##' x <- x[sample(1:nrow(x), 50),]
##' 
##' range <- getDynamicAlphaHull(x, coordHeaders=c('decimallongitude','decimallatitude'), 
##' 	clipToCoast = 'no')
##' 
##' plot(range[[1]], col=transparentColor('dark green', 0.5), border = NA)
##' points(x[,c('decimallongitude','decimallatitude')], cex = 0.5, pch = 3)
##' 
##' # to add a basic coastline
##' # plot(gshhs, add = TRUE)
##' 
##' @export

getDynamicAlphaHull <- function(x, fraction = 0.95, partCount = 3, buff = 10000, initialAlpha = 3, coordHeaders = c('Longitude', 'Latitude'), clipToCoast = 'terrestrial', proj = "+proj=longlat +datum=WGS84", alphaIncrement = 1, verbose = FALSE, alphaCap = 400) {

	if (proj != "+proj=longlat +datum=WGS84") {
		stop("Currently, proj can only be '+proj=longlat +datum=WGS84'.")
	}

	if (clipToCoast == FALSE) {clipToCoast <- 'no'}
	clipToCoast <- match.arg(clipToCoast, c('no', 'terrestrial', 'aquatic'))

	if (ncol(x) == 2) {
		coordHeaders <- c(1,2)
	}

	#reduce to unique coordinates
	x <- x[!duplicated(x[,coordHeaders]), coordHeaders]
	x <- x[complete.cases(x),]
	
	if (nrow(x) < 3) {
		stop('This function requires a minimum of 3 unique coordinates (after removal of duplicates).')
	}
	
	#Alpha hulls cannot be generated if first 3 points are linear. 
	while ((x[1,coordHeaders[1]] == x[2,coordHeaders[1]] & x[2, coordHeaders[1]] == x[3, coordHeaders[1]]) | (x[1,2] == x[2, coordHeaders[2]] & x[2, coordHeaders[2]] == x[3, coordHeaders[2]])) {
		x <- x[sample(1:nrow(x),size = nrow(x)),]
	}

	#create spatialpoints
	x <- SpatialPoints(x, proj4string = CRS(proj))
	x <- remove.duplicates(x)
	if (length(x) < 3) {
		stop('This function requires a minimum of 3 unique coordinates.')
	}

	#create alpha hull and calculate fraction of occurrences that fall within
	#continue until fraction is reached
	alpha <- initialAlpha
	problem <- FALSE
	if (verbose) {cat('\talpha:', alpha, '\n')}

	hull <- try(alphahull::ahull(data.frame(x),alpha = alpha), silent = TRUE)
	while (inherits(hull, 'try-error')) {
		if (verbose) {cat('\talpha:', alpha, '\n')}
		alpha <- alpha + alphaIncrement
		hull <- try(alphahull::ahull(data.frame(x),alpha = alpha), silent = TRUE)
		if (alpha > alphaCap) {
			problem <- TRUE
			break
		}
	}
	
	if (!problem) {

		hull <- ah2sp(hull, proj4string = CRS('+proj=longlat +datum=WGS84'))
	
		if (!is.null(hull)) {
			slot(hull, "polygons") <- lapply(slot(hull, "polygons"), checkPolygonsGEOS2)
		}
	 
		while (is.null(hull) | inherits(hull, 'try-error') | !cleangeo::clgeo_IsValid(hull)) {
			alpha <- alpha + alphaIncrement
			if (verbose) {cat('\talpha:', alpha, '\n')}
			hull <- try(ah2sp(alphahull::ahull(data.frame(x),alpha=alpha), proj4string=CRS('+proj=longlat +datum=WGS84')), silent = TRUE)
			if (!is.null(hull)) {
				slot(hull, "polygons") <- lapply(slot(hull, "polygons"), checkPolygonsGEOS2)
			}
		}
	
		#how many points are within hull?
		slot(hull, "polygons") <- lapply(slot(hull, "polygons"), checkPolygonsGEOS2)
		pointWithin <- rgeos::gIntersects(x, hull, byid = TRUE)
	
		alphaVal <- alpha
		buffered <- FALSE
	
		while (any(length(hull@polygons[[1]]@Polygons) > partCount, length(which(pointWithin) == TRUE)/length(x) < fraction, !cleangeo::clgeo_IsValid(hull))) {
		    alpha <- alpha + alphaIncrement
		    if (verbose) {cat('\talpha:', alpha, '\n')}
		    hull <- try(alphahull::ahull(data.frame(x), alpha = alpha), silent = TRUE)
		    while (inherits(hull, 'try-error') & alpha <= alphaCap) {
		      alpha <- alpha + alphaIncrement
		      hull <- try(alphahull::ahull(data.frame(x),alpha = alpha), silent = TRUE)
		    }
			if (!inherits(hull, 'try-error')) {
				hull <- ah2sp(hull, proj4string = CRS('+proj=longlat +datum=WGS84'))
				hull <- sp::spTransform(hull, CRS("+init=epsg:3395"))
				if (cleangeo::clgeo_IsValid(hull)) {
					slot(hull, "polygons") <- lapply(slot(hull, "polygons"), checkPolygonsGEOS2)
					hull <- rgeos::gBuffer(hull, width = buff)
					hull <- sp::spTransform(hull, CRS(proj))
					buffered <- TRUE
					pointWithin <- rgeos::gIntersects(x, hull, byid = TRUE)
				}
			}
			alphaVal = alpha
			if (alpha > alphaCap) {
				hull <- rgeos::gConvexHull(x)
				hull <- sp::spTransform(hull, CRS("+init=epsg:3395"))
			    hull <- rgeos::gBuffer(hull, width = buff)
			    hull <- sp::spTransform(hull, CRS(proj))
				buffered <- TRUE
				alphaVal = 'MCH'
				break
			}
		}
	} else {
		hull <- rgeos::gConvexHull(x)
		hull <- sp::spTransform(hull, CRS("+init=epsg:3395"))
	    hull <- rgeos::gBuffer(hull, width = buff)
	    hull <- sp::spTransform(hull, CRS(proj))
		buffered <- TRUE
		alphaVal = 'MCH'
	}

	if (!buffered) {
		hull <- sp::spTransform(hull, CRS("+init=epsg:3395"))
		hull <- rgeos::gBuffer(hull, width = buff)
		hull <- sp::spTransform(hull, CRS(proj))	
	}
  
	if (clipToCoast != 'no') {
		# load built-in gshhs dataset
		data(gshhs, envir = environment())
		gshhs <- sp::spTransform(gshhs, CRS(proj4string(hull)))
		if (clipToCoast == 'terrestrial') {
			hull <- rgeos::gIntersection(hull, gshhs)
		} else {
			hull <- rgeos::gDifference(hull, gshhs)
		}
	}
  
	return(list(hull, alpha = paste('alpha', alphaVal, sep = '')))
}



#taken from the maptools package
checkPolygonsGEOS2 <- function(obj, properly = TRUE, force = TRUE, useSTRtree = FALSE) {
	if (!is(obj, "Polygons")) 
	    stop("not a Polygons object")
	comm <- try(rgeos::createPolygonsComment(obj), silent = TRUE)
	if (!inherits(comm, "try-error") && !force) {
	    comment(obj) <- comm
	    return(obj)
	}
	pls <- slot(obj, "Polygons")
	IDs <- slot(obj, "ID")
	n <- length(pls)
	if (n < 1) 
	    stop("Polygon list of zero length")
	uniqs <- rep(TRUE, n)
	if (n > 1) {
	    if (useSTRtree) 
	        tree1 <- rgeos::gUnarySTRtreeQuery(obj)
	    SP <- SpatialPolygons(lapply(1:n, function(i) Polygons(list(pls[[i]]), ID = i)))
	    for (i in 1:(n - 1)) {
	        if (useSTRtree) {
	            if (!is.null(tree1[[i]])) {
	              res <- try(rgeos::gEquals(SP[i, ], SP[tree1[[i]],], byid = TRUE), silent = TRUE)
	              if (inherits(res, "try-error")) {
	                warning("Polygons object ", IDs, ", Polygon ", i, ": ", res)
	                next
	              }
	              if (any(res)) {
	                uniqs[as.integer(rownames(res)[res])] <- FALSE
	              }
				}
	        }
	        else {
	            res <- try(rgeos::gEquals(SP[i, ], SP[uniqs,], byid = TRUE), silent = TRUE)
	            if (inherits(res, "try-error")) {
	              warning("Polygons object ", IDs, ", Polygon ", i, ": ", res)
	              next
	            }
	            res[i] <- FALSE
	            if (any(res)) {
	              wres <- which(res)
	              uniqs[wres[wres > i]] <- FALSE
	            }
	        }
	    }
	}
	if (any(!uniqs)) 
	    warning(paste("Duplicate Polygon objects dropped:", paste(wres, collapse = " ")))
	pls <- pls[uniqs]
	n <- length(pls)
	if (n < 1) 
	    stop("Polygon list of zero length")
	if (n == 1) {
	    oobj <- Polygons(pls, ID = IDs)
	    comment(oobj) <- rgeos::createPolygonsComment(oobj)
	    return(oobj)
	}
	areas <- sapply(pls, slot, "area")
	pls <- pls[order(areas, decreasing = TRUE)]
	oholes <- sapply(pls, function(x) slot(x, "hole"))
	holes <- rep(FALSE, n)
	SP <- SpatialPolygons(lapply(1:n, function(i) Polygons(list(pls[[i]]), ID = i)))
	if (useSTRtree) 
	    tree2 <- rgeos::gUnarySTRtreeQuery(SP)
	for (i in 1:(n - 1)) {
	    if (useSTRtree) {
	        if (!is.null(tree2[[i]])) {
	            if (properly) 
	              res <- rgeos::gContainsProperly(SP[i, ], SP[tree2[[i]], ], byid = TRUE)
	            else res <- rgeos::gContains(SP[i, ], SP[tree2[[i]], ], byid = TRUE)
	        }
	        else {
	            res <- FALSE
	        }
	    }
	    else {
	        if (properly) 
	            res <- rgeos::gContainsProperly(SP[i, ], SP[-(1:i), ], byid = TRUE)
	        else res <- rgeos::gContains(SP[i, ], SP[-(1:i), ], byid = TRUE)
	    }
	    wres <- which(res)
	    if (length(wres) > 0L) {
	        nres <- as.integer(rownames(res))
	        holes[nres[wres]] <- !holes[nres[wres]]
	    }
	}
	for (i in 1:n) {
	    if (oholes[i] != holes[i]) 
	        pls[[i]] <- Polygon(slot(pls[[i]], "coords"), hole = holes[i])
	}
	oobj <- Polygons(pls, ID = IDs)
	comment(oobj) <- rgeos::createPolygonsComment(oobj)
	oobj
}






