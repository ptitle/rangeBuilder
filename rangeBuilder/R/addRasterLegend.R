##' @title addRasterLegend
##'
##' @description Adds a legend to an existing raster plot, with some additional manual control

##' @param r the rasterLayer object that has been plotted
##' @param direction direction of color ramp. If omitted, then direction is automatically 
##'	 inferred, otherwise can be specified as \code{horizontal} or \code{vertical}.
##' @param side side for tick marks, see \code{\link{axis}} documentation. 
##'		Automatically inferred if omitted.
##' @param location either a location name (see \code{Details}), or coordinates for 
##'		the corners of the bar legend \code{c(xmin, xmax, ymin, ymax)}.
##' @param nTicks number of tick marks, besides min and max.
##' @param adj if location is top, left, bottom or right, use this argument to adjust the 
##'		location of the legend, defined in percent of the figure width. See Details 
##'		for additional information. 
##' @param shortFrac Percent of the plot width range that will be used as the short 
##' 	dimention of the legend. Only applies to preset location options.
##' @param longFrac Percent of the plot width range that will be used as the long dimention 
##'		of the legend. Only applies to preset location options.	
##' @param axisOffset distance from color bar for labels, as a percent of the plot width.
##' @param border logical, should the color legend have a black border
##' @param ramp either a vector of color names for defining the color ramp, 
##'		or "terrain" (default raster behavior)
##' @param isInteger If \code{auto}, automatically determines if raster is made up of integer 
##'		values, otherwise \code{TRUE} or \code{FALSE}
##' @param ncolors grain size of color ramp
##' @param breaks If a custom set of color breaks were used in plotting the raster, pass 
##'		those color breaks here. This overrides the minmax option. 
##' @param minmax min and max values from which the color ramp will be derived. If left
##' 	as \code{NULL}, the min and max of the raster will be used.
##' @param locs locations of tick marks, if \code{NULL} automatically placed
##' @param cex.axis size of axis labels
##' @param labelDist distance from axis to axis labels (passed to \code{mgp})
##' @param digits number of decimal places for labels
##' @param bigmark character used to separate thousands and millions, passed 
##' 	to \code{\link{format}}
##' @param ... additional parameters to be passed to \code{\link{axis}}.

##' @details
##'		A number of predefined locations exist in this function to make it easy to 
##'		add a legend to a raster plot. 
##'		Preset \code{locations} are: \code{topleft}, \code{topright}, \code{bottomleft}, 
##'		\code{bottomright}, \code{left}, \code{right}, \code{top} and \code{bottom}. 
##'		If more fine-tuned control is desired, then a numeric vector of length 4 can be 
##'		supplied to \code{location}, specifying the min x, max x, min y and max y 
##'		values for the legend. 
##'		Additionally, the \code{adj} argument can be used to more intuitively adjust where
##' 	the legend is placed. \code{adj} is defined as a percentage of the figure width or 
##' 	height, left to right, or bottom to top, respectively. For example, if the legend is at 
##' 	the bottom, \code{adj = 0.8} will place the legend 80% of the distance from the top of 
##' 	the figure, horizontally centered.  
##'		See examples.

##' @return Invisibly returns a list with the following components.
##' \itemize{
##'	\item{coords}{2-column matrix of xy coordinates for each color bin in the legend.}
##'	\item{width}{Coordinates for the short dimension of the legend.}
##'	\item{pal}{the color ramp}
##'	\item{tickLocs}{the tick mark locations in plotting units}
##'	\item{labels}{the values associated with those tick locations.}
##' }

##' @author Pascal Title

##' @examples
##' library(raster)
##' r <- raster(system.file("external/test.grd", package="raster"))
##' 
##' plot(r, legend = FALSE)
##' addRasterLegend(r, location = 'right')
##' addRasterLegend(r, location = 'top')
##' 
##' #fine-tune placement
##' plot(r, legend = FALSE)
##' addRasterLegend(r, location=c(181000, 181100, 330500, 331500), side = 4)
 
##' @export


addRasterLegend <- function(r, direction, side, location = 'right', nTicks = 2, adj = NULL, shortFrac = 0.02, longFrac = 0.3, axisOffset = 0, border = TRUE, ramp = "terrain", isInteger = 'auto', ncolors = 64, breaks = NULL, minmax = NULL, locs = NULL, cex.axis = 0.8, labelDist = 0.7, digits = 2, bigmark = '', ...) {
		
	if (inherits(r, 'speciesRaster')) {
		r <- r[[1]]
	}
	
	if (!inherits(r, 'RasterLayer')) {
		stop("r must be a speciesRaster object or RasterLayer.");
	}
		
	if(!methods::hasArg('direction')) {
		direction <- 'auto'
	}
	
	if (!direction %in% c('auto', 'vertical', 'horizontal')) {
		stop("direction must be auto, vertical or horizontal.");
	}
	
	if (is.character(location)) {
		if (!location %in% c('bottomleft','bottomright','topleft','topright','bottom','top','left','right')) {
			stop('location is not recognized.');
		}
	}

	if (!isInteger %in% c('auto', TRUE, FALSE)) {
		stop('isInteger must be "auto", TRUE or FALSE.')
	}
	
	if(!methods::hasArg('ramp')) {
		pal <- rev(grDevices::terrain.colors(ncolors));
	} else {
		if (inherits(ramp, 'function')) {
			pal <- ramp(ncolors)
		} else {
			pal <- grDevices::colorRampPalette(ramp)(ncolors)
		}
	}
	
	#if minmax provided, use to generate linear color breaks
	if (is.null(minmax)) {
		colorbreaks <- seq(raster::minValue(r), raster::maxValue(r), length.out = (ncolors+1));
	} else {
		colorbreaks <- seq(minmax[1], minmax[2], length.out = (ncolors + 1));
	}
	
	#if supplied, use custom set of breaks
	if (!is.null(breaks)) {
		colorbreaks <- breaks;
		ncolors <- length(breaks) + 1;
	}
	
	n <- length(colorbreaks);
	
	#return plot region extremes and define outer coordinates
	minX <- grconvertX(par('fig')[1], from = 'ndc', to = 'user') 
	maxX <- grconvertX(par('fig')[2], from = 'ndc', to = 'user')
	minY <- grconvertY(par('fig')[3], from = 'ndc', to = 'user')
	maxY <- grconvertY(par('fig')[4], from = 'ndc', to = 'user')
	
	xrange <- maxX - minX
	yrange <- maxY - minY
	minX <- minX + xrange * 0.05
	maxX <- maxX - xrange * 0.05
	minY <- minY + yrange * 0.05
	maxY <- maxY - yrange * 0.05
	
	if (is.character(location)) {
		
		locChar <- location
	
		if (location == 'topleft' & direction %in% c('auto', 'vertical')) {
			location <- vector('numeric', length = 4);
			location[1] <- minX
			location[2] <- minX + (maxX - minX) * shortFrac
			location[3] <- maxY - (maxY - minY) * longFrac
			location[4] <- maxY
		} else
		
		if (location == 'topleft' & direction == 'horizontal') {
			location <- vector('numeric', length = 4);
			location[1] <- minX
			location[2] <- minX + (maxX - minX) * longFrac
			location[3] <- maxY - (maxY - minY) * shortFrac
			location[4] <- maxY
		} else
	
		if (location == 'topright' & direction %in% c('auto', 'vertical')) {
			location <- vector('numeric', length = 4);
			location[1] <- maxX - (maxX - minX) * shortFrac
			location[2] <- maxX
			location[3] <- maxY - (maxY - minY) * longFrac
			location[4] <- maxY
		} else
	
		if (location == 'topright' & direction == 'horizontal') {
			location <- vector('numeric', length = 4);
			location[1] <- maxX - (maxX - minX) * longFrac
			location[2] <- maxX
			location[3] <- maxY - (maxY - minY) * shortFrac
			location[4] <- maxY
		} else
	
		if (location == 'bottomleft' & direction %in% c('auto', 'vertical')) {
			location <- vector('numeric', length = 4);
			location[1] <- minX
			location[2] <- minX + (maxX - minX) * shortFrac
			location[3] <- minY
			location[4] <- minY + (maxY - minY) * longFrac
		} else
		
		if (location == 'bottomleft' & direction == 'horizontal') {
			location <- vector('numeric', length = 4);
			location[1] <- minX
			location[2] <- minX + (maxX - minX) * longFrac
			location[3] <- minY
			location[4] <- minY + (maxY - minY) * shortFrac
		} else
	
		if (location == 'bottomright' & direction %in% c('auto', 'vertical')) {
			location <- vector('numeric', length = 4);
			location[1] <- maxX - (maxX - minX) * shortFrac
			location[2] <- maxX
			location[3] <- minY
			location[4] <- minY + (maxY - minY) * longFrac
		} else
		
		if (location == 'bottomright' & direction == 'horizontal') {
			location <- vector('numeric', length = 4);
			location[1] <- maxX - (maxX - minX) * longFrac
			location[2] <- maxX
			location[3] <- minY
			location[4] <- minY + (maxY - minY) * shortFrac 
		} else
		
		if (location == 'left') {
			location <- vector('numeric', length = 4);
			location[1] <- minX
			location[2] <- minX + (maxX - minX) * shortFrac
			location[3] <- mean(par('usr')[3:4]) - ((maxY - minY) * longFrac)/2
			location[4] <- mean(par('usr')[3:4]) + ((maxY - minY) * longFrac)/2
			direction <- 'vertical'
		} else
	
		if (location == 'right') {
			location <- vector('numeric', length = 4);
			location[1] <- maxX - (maxX - minX) * shortFrac
			location[2] <- maxX
			location[3] <- mean(par('usr')[3:4]) - ((maxY - minY) * longFrac)/2
			location[4] <- mean(par('usr')[3:4]) + ((maxY - minY) * longFrac)/2
			direction <- 'vertical'
		} else
		
		if (location == 'top') {
			location <- vector('numeric', length = 4);
			location[1] <- mean(par('usr')[1:2]) - ((maxX - minX) * longFrac)/2
			location[2] <- mean(par('usr')[1:2]) + ((maxX - minX) * longFrac)/2
			location[3] <- maxY - (maxY - minY) * shortFrac
			location[4] <- maxY
			direction <- 'horizontal'
		} else
	
		if (location == 'bottom') {
			location <- vector('numeric', length = 4);
			location[1] <- mean(par('usr')[1:2]) - ((maxX - minX) * longFrac)/2
			location[2] <- mean(par('usr')[1:2]) + ((maxX - minX) * longFrac)/2
			location[3] <- minY
			location[4] <- minY + (maxY - minY) * shortFrac
			direction <- 'horizontal'
		}
	}
	
	# if adj argument provided, use to adjust location
	# currently only implemented for locations bottom, left, top, right
	if (!is.null(adj)) {
		if (adj > 1) {
			stop('adj must be supplied as a percentage.')
		}
		if (locChar %in% c('top', 'bottom')) {
			Ydiff <- diff(location[3:4])
			location[3] <- minY + (maxY - minY) * adj
			location[4] <- location[3] + Ydiff
		} else if (locChar %in% c('left', 'right')) {
			Xdiff <- diff(location[1:2])
			location[1] <- minX + (maxX - minX) * adj
			location[2] <- location[1] + Xdiff			
		}
	}

	# infer direction based on dimensions of legend box
	if (direction == 'auto') {
		if (((location[2] - location[1]) / (par('usr')[2] - par('usr')[1])) >= ((location[4] - location[3]) / (par('usr')[4] - par('usr')[3]))) {
			direction <- 'horizontal';
		} else {
			direction <- 'vertical';
		}
	}

	if (direction == 'horizontal') {
		axisOffset <- axisOffset * (par('usr')[4] - par('usr')[3]);
	} else if (direction == 'vertical') {
		axisOffset <- axisOffset * (par('usr')[2] - par('usr')[1]);
	}
	
	#determine side for labels based on location in plot and direction
	if (!methods::hasArg('side')) {
		if (direction == 'vertical') { #side = 1 or 4
			if (mean(location[1:2]) <= mean(par('usr')[1:2])) {
				side <- 4;
			} else {
				side <- 2;
			}
		}
		if (direction == 'horizontal') { #side = 2 or 3
			if (mean(location[3:4]) > mean(par('usr')[3:4])) {
				side <- 1;
			} else {
				side <- 3;
			}
		}
	}

	if (direction == 'horizontal') {
		x <- seq(from = location[1], to = location[2], length.out = n);
		width <- location[3:4];
	} else {
		x <- seq(from = location[3], to = location[4], length.out = n);
		width <- location[1:2];
	}
	
	#get bin coordinates
	x <- rep(x,each = 2);
	x <- x[-c(1,length(x))];
	x <- matrix(x, ncol = 2, byrow = TRUE);
	
	#find tick locations
	#get equivalent color bins
	z <- rep(colorbreaks,each = 2);
	z <- z[-c(1,length(z))];
	z <- matrix(z, ncol = 2, byrow = TRUE);

	#if tick locations are supplied, use them, otherwise generate regularly spaced tick locations
	if (!is.null(locs)) {
		# if max value is included, add in manually
		tol <- 1e-10
		maxValueIncluded <- FALSE
		if (any(abs(raster::maxValue(r) - locs) < tol)) {
			locs <- locs[which(abs(raster::maxValue(r) - locs) >= tol)]
			maxValueIncluded <- TRUE
		}
		tickLocs <- x[sapply(locs, function(x) which((x >= z[,1] & x < z[,2]) == TRUE)),1];
		if (maxValueIncluded) {
			tickLocs <- c(tickLocs, max(x[,2]));
			locs <- c(locs, max(colorbreaks))
		}
		tx <- locs;
	} else {
		tx <- trunc(seq(from = 1, to = nrow(x), length.out = nTicks + 2));
		tickLocs <- x[tx,1];
		tx <- z[tx,1];
		tickLocs[length(tickLocs)] <- max(x[,2]);
		tx[length(tx)] <- max(z[,2]);
	}
	
	#if raster values are integer, then make legend have integer values
	if (isInteger == 'auto') {
		randomSample <- sample(raster::values(r)[which(!is.na(raster::values(r)))], size=1000, replace = TRUE)
		if (identical(randomSample, trunc(randomSample))) {
			tx <- round(tx, 0)
		}
	} else if (isInteger) {
		tx <- round(tx, 0)
	}
	
	#plot bar
	if (direction == 'horizontal') {
		rect(xleft = x[,1], ybottom = width[1], xright = x[,2], ytop = width[2], border = pal, col = pal, xpd = NA);
	} else {
		rect(xleft = width[1], ybottom = x[,1], xright = width[2], ytop = x[,2], border = pal, col = pal, xpd = NA);
	}
	
	if (border) {
		rect(location[1], location[3], location[2], location[4], border='black', xpd = NA);
	}
	
	if (bigmark != '') {
		tickLabels <- format(tx, big.mark = bigmark)
	} else {	
		tickLabels <- round(tx, digits)
	}
	
	#add tickmarks
	if (side == 1) { #bottom
		axis(side, at = tickLocs, pos = location[3] - axisOffset, labels = tickLabels, xpd = NA, las = 1, cex.axis = cex.axis, mgp = c(3, labelDist, 0), ...);
	} 
	if (side == 3) { #top
		axis(side, at = tickLocs, pos = location[4] + axisOffset, labels = tickLabels, xpd = NA, las = 1, cex.axis = cex.axis, mgp = c(3, labelDist, 0), ...);
	}
	if (side == 2) { #left
		axis(side, at = tickLocs, pos = location[1] - axisOffset, labels = tickLabels, xpd = NA, las = 1, cex.axis = cex.axis, mgp = c(3, labelDist, 0), ...);
	}
	if (side == 4) { #right
		axis(side, at = tickLocs, pos = location[2] + axisOffset, labels = tickLabels, xpd = NA, las = 1, cex.axis = cex.axis, mgp = c(3, labelDist, 0), ...);
	}
}
	
	

	

