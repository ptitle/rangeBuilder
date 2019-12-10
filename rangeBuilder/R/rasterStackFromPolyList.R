##' Polygon List to rasterStack
##' 
##' Takes a list of polygons and creates a rasterStack.
##' 
##' In the rasterization process, all cells for which the polygon covers the
##' midpoint are considered as present and receive a value of 1. If
##' \code{retainSmallRanges = FALSE}, then species whose ranges are so small
##' that no cell registers as present will be dropped. If
##' \code{retainSmallRanges = TRUE}, then the cells that the small polygon is
##' found in will be considered as present.
##' 
##' @param polyList a list of SpatialPolygon objects, named with taxon names
##' @param resolution vertical and horizontal size of raster cell, in units of
##' the polygons' projection
##' @param retainSmallRanges boolean; should small ranged species be dropped or
##' preserved. See details.
##' @param extent if 'auto', then the maximal extent of the polygons will be
##' used.  If not auto, must be a numeric vector of length 4 with minLong,
##' maxLong, minLat, maxLat.
##' @param nthreads number of threads to use for parallelization of the
##' function. The R package \code{parallel} must be loaded for \code{nthreads >
##' 1}.
##' @return an object of class \code{RasterStack} where all rasters contain
##' values of either NA or 1.
##' @author Pascal Title
##' @examples
##' 
##' 
##' \dontrun{
##' data(crotalus)
##' 
##' # standardize species names
##' crotalus$genSp <- synonymMatch(crotalus$genSp, db='squam')
##' 
##' # get 10 species occurrence sets
##' uniqueSp <- unique(crotalus$genSp)[1:10]
##' uniqueSp <- uniqueSp[complete.cases(uniqueSp)]
##' 
##' # create range polygons
##' ranges <- vector('list', length = length(uniqueSp))
##' for (i in 1:length(uniqueSp)) {
##' 	x <- crotalus[which(crotalus$genSp == uniqueSp[i]),]
##' 
##' 	ranges[[i]] <- getDynamicAlphaHull(x, coordHeaders = c('decimallongitude', 
##' 		'decimallatitude'), clipToCoast = 'terrestrial')
##' }
##' 
##' # name the polygons
##' names(ranges) <- uniqueSp
##' 
##' # keep only the polygons
##' ranges <- lapply(ranges, function(x) x[[1]])
##' 
##' # Create a rasterStack with the extent inferred from the polygons, and a cell
##' # resolution of 0.2 degrees.
##' # cells with the presence of a species get a value of 1, NA if absent. 
##' 
##' rangeStack <- rasterStackFromPolyList(ranges, resolution = 0.2)
##' 
##' # calculate species richness per cell, where cell values are counts of species
##' richnessRaster <- calc(rangeStack, fun=sum, na.rm = TRUE)
##' 
##' # set values of 0 to NA
##' richnessRaster[richnessRaster == 0] <- NA
##' 
##' #plot
##' ramp <- colorRampPalette(c('blue','yellow','red'))
##' plot(richnessRaster, col=ramp(100))
##' 
##' plot(gshhs, add = TRUE, lwd=0.5)
##' 
##' }
##' 
##' @export

rasterStackFromPolyList <- function(polyList, resolution = 50000, retainSmallRanges = TRUE, extent = 'auto', nthreads = 1) {
	
	if (is.list(polyList)) {
		if (!any(inherits(polyList[[1]], c('SpatialPolygons', 'SpatialPolygonsDataFrame')))) {
			stop('Input must be a list of SpatialPolygons or a RasterStack.')
		}
	}

	if (is.null(names(polyList))) {
		stop('List must be named with species names.')
	}

	# test that all have same CRS
	if (length(unique(sapply(polyList, sp::proj4string))) != 1) {
		stop('proj4string of all polygons must match.')
	}
	proj <- sp::proj4string(polyList[[1]])

	if ('auto' %in% extent) {
		#get overall extent
		masterExtent <- getExtentOfList(polyList)
		masterExtent <- list(minLong = masterExtent@xmin, maxLong = masterExtent@xmax, minLat = masterExtent@ymin, maxLat = masterExtent@ymax)
	} else if (is.numeric(extent) & length(extent) == 4) {
		masterExtent <- list(minLong = extent[1], maxLong = extent[2], minLat = extent[3], maxLat = extent[4])
	} else {
		stop("extent must be 'auto' or a vector with minLong, maxLong, minLat, maxLat.")
	}

	#create template raster
	ras <- raster::raster(xmn = masterExtent$minLong, xmx = masterExtent$maxLong, ymn = masterExtent$minLat, ymx = masterExtent$maxLat, resolution = rep(resolution, 2), crs = proj)
	
	if (nthreads > 1) {
		cl <- parallel::makePSOCKcluster(nthreads)
		parallel::clusterExport(cl = cl, varlist = c('polyList', 'ras', 'rasterize'), envir = environment())
		rasList <- pbapply::pblapply(polyList, function(x) raster::rasterize(x, ras), cl = cl)
		parallel::stopCluster(cl)
	} else {
		rasList <- pbapply::pblapply(polyList, function(x) raster::rasterize(x, ras))
	}
	
	# force non-NA values to be 1
	for (i in 1:length(rasList)) {
		raster::values(rasList[[i]])[!is.na(raster::values(rasList[[i]]))] <- 1
	}
	
	ret <- raster::stack(rasList)
	
	# if user wants to retain species that would otherwise be dropped
	# sample some random points in the range and identify cells
	valCheck <- raster::minValue(ret)
	smallSp <- which(is.na(valCheck))

	if (retainSmallRanges) {
		
		if (length(smallSp) > 0) {
			for (i in 1:length(smallSp)) {
				try(presenceCells <- unique(raster::cellFromXY(ret[[smallSp[i]]], sp::spsample(polyList[[smallSp[i]]], 10, type = 'random'))), silent = TRUE)
				if (inherits(presenceCells, 'try-error')) {
					counter <- 1
					while (inherits(presenceCells, 'try-error') & counter < 10) {
						try(presenceCells <- unique(raster::cellFromXY(ret[[smallSp[i]]], sp::spsample(polyList[[smallSp[i]]], 10, type = 'random'))), silent = TRUE)
					}
				}
				ret[[smallSp[i]]][presenceCells] <- 1
			}
		}
		
	} else {
		
		# drop those species with no presences (due to small range size)
		ret <- ret[[setdiff(1:raster::nlayers(ret), smallSp)]]
	}

	return(ret)
}	

