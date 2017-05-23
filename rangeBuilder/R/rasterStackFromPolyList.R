
rasterStackFromPolyList <- function(polyList, resolution = 50000, retainSmallRanges = TRUE, extent = 'auto', nthreads = 1) {
	
	if (class(polyList) == 'list') {
		if (!class(polyList[[1]]) %in% c('SpatialPolygons', 'SpatialPolygonsDataFrame')) {
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
				if ('try-error' %in% class(presenceCells)) {
					counter <- 1
					while ('try-error' %in% class(presenceCells) & counter < 10) {
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

