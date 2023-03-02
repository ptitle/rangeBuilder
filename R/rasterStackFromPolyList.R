##' Polygon List to rasterStack
##' 
##' Takes a list of polygons and creates a multi-layer SpatRaster.
##' 
##' In the rasterization process, all cells for which the polygon covers the
##' midpoint are considered as present and receive a value of 1. If
##' \code{retainSmallRanges = FALSE}, then species whose ranges are so small
##' that no cell registers as present will be dropped. If
##' \code{retainSmallRanges = TRUE}, then the cells that the small polygon is
##' found in will be considered as present.
##' 
##' @param polyList a list of spatial polygon objects, named with taxon names. 
##' It is assumed that all items in last have same crs.
##' @param resolution vertical and horizontal size of raster cell, in units of
##' the polygons' projection
##' @param retainSmallRanges boolean; should small ranged species be dropped or
##' preserved. See details.
##' @param extent if 'auto', then the maximal extent of the polygons will be
##' used.  If not auto, must be a numeric vector of length 4 with minLong,
##' maxLong, minLat, maxLat.
##' @return an object of class \code{SpatRaster} where all rasters contain
##' values of either NA or 1.
##' @author Pascal Title
##' @examples
##' 
##' 
##' \dontrun{
##' data(crotalus)
##' library(sf)
##' library(terra)
##' 
##' # get 10 species occurrence sets
##' uniqueSp <- split(crotalus, crotalus$genSp)
##' uniqueSp <- lapply(uniqueSp, function(x) 
##' 	x[!duplicated(x[, c('decimallongitude', 'decimallatitude')]),])
##' uniqueSp <- names(uniqueSp[sapply(uniqueSp, nrow) > 5])
##' uniqueSp <- uniqueSp[1:10]
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
##' # Create a SpatRaster with the extent inferred from the polygons, and a cell
##' # resolution of 0.2 degrees.
##' # cells with the presence of a species get a value of 1, NA if absent. 
##' 
##' rangeStack <- rasterStackFromPolyList(ranges, resolution = 0.2)
##' 
##' # calculate species richness per cell, where cell values are counts of species
##' richnessRaster <- app(rangeStack, fun=sum, na.rm = TRUE)
##' 
##' # set values of 0 to NA
##' richnessRaster[richnessRaster == 0] <- NA
##' 
##' #plot
##' ramp <- colorRampPalette(c('blue','yellow','red'))
##' plot(richnessRaster, col=ramp(100))
##' 
##' # to add a basic coastline, you can use the internal map
##' # world <- rangeBuilder:::loadWorldMap()
##' # plot(world, add = TRUE, lwd = 0.5)

##' 
##' }
##' 
##' @export

rasterStackFromPolyList <- function(polyList, resolution = 50000, retainSmallRanges = TRUE, extent = 'auto') {
	
	if (is.list(polyList)) {
		if (!any(inherits(polyList[[1]], c('SpatialPolygons', 'SpatialPolygonsDataFrame', 'sf', 'sfc')))) {
			stop('Input must be a list of spatial objects.')
		}
	}

	if (is.null(names(polyList))) {
		stop('List must be named with species names.')
	}

	# test that all have same CRS
	# check at least that all are either projected or unprojected
	if (!sum(sapply(polyList, sf::st_is_longlat)) %in% c(0, length(polyList))) {
		stop('Some polygons are projected, some unprojected.')
	}
	
	if (sf::st_is_longlat(polyList[[1]])) {
		proj <- sf::st_crs(4326)
	} else {
		proj <- sf::st_crs(polyList[[1]])
	}

	# if data are unprojected, then catch likely mistake where resolution is specified in meters.
	## (data would be in decimal degrees, and resolution would be in the 100s or 1000s)
	# also, if data are projected, and resolution is < 90, then likely a mistake as well.
	if (sf::st_is_longlat(proj) & resolution > 90) {
		stop("Input data are in longlat, therefore resolution must be in decimal degrees.")
	}
	if (!sf::st_is_longlat(proj) & resolution < 90) {
		stop('Input data are projected, but resolution does not seem to be in the same units.')
	}

	toDrop <- c()
	for (i in 1:length(polyList)) {
		if (inherits(polyList[[i]], 'sf')) {
			num <- nrow(polyList[[i]])
		} else {
			num <- length(polyList[[i]])
		}
		if (num == 0) {
			toDrop <- c(toDrop, i)
		}
	}
	if (length(toDrop) > 0) {
		message('\tempty inputs detected... Dropping list items: ', paste0(toDrop, collapse = ', '))
		polyList <- polyList[setdiff(1:length(polyList), toDrop)]
	}
	
	if ('auto' %in% extent) {
		#get overall extent
		masterExtent <- getExtentOfList(polyList)
	} else if (is.numeric(extent) & length(extent) == 4) {
		masterExtent <- sf::st_bbox(polyList[[1]])
		masterExtent['xmin'] <- extent[1]
		masterExtent['xmax'] <- extent[2]
		masterExtent['ymin'] <- extent[3]
		masterExtent['ymax'] <- extent[4]
	} else {
		stop("extent must be 'auto' or a vector with minLong, maxLong, minLat, maxLat.")
	}

	#create template raster
	ras <- terra::rast(xmin = masterExtent['xmin'], xmax = masterExtent['xmax'], ymin = masterExtent['ymin'], ymax = masterExtent['ymax'], resolution = rep(resolution, 2), crs = proj$input)
	
	rasList <- pbapply::pblapply(polyList, function(x) terra::rasterize(terra::vect(x), ras))
	
	# force non-NA values to be 1
	for (i in 1:length(rasList)) {
		terra::values(rasList[[i]])[!is.na(terra::values(rasList[[i]]))] <- 1
	}
	
	ret <- terra::rast(rasList)
	
	# if user wants to retain species that would otherwise be dropped
	# sample some random points in the range and identify cells
	valCheck <- terra::minmax(ret)[1,]
	smallSp <- which(is.na(valCheck))

	if (retainSmallRanges) {
		
		if (length(smallSp) > 0) {
			for (i in 1:length(smallSp)) {
				try(presenceCells <- unique(terra::cellFromXY(ret[[smallSp[i]]], sf::st_coordinates(sf::st_sample(polyList[[smallSp[i]]], 10, type = 'random')))), silent = TRUE)
				if (inherits(presenceCells, 'try-error')) {
					counter <- 1
					while (inherits(presenceCells, 'try-error') & counter < 10) {
						try(presenceCells <- unique(terra::cellFromXY(ret[[smallSp[i]]], sf::st_coordinates(sf::st_sample(polyList[[smallSp[i]]], 10, type = 'random')))), silent = TRUE)
					}
				}
				ret[[smallSp[i]]][presenceCells] <- 1
			}
		}
		
	} else {
		
		# drop those species with no presences (due to small range size)
		ret <- ret[[setdiff(1:terra::nlyr(ret), smallSp)]]
	}

	return(ret)
}	

