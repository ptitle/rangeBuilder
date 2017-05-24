# Function to filter occurrences by land/ocean

filterByLand <- function(coords, proj = '+proj=longlat +datum=WGS84') {

	# if vector, convert to matrix
	if (is.null(dim(coords))) {
		coords <- matrix(coords, nrow = 1, ncol = 2)
	}

	if (class(coords) == 'data.frame') {
		coords <- as.matrix(coords)
	}

	if (class(coords) %in% c('SpatialPoints', 'SpatialPointsDataFrame')) {
		
		if (is.na(proj4string(coords))) {
			stop('proj4string must be specified for spatial input.')
		}

		# transform if needed
		if (proj4string(worldRaster) != proj) {
			coords <- sp::spTransform(coords, CRS(proj4string(worldRaster)))
		}

		# convert to matrix
		coords <- as.matrix(as.data.frame(coords))
	}

	if (class(coords) == 'matrix' & proj4string(worldRaster) != proj) {

		#transform
		coords <- SpatialPoints(coords, CRS(proj))
		coords <- sp::spTransform(coords, CRS(proj4string(worldRaster)))
		coords <- as.matrix(as.data.frame(coords))
	}

	if (class(coords) != 'matrix' | mode(coords) != 'numeric') {
		stop('coords must be a numeric matrix.')
	}

	#extract worldRaster values
	e <- extract(worldRaster, coords)
	e <- ifelse(is.na(e), FALSE, TRUE)

	return(e)	
}