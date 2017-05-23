

closestCountry <- function(pt, proj = "+proj=longlat +datum=WGS84") {

	# if spatial object
	if (class(pt) %in% c('SpatialPoints', 'SpatialPointsDataFrame')) {
		proj <- proj4string(pt)
		if (is.na(proj)) {
			stop('If pt is a SpatialPoints object, it must have a proj4string.')
		}
		pt <- coordinates(pt)
	}

	# check that provided proj4string is valid
	CRS(proj)

	if (proj != "+proj=longlat +datum=WGS84") {
		pt <- SpatialPoints(pt, proj4string=CRS(proj))
		pt <- spTransform(pt, CRS('+proj=longlat +datum=WGS84'))
	}
		
	# if single row table, convert to vector
	if (is.matrix(pt) | is.data.frame(pt)) {
		if (nrow(pt) == 1) {
			pt <- as.numeric(pt)
		}
	}

	# if multiple points
	if (is.matrix(pt) | is.data.frame(pt)) {
		w <- shortDistInd(as.matrix(pt), as.matrix(worldPoints))
		ret <- worldPointCountries[w]
		names(ret) <- NULL
		return(ret)
	} else {
		d <- spDistsN1(worldPoints, pt, longlat = FALSE)
		w <- which.min(d)
		return(worldPointCountries[[w]])
	} 
}



# closestCountry <- function(pt, proj = "+proj=longlat +datum=WGS84", maxPts = 1000) {

	# # if spatial object
	# if (class(pt) %in% c('SpatialPoints', 'SpatialPointsDataFrame')) {
		# proj <- proj4string(pt)
		# if (is.na(proj)) {
			# stop('If pt is a SpatialPoints object, it must have a proj4string.')
		# }
		# pt <- coordinates(pt)
	# }

	# # check that provided proj4string is valid
	# CRS(proj)

	# if (proj != "+proj=longlat +datum=WGS84") {
		# pt <- SpatialPoints(pt, proj4string=CRS(proj))
		# pt <- spTransform(pt, CRS('+proj=longlat +datum=WGS84'))
	# }
		
	# # if single row table, convert to vector
	# if (is.matrix(pt) | is.data.frame(pt)) {
		# if (nrow(pt) == 1) {
			# pt <- as.numeric(pt)
		# }
	# }

	# # if multiple points
	# if (is.matrix(pt) | is.data.frame(pt)) {
		
		# # limit to maxPts records at a time to avoid memory overload
		# if (nrow(pt) > maxPts) {
			# blocks <- seq(1, nrow(pt), by = maxPts)
			# if (blocks[length(blocks)] != nrow(pt)) {
				# blocks <- c(blocks, nrow(pt))
			# }
			# ret <- numeric(nrow(pt))
			# for (i in 1:(length(blocks) - 1)) {
				# cat(i, '\n')
				# d <- fields::rdist(x1 = pt[blocks[i]:(blocks[i+1] - 1),], x2 = worldPoints)
				# ret[blocks[i]:(blocks[i+1] - 1)] <- apply(d, 1, which.min)
			# }
		
		# } else {
			# d <- fields::rdist(x1 = pt, x2 = worldPoints)
			# w <- apply(d, 1, which.min)
		# }
		# ret <- worldPointCountries[w]
		# names(ret) <- NULL
		# return(ret)
	# } else {
		# d <- spDistsN1(worldPoints, pt, longlat = FALSE)
		# w <- which.min(d)
		# return(worldPointCountries[[w]])
	# } 
# }





