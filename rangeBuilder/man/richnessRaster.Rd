\name{richnessRaster}
\alias{richnessRaster}
\title{
Map species richness
}
\description{
This function generates a raster of species richness from a set of polygons or rasters
}
\usage{
richnessRaster(ranges, resolution = 1, resUnits = 'degrees', 
	extent = 'auto', speciesByCell = FALSE, coverCutoff = 0.5, nthreads = 1)
}
\arguments{
	\item{ranges}{
		a RasterStack/RasterBrick or a list of SpatialPolygons
	}
	\item{resolution}{
		the size of the cells of the resulting raster (ignored if input is raster)
	}
	\item{resUnits}{
		if 'degrees', then raster will be unprojected, if 'meters' then raster will be 
		projected to equal area Behrmann projection (ignored if input is raster)
	}
	\item{extent}{
		if 'auto', then the maximal extent of the polygons will be used, if 
		input is raster, this will be ignored as the extent of the RasterStack will 
		be used. If not auto, must be a numeric vector of length 4 with minLong, 
		maxLong, minLat, maxLat. 
	}
	\item{speciesByCell}{
		if \code{TRUE}, rather than a raster, returns a list where each entry 
		is a raster cell, with a vector of species names that occur in that cell. 
	}
	\item{coverCutoff}{
		only applies if input is SpatialPolygons. In rasterization of the polygons,
		the percent of a raster cell that must be covered by the polygon for it to
		count as a presence. 
	}
	\item{nthreads}{
		number of threads to use for parallelization of the function. The R package
		\code{parallel} must be loaded for \code{nthreads > 1}.
	}
}

\details{
	This function generates a raster that represents the number of species ranges 
	that overlap each cell. If the input is \code{RasterStack}, presence is any 
	non-NA value in the input raster. If the input is \code{SpatialPolygons}, 
	presence is overlap of 50 percent or more of the cell (specified 
	with \code{coverCutoff}).

	If ranges are in SpatialPolygon format, it might be more convenient to have 
	SpatialPolygon input to this function, however the majority of the runtime 
	of this function is put towards rasterizing the polygons. Therefore, it is 
	preferable to first generate rasters, and then to provide a RasterStack
	to this function, as the most time-consuming step is done separately. 

	Control of resolution, projection and extent are all intended for
	SpatialPolygon input. If rasters are input, then these manipulations should
	be done beforehand. 
	
	This function expects that all input rasters have presence values (i.e., 
	non-NA values). If any rasters have entirely NA cells, then the function will stop
	with a warning, and the output will be the index in the rasterStack of those rasters.
}

\value{
	If \code{speciesByCell = FALSE}, \cr
	RasterLayer, where values represent counts of species. Counts of 0 are set to NA.
	If the function stopped due to all-NA rasters, then the output is a numeric vector of
	raster indices.


	If \code{speciesByCell = TRUE}, \cr
	list of vectors of species names: each entry is a raster cell, and the species are 
	those that are found in that cell. 
}
\author{
	Pascal Title
}

\examples{
## NOT RUN

# library(parallel)
# data(crotalus)
# data(gshhs)

# ncores <- 2

# # We will use the various funtions in this package to first filter occurrences, and address
# # synonomy issues.

# # Remove points that fall in ocean
# onLand <- filterByLand(crotalus[,c('decimallongitude','decimallatitude')], returnGood = TRUE)
# crotalus <- crotalus[onLand,]

# # Match synonyms
# match <- synonymMatch(crotalus$genSp, db = 'squamates', nthreads = ncores)

# # We will leave out records for species that found no match
# crotalus <- cbind(crotalus, match = match)
# crotalus <- crotalus[which(!is.na(crotalus$match)),]
# crotalus$match <- as.character(crotalus$match)

# # Use Reptile DB's country listings to filter species records
# ## standardize countries
# crotalus <- cbind(crotalus, 
#	matchedCountries=standardizeCountry(crotalus$country, nthreads = ncores))
# crotalus$matchedCountries <- as.character(crotalus$matchedCountries)

# #for each record, do the coordinates actually fall in the listed country?
# #for records with no country, fill that country in
# #if returned country does not match country, check for sign errors with flipSign
# pb <- txtProgressBar(min = 0, max = nrow(crotalus), style = 3)
# for (i in 1:nrow(crotalus)) {
	# setTxtProgressBar(pb, i)
	# x <- closestCountry(crotalus[i, c('decimallongitude','decimallatitude')])
	# if (!crotalus$matchedCountries[i] %in% x & crotalus$matchedCountries[i] != '') {
		# sign <- flipSign(crotalus[i, c('decimallongitude','decimallatitude')], 
			# country = crotalus$matchedCountries[i])
		# if (sign$matched == TRUE) {
			# crotalus[i, c('decimallongitude', 'decimallatitude')] 
			#	<- as.numeric(sign$newcoords)
		# }
	# } else if (crotalus$matchedCountries[i] == '') {
		# crotalus$matchedCountries[i] <- x[1]
	# }
# }

# #get list of countries for each species
# spCountries <- getRepDBcountryList(unique(crotalus$match))

# # remove countries that are designated as invasive according to the
# # global invasive species database
# gisd <- lapply(unique(crotalus$match), function(x) queryGISD(x))
# names(gisd) <- unique(crotalus$match)
# # Crotalus species not present in GISD

# # filter according to these country lists
# crotalus <- split(crotalus, crotalus$match)
# spCountries <- spCountries[names(crotalus)]
# for (i in 1:length(crotalus)) {
	# crotalus[[i]] <- crotalus[[i]][crotalus[[i]]$matchedCountries %in% spCountries[[i]],]
# }

# # range creation requires at least 3 unique occurrences.
# occCount <- sapply(crotalus, function(x) {
	# nrow(x[!duplicated(x[,c('decimallongitude','decimallatitude')]),])
# })
# crotalus <- crotalus[which(occCount > 3)]

# #thin to speed up testing
# crotalus <- lapply(crotalus, function(x) {
	# if (nrow(x) > 200) {
		# x <- x[sample(1:nrow(x), 200),]
	# }
	# return(x)
# })

# # generate range polygons
# ranges <- vector('list', length=length(crotalus))
# for (i in 1:length(crotalus)) {
	# cat(i, '\n')
	# ranges[[i]] <- getDynamicAlphaHull(crotalus[[i]], 
		# coordHeaders=c('decimallongitude','decimallatitude'), partCount = 1)
# }

# # the first list item for each is the actual polygon
# ranges <- lapply(ranges, function(x) x[[1]])

# # create rasters and map richness
# # We will use an extent of the Americas
# bb <- c(-133, -32, -55, 50)
# r <- richnessRaster(ranges, resolution = 0.5, extent = bb, nthreads = ncores)

# #plot
# plot(r, legend = FALSE)
# plot(gshhs, add = TRUE)
# addRasterLegend(r, location = c(-120, -118, -40, 0), border = TRUE)

}
 








