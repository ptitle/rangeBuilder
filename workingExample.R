
# # NOT RUN
library(parallel)
data(crotalus)
data(gshhs)

ncores <- 2

# We will use the various funtions in this package to first filter occurrences, and address
# synonomy issues.

# Remove points that fall in ocean
onLand <- filterByLand(crotalus[,c('decimallongitude','decimallatitude')], returnGood = TRUE)
crotalus <- crotalus[onLand,]

# Match synonyms
match <- synonymMatch(crotalus$genSp, db = 'squamates', nthreads = ncores)

# We will leave out records for species that found no match
crotalus <- cbind(crotalus, match = match)
crotalus <- crotalus[which(!is.na(crotalus$match)),]
crotalus$match <- as.character(crotalus$match)

# Use Reptile DB's country listings to filter species records
## standardize countries
crotalus <- cbind(crotalus, 
	matchedCountries=standardizeCountry(crotalus$country, nthreads = ncores))
crotalus$matchedCountries <- as.character(crotalus$matchedCountries)

#for each record, do the coordinates actually fall in the listed country?
#for records with no country, fill that country in
#if returned country does not match country, check for sign errors with flipSign
pb <- txtProgressBar(min = 0, max = nrow(crotalus), style = 3)
for (i in 1:nrow(crotalus)) {
	setTxtProgressBar(pb, i)
	x <- closestCountry(crotalus[i, c('decimallongitude','decimallatitude')])
	if (!crotalus$matchedCountries[i] %in% x & crotalus$matchedCountries[i] != '') {
		sign <- flipSign(crotalus[i, c('decimallongitude','decimallatitude')], 
			country = crotalus$matchedCountries[i])
		if (sign$matched == TRUE) {
			crotalus[i, c('decimallongitude', 'decimallatitude')] <- as.numeric(sign$newcoords)
		}
	} else if (crotalus$matchedCountries[i] == '') {
		crotalus$matchedCountries[i] <- x[1]
	}
}

#get list of countries for each species
spCountries <- getRepDBcountryList(unique(crotalus$match))

# remove countries that are designated as invasive according to the
# global invasive species database
gisd <- lapply(unique(crotalus$match), function(x) queryGISD(x))
names(gisd) <- unique(crotalus$match)
# Crotalus species not present in GISD

# filter according to these country lists
crotalus <- split(crotalus, crotalus$match)
spCountries <- spCountries[names(crotalus)]
for (i in 1:length(crotalus)) {
	crotalus[[i]] <- crotalus[[i]][crotalus[[i]]$matchedCountries %in% spCountries[[i]],]
}

# range creation requires at least 3 unique occurrences.
occCount <- sapply(crotalus, function(x) {
	nrow(x[!duplicated(x[,c('decimallongitude','decimallatitude')]),]))
})
crotalus <- crotalus[which(occCount > 3)]

#thin to speed up testing
crotalus <- lapply(crotalus, function(x) {
	if (nrow(x) > 200) {
		x <- x[sample(1:nrow(x), 200),]
	}
	return(x)
})

# generate range polygons
ranges <- vector('list', length=length(crotalus))
for (i in 1:length(crotalus)) {
	cat(i, '\n')
	ranges[[i]] <- getDynamicAlphaHull(crotalus[[i]], 
		coordHeaders=c('decimallongitude','decimallatitude'), partCount = 1)
}

# the first list item for each is the actual polygon
ranges <- lapply(ranges, function(x) x[[1]])

# create rasters and map richness
# We will use an extent of the Americas
bb <- c(-133, -32, -55, 50)
r <- richnessRaster(ranges, resolution = 0.5, extent = bb, nthreads = ncores)

#plot
plot(r, legend = FALSE)
plot(gshhs, add = TRUE)
addRasterLegend(r, location = c(-120, -118, -40, 0), border = TRUE)
