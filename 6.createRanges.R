# Read in species occurrences, create alpha hulls, plot

## INPUT / OUTPUT OPTIONS
### normal pipeline
splist <- readRDS('~/Dropbox/NWsnakes/v2/data/splist_seed2.rds')

# ## least inclusive: no-georef points at centroid-border
# splist <- readRDS('~/Dropbox/NWsnakes/v2/data/alternateRanges/splist_collapseToBorder.rds')
# outfile <- '~/Dropbox/NWsnakes/v2/data/alternateRanges/polyFromOcc_collapseToBorder.rds'

## least inclusive: no-georef points at centroid-border -- all sp
# splist <- readRDS('~/Dropbox/NWsnakes/v2/data/alternateRanges/splist_collapseToBorder_allsp.rds')
# outfile <- '~/Dropbox/NWsnakes/v2/data/alternateRanges/polyFromOcc_collapseToBorder_allsp.rds'

## most inclusive: sampling of entire stateProvince
# splist <- readRDS('~/Dropbox/NWsnakes/v2/data/alternateRanges/splist_FullState.rds')
# outfile <- '~/Dropbox/NWsnakes/v2/data/alternateRanges/polyFromOcc_fullState.rds'

# -----------------------

require(rangeBuilder)
require(maptools)
require(rgeos)
require(raster)
require(parallel)

nCores <- 6; # for parallel processing

##-----------------------
## CREATE SPECIES RANGES
##

#verify occurrence data
for (i in 1:length(splist)) {
	if (length(unique(splist[[i]]$match)) != 1 | splist[[i]]$match[1] != names(splist)[i]) {
		cat('PROBLEM:', i, '\n')
	}
}


# #parallel version
makeRange <- function(pts) {
	pts <- pts[,c('Longitude','Latitude')]
	pts <- pts[complete.cases(pts),]
	pts <- pts[!duplicated(pts),]
	
	if (nrow(pts) > 0) {
		pts <- filterByProximity(pts, dist=0.5)
		if (class(pts) == 'numeric') {
			pts <- as.data.frame(matrix(pts, nrow=1, ncol=2))
		}
		pts <- data.frame(filterByProximity(pts, dist=0.5)) #0.5 km
	}
	if (nrow(pts) > 5) {
		#alpha hull
		res <- getDynamicAlphaHull(pts, fraction=0.99, partCount = 1, buff=50000, clipToCoast = FALSE)
	}
	if (nrow(pts) <= 5 & nrow(pts) >= 3) {
		#convex hull
		res <- list(gBuffer(gConvexHull(SpatialPoints(pts)), width=0.5), method='MCH')
	}
	if (nrow(pts) > 0 & nrow(pts) < 3) {
		#buffered points
		res <- list(gBuffer(SpatialPoints(pts), width=1), method='pointBuff1deg')
	}
	if (nrow(pts) == 0) {
		res <- 'no points'
	}
	return(res)
}

makeRange2 <- function(pts) {
	pts <- pts[,c('Longitude','Latitude')]
	pts <- pts[complete.cases(pts),]
	pts <- pts[!duplicated(pts),]
	
	if (nrow(pts) > 0) {
		pts <- filterByProximity(pts, dist=0.5)
		if (class(pts) == 'numeric') {
			pts <- as.data.frame(matrix(pts, nrow=1, ncol=2))
		}
		pts <- data.frame(filterByProximity(pts, dist=0.5)) #0.5 km
	}
	if (nrow(pts) > 5) {
		#alpha hull
		res <- getDynamicAlphaHull2(pts, fraction=0.99, partCount = 1, buff=50000, clipToCoast = FALSE)
	}
	if (nrow(pts) <= 5 & nrow(pts) >= 3) {
		#convex hull
		res <- list(gBuffer(gConvexHull(SpatialPoints(pts)), width=0.5), method='MCH')
	}
	if (nrow(pts) > 0 & nrow(pts) < 3) {
		#buffered points
		res <- list(gBuffer(SpatialPoints(pts), width=1), method='pointBuff1deg')
	}
	if (nrow(pts) == 0) {
		res <- 'no points'
	}
	return(res)
}



########
# polylist <- mclapply(splist, function(x) makeRange(x), mc.cores = nCores)

splist <- splist[1:20]

## FOR DEBUGGING
polylist <- vector('list', length=length(splist))
for (i in 1:length(splist)) {
	cat(i, '\n')
	polylist[[i]] <- makeRange(splist[[i]])
}

polylist2 <- vector('list', length=length(splist))
for (i in 1:length(splist)) {
	cat(i, '\n')
	polylist2[[i]] <- makeRange2(splist[[i]])
}

#clip to coastline
coast <- readShapeSpatial("~/Dropbox/NWsnakes/source/gshhs/GSHHS_l_L1_NW.shp", force_ring=TRUE, delete_null_obj=TRUE, proj4string=CRS('+proj=longlat +datum=WGS84'))

#set projection so that gIntersection doesn't complain
for (i in 1:length(polylist)) {
	if (length(polylist[[i]]) == 2) {
		proj4string(polylist[[i]][[1]]) <- CRS('+proj=longlat +datum=WGS84')
	}
}

#clip to coast...parallel
ind <- sapply(polylist, function(x) length(x) > 1 & !is.null(x))
ind <- ind[which(ind == TRUE)]
polylist2 <- polylist
polylist2[ind] <- mclapply(polylist2[ind], function(x) list(gIntersection(x[[1]], coast), x[[2]]), mc.cores = nCores)



