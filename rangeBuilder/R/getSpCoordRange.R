#Function to return long/lat range for species
getSpCoordRange <- function(sp, db, useSpatialtaxonomy = FALSE) {
	
	db <- match.arg(db, c('squamates', 'birds', 'mammals', 'amphibians'))
	
	if ('squamates') {
		stop("This resource is not yet available for squamates.")
	}
	
	# identify proper table
	if (db == 'amphibians') {
		coordRange <- amphibCoordRange
	} else if (db == 'birds') {
		coordRange <- birdCoordRange
	} else if (db == 'mammals') {
		coordRange <- mammalCoordRange
	} else {
		stop('db currently only birds, mammals or amphibians.')
	}
	
	if (db != 'birds') {
		if (useSpatialtaxonomy & db != 'squamates') {
			newNames <- strsplit(rownames(coordRange), '-')
			newNames <- sapply(newNames, function(x) x[1])
			rownames(coordRange) <- newNames
		}
		
		if (!useSpatialtaxonomy & db != 'squamates') {
			newNames <- strsplit(rownames(coordRange), '-')
			iucnNames <- sapply(newNames, function(x) x[1]) 
			newNames <- sapply(newNames, function(x) x[2])
			
			# merge species where appropriate
			if (any(grepl('\\|', newNames))) {
				for (i in 1:length(newNames)) {
					if (grepl('\\|', newNames[i])) {
						taxa <- strsplit(newNames[i], '\\|')[[1]]
						ind <- c(i, sapply(taxa, function(x) which(iucnNames == x)))
						ind <- ind[sapply(ind, function(x) length(x) > 0)]
						coordRange[i,1] <- min(coordRange[unlist(ind),1])
						coordRange[i,2] <- max(coordRange[unlist(ind),2])
						coordRange[i,3] <- min(coordRange[unlist(ind),3])
						coordRange[i,4] <- max(coordRange[unlist(ind),4])
						newNames[i] <- taxa[1]
					}
				}
			}
			
			# replace list names
			rownames(coordRange) <- newNames
			
			# drop NA names (they are not an accepted taxon name)
	  		coordRange <- coordRange[which(newNames != 'NA'),]
		}
	}
	
	sp <- gsub("\\s+", "_", sp)
	present <- sp %in% rownames(coordRange)
	res <- matrix(nrow = length(sp), ncol = 4)
	colnames(res) <- colnames(coordRange)
	rownames(res) <- sp
	res[present,] <- coordRange[sp[present],]
	return(res)
}


