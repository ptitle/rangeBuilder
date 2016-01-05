
# db can be birds, mammals, squamates

#Function to return all synonyms of an accepted species name
getSynonymsFromAccepted <- function(sp, db) {
	sp <- gsub(' ', '_', sp)
	
	if (db == 'squamates') {
		if (!sp %in% names(RepDBlist)) {
			stop('Sp not an accepted species name.')
		}
		return(RepDBlistYr[[which(names(RepDBlistYr) == sp)]])
	} else if (db == 'birds') {
		if (!sp %in% names(birdList)) {
			stop('Sp not an accepted species name.')
		}
		return(birdList[[which(names(birdList) == sp)]])
	} else if (db == 'mammals') {
		if (!sp %in% names(mammalList)) {
			stop('Sp not an accepted species name.')
		}
		return(mammalList[[which(names(mammalList) == sp)]])
	} else {
		stop('db currently only squamates, birds or mammals.')
	}
}



#Function to return all accepted names that have a given synonym
getAcceptedFromSynonym <- function(sp, db) {
	sp <- gsub(' ', '_', sp)
	
	if (db == 'squamates') {
		db <- RepDBlist
	} else if (db == 'mammals') {
		db <- mammalList
	} else if (db == 'birds') {
		db <- birdList
	} else {
		stop('db currently only squamates, birds or mammals.')
	}
	
	if (!sp %in% unlist(db)) {
		stop('Taxon not in database.')
	}
	
	return(names(db)[unlist(lapply(db, function(x) sp %in% x))])
}
	

# Return list of all accepted species names
getAcceptedNames <- function(db) {
	
	if (db == 'squamates') {
		return(names(RepDBlist))
	} else if (db == 'mammals') {
		return(names(mammalList))
	} else if (db == 'birds') {
		return(names(birdList))
	} else {
		stop('db currently only squamates, birds or mammals.')
	}
}
