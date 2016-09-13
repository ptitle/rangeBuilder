# standardize country name to the set used in the rangeBuilder package

standardizeCountry <- function(country, fuzzyDist = 1, nthreads = 1) {
	
	if (nthreads > 1) {
		if (!"package:parallel" %in% search()) {
			stop("Please load package 'parallel' for using the multi-thread option\n");
		}
	}

	if (any(is.na(country))) {
		country[which(is.na(country))] <- ''
	}
	country <- toupper(country)
	country <- gsub('_|-', ' ', country)
	country <- gsub('\\.', '', country)
	country <- stringi::stri_trans_general(country, "Latin-ASCII")
	country <- gsub('(^|\\s)ST\\.?\\s', 'SAINT ', country)
	country <- gsub('\\?|\\[|\\]', '', country)
	country <- gsub('\\/', '', country)
	country <- gsub('\\s+', ' ', country)
	country <- trim(country)
	
	# function to look at each entry in countryList and look for match
	matchCountry <- function(val, countryList, fuzzyDist = fuzzyDist) {
		
		# first determine if this is an ISO code (2 or 3 character)
		if (nchar(val) == 2) {
			if (val %in% isoLookup[,2]) {
				ind <- which(names(countryList) == isoLookup[which(isoLookup[,2] == val),1])
			} else {
				ind <- NULL
			}
		}
		if (nchar(val) == 3) {
			if (val %in% isoLookup[,3]) {
				ind <- which(names(countryList) == isoLookup[which(isoLookup[,3] == val),1])
			} else {
				ind <- NULL
			}
		}
		
		# val is not an ISO code, moving on to country names
		
		if (nchar(val) > 3) {
			# first, test for exact match
			ind <- which(sapply(countryList, function(y) val %in% y) == TRUE)
	
			if (length(ind) == 0) {
				# second, test for fuzzy matching with accepted names
				d <- adist(val, names(countryList))
				mind <- which.min(d)
				if (d[mind] <= fuzzyDist) {
					ind <- mind
				} else {
					# third, check each alternative with fuzzy match
					d <- sapply(countryList, function(y) adist(val, y))
					d <- sapply(d, min)
					mind <- which.min(d)
					if (d[mind] <= fuzzyDist) {
						ind <- mind
					}
				}	
			}
		}
		
		if (nchar(val) < 2) {
			ind <- NULL
		}
		
		if (length(ind) == 0) {
			return('')
		} else {
			return(names(countryList)[ind])
		}
	}
	
	if (nthreads > 1) {
		cl <- parallel::makePSOCKcluster(nthreads)
		parallel::clusterExport(cl = cl, varlist = c('country', 'countryList'), envir = environment())
		res <- parallel::parSapply(cl, country, function(x) {
			return(matchCountry(x, countryList, fuzzyDist = fuzzyDist))
		}, simplify = TRUE, USE.NAMES = FALSE)
		parallel::stopCluster(cl)
	} else {	
		res <- sapply(country, function(x) {
			return(matchCountry(x, countryList, fuzzyDist = fuzzyDist))
		}, simplify = TRUE, USE.NAMES = FALSE)	
	}
	
	return(res)
}
	


