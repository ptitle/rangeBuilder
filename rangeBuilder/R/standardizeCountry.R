##' Standardize country name
##' 
##' Standardizes country names to the list of countries used internally by this
##' package.
##' 
##' This package interacts with data from the Global Invasive Species Database
##' (GISD), the Reptile Database, as well as global maps that were used to
##' generate the internal dataset used by \code{\link{closestCountry}}. Efforts
##' have been made to make country names consistent across these separate
##' datasets. This function can be used to convert the user's \code{Country}
##' field to the same standardized set.
##' 
##' Fuzzy matching uses the function \code{\link{adist}}.
##' 
##' Parallelization with \code{nthreads} becomes more time-efficient only if
##' the input vector is of multiple thousands of country names.
##' 
##' @param country character vector of country names or ISO codes
##' @param fuzzyDist for fuzzy searching, the maximum string distance allowed
##' for a match; if 0, fuzzy searching is disabled.
##' @param nthreads number of threads to use for parallelization of the
##' function.  The R package \code{parallel} must be loaded for \code{nthreads
##' > 1}.
##' @param progressBar if \code{FALSE}, progress bar will be suppressed.
##' @return Character vector of the standardized country names. If no match
##' found, \code{""} is returned.
##' @author Pascal Title
##' @examples
##' 
##' standardizeCountry(c("Russian Federation", "USA", "Plurinational State of Bolivia", "Brezil"))
##' 
##' @export

standardizeCountry <- function(country, fuzzyDist = 1, nthreads = 1, progressBar = TRUE) {
	
	if (any(is.na(country))) {
		country[which(is.na(country))] <- ''
	}
	country <- toupper(country)
	country <- gsub('_|-', ' ', country)
	country <- gsub('\\.', '', country)
	country <- stringi::stri_trans_general(country, "Latin-ASCII")
	country <- gsub('(^|\\s)ST\\.?\\s', 'SAINT ', country)
	country <- gsub('\\s+\\&\\s+', ' AND ', country)
	country <- gsub('ISLAND\\/S', 'ISLANDS', country)
	country <- gsub('\\?|\\[|\\]', '', country)
	country <- gsub('\\/', '', country)
	country <- gsub('\\s+', ' ', country)
	country <- trim(country)
	
	# prepare results vector
	res <- character(length(country))
	
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
	
	uniqueCountry <- unique(country)
	
	op <- pbapply::pboptions(type = "timer")
	if (length(uniqueCountry) < 10 | !progressBar) {
		pbapply::pboptions(type = "none")
	}	
	
	if (nthreads > 1 & length(uniqueCountry) > 10) {
		cl <- parallel::makePSOCKcluster(nthreads)
		parallel::clusterExport(cl = cl, varlist = c('uniqueCountry', 'countryList'), envir = environment())
		uniqueRes <- pbapply::pbsapply(uniqueCountry, function(x) {
			return(matchCountry(x, countryList, fuzzyDist = fuzzyDist))
		}, simplify = TRUE, USE.NAMES = FALSE, cl = cl)
		parallel::stopCluster(cl)
	} else {	
		uniqueRes <- pbapply::pbsapply(uniqueCountry, function(x) {
			return(matchCountry(x, countryList, fuzzyDist = fuzzyDist))
		}, simplify = TRUE, USE.NAMES = FALSE)	
	}
	
	# fill in results vector
	for (i in 1:length(uniqueCountry)) {
		res[which(country == uniqueCountry[i])] <- uniqueRes[i]
	}
	
	pbapply::pboptions(op)
	
	return(res)
}
	


