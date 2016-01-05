# wrapper function for all synonym matching functions


synonymMatch <- function(sp, db, fuzzy = TRUE, fuzzyDist = 2, advancedSearch = TRUE, searchSynonyms = TRUE, year1 = 1950, year2 = 1900, returnMultiple = FALSE, printReport = TRUE, nthreads = 1) {

	if (!db %in% c('squamates','birds','mammals')) {
		stop('db can currently only be squamates, birds or mammals.')
	}
	
	if (!is.vector(sp)) {
		stop('sp must be a vector of species names.')
	}
	
	if (db == 'squamates') {
		if (!searchSynonyms) {
			year1 <- NULL
		}
		res <- synonymMatchByYear_repDB(x = sp, year1 = year1, year2 = year2, fuzzy = fuzzy, fuzzyDist = fuzzyDist, advancedSearch = advancedSearch, returnMultiple = returnMultiple, printReport = printReport, nthreads = nthreads)
	}
	
	if (db == 'birds' | db == 'mammals') {
		res <- synonymMatch_birdMammal(x = sp, db = db, fuzzy = fuzzy, fuzzyDist = fuzzyDist, advancedSearch = advancedSearch, searchSynonyms = searchSynonyms, returnMultiple = returnMultiple, printReport = printReport)
	}
	
	return(res)
}



