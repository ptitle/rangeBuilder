##' @title Get List of Countries
##' 
##' @description Get a list of country names for which species lists are available. 
##' 
##' @param db appropriate synonyms database: squamates, birds, mammals, amphibians
##'
##' @details There will be minor differences between taxon databases because, for example,
##' 	there are more countries where birds occur than where squamates occur. See
##' 	documentation for \code{link{getCountryFromSpecies}} for details on data sources.
##'
##' @return List of country names for each species. 
##' @author Pascal Title
##' @examples
##' 
##' getAllCountries(db='birds')
##' 
##' @export


#Function to see list of countries
getAllCountries <- function(db) {

	db <- match.arg(db, c('squamates', 'birds', 'mammals', 'amphibians'))
	
	if (db == 'squamates') {
		countryList <- repDBcountryList_bySp
	} else if (db == 'amphibians') {
		countryList <- amphibCountries_bySp
	} else if (db == 'birds') {
		countryList <- birdCountries_bySp
	} else if (db == 'mammals') {
		countryList <- mammalCountries_bySp
	} else {
		stop('db currently only squamates, birds, mammals or amphibians.')
	}
	
	return(sort(unique(unlist(countryList))))
}
