##' Get synonyms from accepted
##' 
##' Returns the synonyms that are associated with a given accepted taxon name.
##' 
##' @param sp genus and species
##' @param db appropriate synonyms database: squamates, birds, mammals, amphibians
##' 
##' The squamate database is a local copy of the Reptile Database
##' (\url{http://reptile-database.reptarium.cz/}), which will be updated
##' periodically. The list of accepted names within this R package are those
##' that are listed as such on the website.
##' 
##' The bird database is the BirdLife Taxonomic Checklist v8.0 as downloaded
##' from \url{http://datazone.birdlife.org/species/taxonomy}.
##' 
##' The mammal database is Wilson and Reeder's Mammal Species of the World, 3rd
##' edition, downloaded from
##' \url{http://www.departments.bucknell.edu/biology/resources/msw3/}.
##' 
##' The amphibian database is a local copy of the AmphibiaWeb taxonomy
##' (\url{https://amphibiaweb.org/taxonomy/index.html}), which will be updated
##' periodically.
##' 
##' To see when these datasets were last updated for this R package, run
##' \code{\link{downloadDates}}.
##' 
##' Citation:
##' 
##' BirdLife International. 2015. The BirdLife checklist of the birds of the
##' world: Version 8. Downloaded from
##' http://www.birdlife.org/datazone/userfiles/file/Species/Taxonomy/BirdLife_Checklist_Version_80.zip
##' [.xls zipped 1 MB].
##' 
##' Don E. Wilson & DeeAnn M. Reeder (editors). 2005. Mammal Species of the
##' World. A Taxonomic and Geographic Reference (3rd ed), Johns Hopkins
##' University Press, 2,142 pp.
##' 
##' Uetz P., Hosek, J. (ed.). 2016. The Reptile Database,
##' http://www.reptile-database.org (accessed 30 April 2016).
##' 
##' @return \code{getSynonymsFromAccepted} returns a vector of synonyms for the
##' specified accepted species name.
##' @author Pascal Title
##' @seealso \code{\link{synonymMatch}}
##' @examples
##' 
##' getSynonymsFromAccepted('Phrynosoma_coronatum', db = 'squamates')
##' 
##' @export

getSynonymsFromAccepted <- function(sp, db) {
	sp <- gsub(' ', '_', sp)
	db <- match.arg(db, c('squamates', 'birds', 'mammals', 'amphibians'))
	
	# identify proper synonym table
	if (db == 'squamates') {
		synonymTable <- squamTable
	} else if (db == 'amphibians') {
		synonymTable <- amphibTable
	} else if (db == 'birds') {
		synonymTable <- birdTable
	} else if (db == 'mammals') {
		synonymTable <- mammalTable
	} else {
		stop('db currently only squamates, birds, mammals or amphibians.')
	}
	
	sp <- gsub('\\s+', '_', sp)
	queryGenus <- strsplit(sp, '_|\\s')[[1]][1]
	querySpecies <- strsplit(sp, '_|\\s')[[1]][2]

	ind <- which(synonymTable$accepted_genus == queryGenus & synonymTable$accepted_species == querySpecies)
	
	if (length(ind) == 0) {
		stop('Sp not an accepted species name.')
	}
	
	ret <- synonymTable[ind, c('synonym_genus', 'synonym_species')]
	ret <- apply(ret, 1, function(x) paste(x, collapse='_'))
	ret <- gsub('_NA$', '', ret)
	ret <- unique(ret)
	ret <- sort(ret[ret != sp])
	return(ret)
}
	
