##' @title Get List of Countries For Species
##' 
##' @description Get a list of countries that a species is known to occur in, based on a 
##' range polygon dataset. 
##' 
##' @param sp character vector of genus_species
##' @param db appropriate synonyms database: squamates, birds, mammals, amphibians
##' @param useSpatialTaxonomy If \code{TRUE}, then the taxonomy of the original dataset is used,
##' 	if \code{FALSE}, then the 'accepted' taxonomy used in synonymy matching functions of this package
##' 	is used. See details. 
##'
##' @details
##' 	For mammals and amphibians, countries by species are derived from IUCN range polygons. For birds, range 
##' 	polygons come from BirdLife International. For squamates, distribution data are from Reptile Database.
##' 	If \code{useSpatialTaxonomy = FALSE}, then the taxon names associated with the spatial data are matched
##' 	to 'accepted' taxon names, as determined through \code{\link{synonymMatch}}. In this case, unrecognized 
##' 	species are dropped, and if two taxa match to the same accepted taxon name, then the country data are 
##' 	combined. For birds and squamates, as the distributional data come from the same source as the taxonomy 
##' 	data, there is only one taxonomy.
##' 
##' 	For birds, mammals and amphibians, geographic range polygons were subset to represent the known range 
##' 	(presence = 1), the native or reintroduced range (origin = 1 or 2), and resident, breeding and 
##' 	non-breeding range (seasonal = 1 or 2 or 3). For squamates, this function should return only 
##' 	native ranges.
##' 
##' @return List of country names for each species. 
##' @author Pascal Title
##' @examples
##' 
##' getCountryFromSpecies(c('Junco hyemalis', 'unrecognized_taxon', 'Piranga_rubra'), db = 'birds')
##' getCountryFromSpecies('Anolis_carolinensis', db = 'squamates')
##' getCountryFromSpecies('', db='mammals')
##'
##' @export


getCountryFromSpecies <- function(sp, db, useSpatialTaxonomy = FALSE) {
	
	db <- match.arg(db, c('squamates', 'birds', 'mammals', 'amphibians'))
	
	if (useSpatialTaxonomy & db == 'squamates') {
		stop("IUCN taxonomy not available for squamates. Data come from Reptile Database.")
	}
	
	# identify proper table
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
	
	if (!db %in% c('birds', 'squamates')) {
		# bird and squamate datasets do not have synonymy in these data.
		
		if (useSpatialTaxonomy) {
			newNames <- strsplit(names(countryList), '-')
			newNames <- sapply(newNames, function(x) x[1])
			names(countryList) <- newNames
		} else {
			newNames <- strsplit(names(countryList), '-')
			iucnNames <- sapply(newNames, function(x) x[1]) 
			newNames <- sapply(newNames, function(x) x[2])
			
			# merge species where appropriate
			if (any(grepl('\\|', newNames))) {
				for (i in 1:length(newNames)) {
					if (grepl('\\|', newNames[i])) {
						taxa <- strsplit(newNames[i], '\\|')[[1]]
						ind <- c(i, sapply(taxa, function(x) which(iucnNames == x)))
						ind <- ind[sapply(ind, function(x) length(x) > 0)]
						countryList[[i]] <- Reduce(union, countryList[unlist(ind)])
						newNames[i] <- taxa[1]
					}
				}
			}
			
			# replace list names
			names(countryList) <- newNames
			
			# drop NA names (they are not an accepted taxon name)
	  		countryList <- countryList[which(newNames != 'NA')]
		}
	}
	
	
	sp <- gsub("\\s+", "_", sp)
	res <- lapply(sp, function(x) countryList[[x]])
	names(res) <- sp
	return(res)
}
