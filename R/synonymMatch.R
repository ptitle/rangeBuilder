##' Match synonyms to accepted names
##' 
##' @description Performs strict and fuzzy matching to return the accepted species name
##' 
##' @param x a character vector of Genus_species_subspecies (can be multiple)
##' @param db squamates, birds, mammals or amphibians
##' @param fuzzyDist for fuzzy searching, the maximum string distance allowed
##' 	for a match
##' @param advancedSearch logical, should advanced searching be used, see
##' 	Details.
##' @param searchSynonyms if \code{FALSE}, strict and fuzzy matching is applied only
##' 	to the list of accepted names
##' @param yearCutoff year for oldest considered synonyms, please treat as experimental.
##' @param returnMultiple if \code{FALSE}, \code{NA} is returned if no match
##' 	found or if multiple matches found. if \code{TRUE}, then multiple hits are
##' 	returned.
##'	@param progressBar if \code{FALSE}, progress bar will be suppressed. 
##' @param nthreads number of threads to use for parallelization of the
##' function.

##' @details
##' 	The order of the procedure applied here is as follows: \cr
##' 	First Pass: \cr
##'		\itemize{
##' 	\item Strict matching against accepted names, \cr
##' 	\item fuzzy matching against accepted names, \cr
##' 	\item strict matching against synonyms,\cr 
##' 	\item fuzzy matching against synonyms,\cr
##'		}
##' 	Second Pass:
##' 	\itemize{
##'		\item Same as first pass, but ignoring subspecies\cr
##'		}
##' 	Advanced Search: \cr
##' 	\itemize{
##'		\item Same as first pass, but with consideration of alternate latin suffixes and 
##' 	all genus/species combinations.
##'		}
##'	
##' 	The squamate database is a local copy of the Reptile Database
##' 	(\url{http://reptile-database.reptarium.cz/}), which will be updated
##' 	periodically. The list of accepted names within this R package are those
##' 	that are listed as such on the website.
##' 
##' 	The bird database is the BirdLife Taxonomic Checklist as downloaded
##' 	from \url{http://datazone.birdlife.org/species/taxonomy}.
##' 
##' 	The mammal database is Wilson and Reeder's Mammal Species of the World, 3rd
##' 	edition, downloaded from
##' 	\url{http://www.departments.bucknell.edu/biology/resources/msw3/},
##' 	and supplemented with searches of the ITIS database with the 
##' 	\code{taxize} R package.
##' 
##' 	The amphibian database is a local copy of the AmphibiaWeb taxonomy
##' 	(\url{https://amphibiaweb.org/taxonomy/index.html}), which will be updated
##' 	periodically.
##' 
##' 	To see when these datasets were last updated for this R package, run
##' 	\code{\link{downloadDates}}.
##'
##' @return a vector of matches, \code{NA} if the species name could not be
##' 	unambiguously matched to a single accepted name. If \code{returnMultiple =
##' 	TRUE}, then \code{NA} is only returned when the taxon name is not matched at
##' 	all in the database.
##'
##' @references
##' HBW and BirdLife International. 2017. Handbook of the Birds of the World and BirdLife 
##' International digital checklist of the birds of the world. Version 2 Available at: 
##' \url{http://datazone.birdlife.org/userfiles/file/Species/Taxonomy/HBW-BirdLife_Checklist_Version_2.zip}
##' 
##' Don E. Wilson and DeeAnn M. Reeder (editors). 2005. Mammal Species of the
##' World. A Taxonomic and Geographic Reference (3rd ed), Johns Hopkins
##' University Press, 2, 142 pp.
##' 
##' Uetz P., Hosek, J. (ed.). 2016. The Reptile Database,
##' \url{http://www.reptile-database.org}.
##'
##' @author Pascal Title
##'
##' @examples
##'  
##' # simple misspelling
##' synonymMatch('Crotalus_atrix', db = 'squamates')
##' 
##' # synonym
##' synonymMatch('Pipistrellus_macrotis', db = 'mammals')
##' 
##' # synonym with slight misspelling
##' synonymMatch('Tangara_pulchirrima', db = 'birds')
##' 
##' # no match, but return multiple
##' synonymMatch('Masticophis_flagellum', db = 'squamates', returnMultiple = TRUE)
##' 
##' @export


# testing
# x <- c('Crotalus_mitchelli_pyrrhus', 'Coluber_flagelus', 'Crotalus_pyrrhus', 'Coluber_flagella', 'Masticophis_flagellum', 'Crotalus_mitchelli', 'Crotalus_atrox_bombifer', 'Crotalus_testtest')



synonymMatch <- function(x, db, yearCutoff = NULL, searchSynonyms = TRUE, fuzzyDist = 2, advancedSearch = FALSE, returnMultiple = FALSE, progressBar = TRUE, nthreads = 1) {

	db <- match.arg(db, c('squamates', 'birds', 'mammals', 'amphibians'))

	if (!db %in% c('squamates', 'birds', 'mammals', 'amphibians')) {
		stop('db can currently only be squamates, birds, mammals or amphibians.')
	}

	res <- vector(mode = 'character', length = length(x))
	
	# replace spaces with underscores
	x <- gsub('\\s+', '_', x)
	
	# if only one word found, add an NA
	x[!grepl('_', x)] <- paste0(x[!grepl('_', x)], '_NA')
	
	x <- gsub('_NA$', '', x)
	
	uniqueSp <- unique(x)

	# identify proper synonym table
	if (db == 'squamates') {
		synonymTable <- squamTable
	}
	if (db == 'amphibians') {
		synonymTable <- amphibTable
	}
	if (db == 'birds') {
		synonymTable <- birdTable
	}
	if (db == 'mammals') {
		synonymTable <- mammalTable
	}

	#return NA for genus only, or species only
	res[grepl("_NA$|^NA_", x)] <- NA
	uniqueSp <- uniqueSp[!grepl("_NA$|^NA_", uniqueSp)]
	
	# if yearCutoff is provided, subset synonym database to anything equal or more recent than that year
	# records with no year will be ignored in this filter
	# amphibTable does not have years, so this step will be skipped
	if (!is.null(yearCutoff)) {
		taxTable <- synonymTable[which(synonymTable$year >= yearCutoff | is.na(synonymTable$year)), 1:5]
	} else {
		taxTable <- synonymTable[, 1:5]
	}
	taxTable <- as.data.table(taxTable)
	
	op <- pbapply::pboptions(type = "timer")
	if (length(uniqueSp) < 5 | !progressBar) {
		pbapply::pboptions(type = "none")
	}	

	if (nthreads > 1 & length(uniqueSp) > 5) {
		cl <- parallel::makePSOCKcluster(nthreads)
		parallel::clusterExport(cl = cl, varlist = c('firstPassDT', 'uniqueSp', 'taxTable', 'searchSynonyms', 'fuzzyDist', 'returnMultiple'), envir = environment())
		uniqueRes <- pbapply::pblapply(uniqueSp, function(x) firstPassDT(x, taxTable = taxTable, searchSynonyms = searchSynonyms, fuzzyDist = fuzzyDist, returnMultiple = returnMultiple), cl = cl)
		parallel::stopCluster(cl)
	} else {
		uniqueRes <- pbapply::pblapply(uniqueSp, function(x) firstPassDT(x, taxTable = taxTable, searchSynonyms = searchSynonyms, fuzzyDist = fuzzyDist, returnMultiple = returnMultiple))
	}
	
	names(uniqueRes) <- uniqueSp
	
	# which were not resolved and do include subspecies (next attempt will involve ignoring subspecies)?
	notfound <- which(sapply(uniqueRes, function(x) x[[2]] == FALSE) == TRUE)
	notfound <- intersect(notfound, which(sapply(names(uniqueRes), function(y) length(strsplit(y, split = '_')[[1]]) == 3) == TRUE))
	
	# for those that were not matched, attempt while ignoring subspecies
	if (length(notfound) > 0) {
		
		uniqueSp2 <- uniqueSp[notfound]
		
		if (nthreads > 1 & length(uniqueSp) > 5) {
			cl <- parallel::makePSOCKcluster(nthreads)
			parallel::clusterExport(cl = cl, varlist = c('firstPassDT', 'uniqueSp', 'taxTable', 'searchSynonyms', 'fuzzyDist', 'returnMultiple'), envir = environment())
			uniqueRes2 <- pbapply::pblapply(uniqueSp2, function(x) firstPassDT(x, taxTable = taxTable, searchSynonyms = searchSynonyms, fuzzyDist = fuzzyDist, returnMultiple = returnMultiple, includeSubspecies = FALSE), cl = cl)
			parallel::stopCluster(cl)
		} else {
			uniqueRes2 <- pbapply::pblapply(uniqueSp2, function(x) firstPassDT(x, taxTable = taxTable, searchSynonyms = searchSynonyms, fuzzyDist = fuzzyDist, returnMultiple = returnMultiple, includeSubspecies = FALSE))
		}

		names(uniqueRes2) <- uniqueSp2
		
		#combine second pass results with first pass
		for (i in 1:length(notfound)) {
			uniqueRes[[notfound[i]]] <- uniqueRes2[[i]]
		}
	}
	

	# ------------------------
	# ADVANCED SEARCH OPTION
	# ------------------------

	# which were not resolved?
	notfound <- which(sapply(uniqueRes, function(x) x[[2]] == FALSE) == TRUE)
	
	if (advancedSearch & length(notfound) > 0) {
		
		advancedSp <- uniqueSp[notfound]
		
		# create version of synonyms table with additional masculine/feminine endings
		# and with all combinations of genus/species/subspecies
		
		taxTable <- as.data.frame(taxTable)
		acceptedGenSp <- paste(taxTable[,1], taxTable[,2], sep = '_')
		tmpTable <- split(taxTable[, 3:5], acceptedGenSp)
		altTable <- lapply(tmpTable, alternateEndingsDT)
		expandedAccepted <- rep(names(altTable), times = sapply(altTable, nrow))
		expandedAccepted <- matrix(unlist(strsplit(expandedAccepted, split = '_')), ncol = 2, byrow = TRUE)
		altTable <- cbind(expandedAccepted, do.call(rbind, altTable))
		colnames(altTable) <- colnames(taxTable[1:5])
		altTable <- as.data.frame(altTable, stringsAsFactors=FALSE)
		altTable <- altTable[which(altTable$synonym_subspecies != ''), ]
		altTable <- as.data.table(altTable)
		
		if (nthreads > 1 & length(uniqueSp) > 5) {
			cl <- parallel::makePSOCKcluster(nthreads)
			parallel::clusterExport(cl = cl, varlist = c('firstPassDT', 'uniqueSp', 'notfound', 'altTable', 'searchSynonyms', 'fuzzyDist', 'returnMultiple'), envir = environment())
			uniqueRes <- pbapply::pblapply(advancedSp, function(x) firstPassDT(x, taxTable = altTable, searchSynonyms = searchSynonyms, fuzzyDist = fuzzyDist, returnMultiple = returnMultiple), cl = cl)
			parallel::stopCluster(cl)
		} else {
			advancedRes <- pbapply::pblapply(advancedSp, function(x) firstPassDT(x, taxTable = altTable, searchSynonyms = searchSynonyms, fuzzyDist = fuzzyDist, returnMultiple = returnMultiple))
		}
		
		names(advancedRes) <- advancedSp
		
		#combine advanced results with first pass
		for (i in 1:length(notfound)) {
			uniqueRes[[notfound[i]]] <- advancedRes[[i]]
		}
	}
	
	# fill in results vector
	for (i in 1:length(uniqueSp)) {
		res[which(x == uniqueSp[i])] <- uniqueRes[[i]][[1]]
	}
	
	pbapply::pboptions(op)
	
	return(res)
}











firstPassDT <- function(sp, taxTable, searchSynonyms, fuzzyDist, returnMultiple, includeSubspecies = TRUE) {

	# split into components
	components <- strsplit(sp, split = '_')[[1]]
	genus <- components[1]
	species <- components[2]
	subspecies <- ifelse(length(components) == 3, components[3], NA)
	
	if (!includeSubspecies | !searchSynonyms) {
		subspecies <- NA
	}
	
	found <- FALSE
	match <- NA
	
	keepCol <- c('accepted_genus', 'accepted_species')
	
	..keepCol <- merged <- accepted_genus <- accepted_species <- . <- NULL
	
	# --------------------------------
	# Strict search on accepted names
	# --------------------------------
	if (is.na(subspecies)) {
		# strict match of accepted genus and species
		setkeyv(taxTable, c('accepted_genus', 'accepted_species'))
		acceptedRows <- unique(taxTable[.(genus, species), nomatch = 0L, keepCol, with = FALSE])
		if (nrow(acceptedRows) > 0) {
			found <- TRUE
			match <- paste0(genus, '_', species)
		}
	}
	
	# --------------------------------
	# Fuzzy search on accepted names
	# --------------------------------
	if (!found & fuzzyDist > 0 & is.na(subspecies)) {
		# fuzzy match of accepted genus
		fuzzy_genusDist <- stringdist::stringdist(genus, taxTable$accepted_genus, method = 'osa', nthread = 1)

		# fuzzy match of accepted species
		fuzzy_speciesDist <- stringdist::stringdist(species, taxTable$accepted_species, method = 'osa', nthread = 1)
		
		# what are combined fuzzy distances and do any records fall below accepted threshold		
		totalFuzzyDist <- fuzzy_genusDist + fuzzy_speciesDist
		minFuzzyDist <- min(totalFuzzyDist)
		synonymInd <- which(totalFuzzyDist == minFuzzyDist)
		
		fuzzyAcceptedMatch <- unique(taxTable[synonymInd, c('accepted_genus', 'accepted_species')])
		if (nrow(fuzzyAcceptedMatch) == 1 & minFuzzyDist <= fuzzyDist) {
			found <- TRUE
			match <- paste(fuzzyAcceptedMatch, collapse = '_')
		}
	}
	
	# --------------------------
	# Strict search on synonyms
	# --------------------------
	if (!found & searchSynonyms) {
		setkeyv(taxTable, c('synonym_genus', 'synonym_species', 'synonym_subspecies'))
		if (!is.na(subspecies)) {
			synonymRows <- unique(taxTable[.(genus, species, subspecies), nomatch = 0L, keepCol, with = FALSE])
		} else {
			synonymRows <- unique(taxTable[.(genus, species), nomatch = 0L, keepCol, with = FALSE])
		}
		if (nrow(synonymRows) > 0) {
			found <- TRUE
			if (nrow(synonymRows) == 1) {
				match <- paste(synonymRows, collapse = '_')
			} else {
				if (returnMultiple) {
					synonymRows[, merged := paste(accepted_genus, accepted_species, sep = '_')]
					match <- paste(synonymRows$merged, collapse = ' | ')
				} else {
					match <- NA
				}			
			}
		}
	}
	
	# --------------------------
	# Fuzzy search on synonyms
	# --------------------------
	if (!found & fuzzyDist > 0 & searchSynonyms) {
		fuzzy_genusDist <- stringdist::stringdist(genus, taxTable$synonym_genus, method = 'osa', nthread = 1)
	
		fuzzy_speciesDist <- stringdist::stringdist(species, taxTable$synonym_species, method = 'osa', nthread = 1)
		
		if (!is.na(subspecies)) {
			fuzzy_subspeciesDist <- stringdist::stringdist(subspecies, taxTable$synonym_subspecies, method = 'osa', nthread = 1)
			
			totalFuzzyDist <- fuzzy_genusDist + fuzzy_speciesDist + fuzzy_subspeciesDist
			
		} else {
			totalFuzzyDist <- fuzzy_genusDist + fuzzy_speciesDist
		}	
		
		minFuzzyDist <- min(totalFuzzyDist)
		synonymInd <- which(totalFuzzyDist == minFuzzyDist)
		
		if (length(synonymInd) > 0 & minFuzzyDist <= fuzzyDist) {
			found <- TRUE
			acceptedMatches <- unique(taxTable[synonymInd, 1:2])
			if (nrow(acceptedMatches) == 1) {
				match <- paste(acceptedMatches, collapse = '_')
			} else {
				if (returnMultiple) {
					acceptedMatches[, merged := paste(accepted_genus, accepted_species, sep = '_')]
					match <- paste(acceptedMatches$merged, collapse = ' | ')
				} else {
					match <- NA
				}			
			}
		}
	}
	
	return(list(match = match, found = found))
}

alternateEndingsDT <- function(x) {
	
	genera <- unique(x[,1])
	species <- unique(x[,2])
	subspecies <- unique(x[,3])
	
	# create set of alternate endings
	# add masculine/feminine variations
	species <- append(species, gsub('a$', 'um', species))
	species <- append(species, gsub('a$', 'is', species))
	species <- append(species, gsub('a$', 'us', species))
	species <- append(species, gsub('um$|is$|us$', 'a', species))

	subspecies <- append(subspecies, gsub('a$', 'um', subspecies))
	subspecies <- append(subspecies, gsub('a$', 'is', subspecies))
	subspecies <- append(subspecies, gsub('a$', 'us', subspecies))
	subspecies <- append(subspecies, gsub('um$|is$|us$', 'a', subspecies))

	#create all combinations
	combi <- CJ(genera, unique(species), unique(subspecies))
	return(as.matrix(combi))
}
	
	











