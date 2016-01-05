\name{synonymMatch}
\alias{synonymMatch}
\title{
Match synonyms to accepted names
}
\description{
Performs strict and fuzzy matching to return the accepted species name
}
\usage{
synonymMatch(sp, db, fuzzy = TRUE, fuzzyDist = 2, advancedSearch = TRUE, 
	searchSynonyms = TRUE, year1=1950, year2=1900, returnMultiple = FALSE, 
	printReport = TRUE, nthreads = 1)
}
\arguments{
	\item{sp}{
		a character vector of Genus_species (can be multiple)
	}
	\item{db}{
		squamates, birds or mammals
	}
	\item{fuzzy}{
		logical, should fuzzy matching be used
	}
	\item{fuzzyDist}{
		for fuzzy searching, the maximum string distance allowed for a match
	}
	\item{advancedSearch}{
		logical, should advanced searching be used, see Details. 
	}
	\item{searchSynonyms}{
		if FALSE, strict and fuzzy matching is applied only to the list of accepted names
	}
	\item{year1}{
		specific to squamates, year for oldest considered synonyms, see details
	}
	\item{year2}{
		specific to squamates, year for oldest considered synonyms, second pass
	}
	\item{returnMultiple}{
		if \code{FALSE}, \code{NA} is returned if no match found or if multiple 
		matches found. if \code{TRUE}, then multiple hits are returned. 
	}
	\item{printReport}{
		if \code{TRUE}, a summary report is printed at the end of the run.
	}
	\item{nthreads}{
		number of threads to use for parallelization of the function. 
		The R package \code{parallel} must be loaded for \code{nthreads > 1}.
	}
}

\details{
	The order of the procedure applied here is as follows:\cr
	Strict matching against accepted names, \cr
	fuzzy matching against accepted names, \cr
	strict matching against synonyms from year1 to present, \cr
	fuzzy matching against synonyms from year1 to present, \cr
	\code{AdvancedSearch}: \cr
	strict matching against synonyms from year2 to present, \cr
	consideration of alternate latin suffixes and all genus/species 
	combinations with strict matching, \cr
	consideration of alternate latin suffixes and all genus/species 
	combinations with fuzzy matching. 

	The printed report shows counts for the set of unique taxon names, not the full 
	vector that was input. 

	Parallelization becomes time-efficient with as few as 15 unique taxon names.
	

	The squamate database is a local copy of the Reptile Databse (\url{http://reptile-database.reptarium.cz/}), downloaded 5 October 2015, which will be updated periodically. The list of accepted names within this R package are those that are listed as such on the website. 

	The bird database is the BirdLife Taxonomic Checklist v8.0 as downloaded from \url{http://www.birdlife.org/datazone/info/taxonomy}. 

	The mammal database is Wilson and Reeder's Mammal Species of the World, 3rd edition, downloaded from \url{http://www.departments.bucknell.edu/biology/resources/msw3/}. 

	Citation:
	
	BirdLife International. 2015. The BirdLife checklist of the birds of the world: Version 8. Downloaded from http://www.birdlife.org/datazone/userfiles/file/Species/Taxonomy/BirdLife_Checklist_Version_80.zip [.xls zipped 1 MB].

	Don E. Wilson & DeeAnn M. Reeder (editors). 2005. Mammal Species of the World. A Taxonomic and Geographic Reference (3rd ed), Johns Hopkins University Press, 2,142 pp.

	Uetz P., Hosek, J. (ed.). 2015. The Reptile Database, http://www.reptile-database.org (accessed 5 October 2015).  

}

\value{
	a vector of matches, \code{NA} if the species name could not be unambiguously matched 
	to a single accepted name. If \code{returnMultiple = TRUE}, then \code{NA} is 
	only returned when the taxon name is not found at all in the database. 
}
\author{
	Pascal Title
}

\examples{

# simple misspelling
synonymMatch('Crotalus_atrix', db = 'squamates')

# synonym
synonymMatch('Pipistrellus_macrotis', db = 'mammals')

#synonym with slight misspelling
synonymMatch('Tangara_pulchirrima', db = 'birds')

#no match, but return multiple
synonymMatch('Masticophis_flagellum', db = 'squamates', returnMultiple = TRUE)
}
 

