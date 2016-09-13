\name{acceptedAndSynonyms}
\alias{getSynonymsFromAccepted}
\alias{getAcceptedFromSynonym}
\alias{getAcceptedNames}
\title{
Accepted names and synonyms
}
\description{
Several functions are provided here to perform simple queries 
on accepted vs synonymous species names. 
}
\usage{
getSynonymsFromAccepted(sp, db)
getAcceptedFromSynonym(sp, db)
getAcceptedNames(db)
}
\arguments{
	\item{sp}{
		genus and species
	}
	\item{db}{
		the database to query, can be squamates, birds, mammals or amphibians.
	}
}

\details{

	The workhorse function for matching synonyms to accepted names is \code{\link{synonymMatch}}.
	The functions here are more simple, and are intended to be complementary to the main matching function.

	The squamate database is a local copy of the Reptile Database (\url{http://reptile-database.reptarium.cz/}), which will be updated periodically. The list of accepted names within this R package are those that are listed as such on the website. 

	The bird database is the BirdLife Taxonomic Checklist v8.0 as downloaded from \url{http://www.birdlife.org/datazone/info/taxonomy}. 

	The mammal database is Wilson and Reeder's Mammal Species of the World, 3rd edition, downloaded from \url{http://www.departments.bucknell.edu/biology/resources/msw3/}. 

	The amphibian database is a local copy of the AmphibiaWeb taxonomy (\url{amphibiaweb.org/taxonomy/index.html}), which will be updated periodically.  
	
	To see when these datasets were last updated for this R package, run \code{\link{downloadDates}}.

	Citation:
	
	BirdLife International. 2015. The BirdLife checklist of the birds of the world: Version 8. Downloaded from http://www.birdlife.org/datazone/userfiles/file/Species/Taxonomy/BirdLife_Checklist_Version_80.zip [.xls zipped 1 MB].

	Don E. Wilson & DeeAnn M. Reeder (editors). 2005. Mammal Species of the World. A Taxonomic and Geographic Reference (3rd ed), Johns Hopkins University Press, 2,142 pp.

	Uetz P., Hosek, J. (ed.). 2016. The Reptile Database, http://www.reptile-database.org (accessed 30 April 2016).  


}

\value{
	\code{getSynonymsFromAccepted} returns a vector of synonyms for the specified accepted species name. 

	\code{getAcceptedFromSynonym} returns the accepted names that have the specified species name as a synonym (as per strict matching). 
	
	\code{getAcceptedNames} returns the list of accepted species names in the database. 
}
\author{
	Pascal Title
}
\seealso{
	\code{\link{synonymMatch}}
}
\examples{
getSynonymsFromAccepted('Phrynosoma_coronatum', db = 'squamates')
getAcceptedFromSynonym('Phrynosoma_jamesi', db = 'squamates')

}
 

