##' Query the Global Invasive Species Database
##' 
##' Returns a list of countries, categorized as native and invasive range.
##' 
##' This function returns distribution information as found on the Distribution
##' tab from the Global Invasive Species Database:
##' \url{http://www.iucngisd.org/gisd/}
##' 
##' Because of how the GISD webservice is designed, it is possible to have the
##' same country listed under both native and invasive distributions. This is
##' because the species in question is native to one part of the country and
##' invasive in another part of that country. See the GISD website for more
##' detailed information.
##' 
##' This function queries a static version of the database, which will be
##' updated periodically.
##' 
##' To see when these datasets were last updated for this R package, run
##' \code{\link{downloadDates}}.
##' 
##' @param species genus and species
##' @return list with 3 elements \item{species}{ the name of the species that
##' was queried.  } \item{native}{ a vector of country names that comprise the
##' native range of the species.  } \item{alien}{ a vector of country names
##' that comprise the alien range of the species.  }
##' @author Pascal Title
##' @examples
##' 
##' # find GISD information for the burmese python
##' queryGISD('Python_molurus')
##' 
##' @export

queryGISD <- function(species) {
	
	species <- gsub(' ', '_', species)
	
	if (species %in% names(invasiveDB[[1]])) {	
		native <- invasiveDB[['nativeRanges']][[species]]
		alien <- invasiveDB[['alienRanges']][[species]]
	} else {
		native <- 'not found'
		alien <- 'not found'
	}
	return(list(species=species, native=native, alien=alien))	
}

