##' @title Return download dates of included datasets
##' @description Returns either the specific date that datasets were downloaded, or returns the dataset version.
##' 
##' @return
##' For the Global Invasive Species Database, the Reptile Database and AmphibiaWeb, the date of 
##' download is returned, as these datasets are updated periodically. 
##' For the BirdLife Taxonomic Checklist, the Wilson & Reeder Mammals of the World, the IUCN geographic range 
##' datasets, the version or edition is returned. 

##' @author Pascal Title
##' @examples
##' downloadDates()
##'
##' @export

downloadDates <- function() {
	cat('\n')
	for (i in 1:length(dateList)) {
		cat(names(dateList)[i])
		cat('\n\t', dateList[[i]], '\n\n')
	}
}