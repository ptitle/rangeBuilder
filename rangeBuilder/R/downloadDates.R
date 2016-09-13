# Function to return the download dates of the various datasets included in rangeBuilder

downloadDates <- function() {
	cat('\n')
	for (i in 1:length(dateList)) {
		cat(names(dateList)[i])
		cat('\n\t', dateList[[i]], '\n\n')
	}
}