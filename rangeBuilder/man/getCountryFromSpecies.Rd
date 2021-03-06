% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCountryFromSpecies.R
\name{getCountryFromSpecies}
\alias{getCountryFromSpecies}
\title{Get List of Countries For Species}
\usage{
getCountryFromSpecies(sp, db, useSpatialTaxonomy = FALSE)
}
\arguments{
\item{sp}{character vector of genus_species}

\item{db}{appropriate synonyms database: squamates, birds, mammals, amphibians}

\item{useSpatialTaxonomy}{If \code{TRUE}, then the taxonomy of the original dataset is used,
if \code{FALSE}, then the 'accepted' taxonomy used in synonymy matching functions of this package
is used. See details.}
}
\value{
List of country names for each species.
}
\description{
Get a list of countries that a species is known to occur in, based on a 
range polygon dataset.
}
\details{
For mammals and amphibians, countries by species are derived from IUCN range polygons. For birds, range 
	polygons come from BirdLife International. For squamates, distribution data are from Reptile Database.
	If \code{useSpatialTaxonomy = FALSE}, then the taxon names associated with the spatial data are matched
	to 'accepted' taxon names, as determined through \code{\link{synonymMatch}}. In this case, unrecognized 
	species are dropped, and if two taxa match to the same accepted taxon name, then the country data are 
	combined. For birds and squamates, as the distributional data come from the same source as the taxonomy 
	data, there is only one taxonomy.

	For birds, mammals and amphibians, geographic range polygons were subset to represent the known range 
	(presence = 1), the native or reintroduced range (origin = 1 or 2), and resident, breeding and 
	non-breeding range (seasonal = 1 or 2 or 3). For squamates, this function should return only 
	native ranges.
}
\examples{

getCountryFromSpecies(c('Junco hyemalis', 'unrecognized_taxon', 'Piranga_rubra'), db = 'birds')
getCountryFromSpecies('Anolis_carolinensis', db = 'squamates')
getCountryFromSpecies('', db='mammals')

}
\author{
Pascal Title
}
