% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAllCountries.R
\name{getAllCountries}
\alias{getAllCountries}
\title{Get List of Countries}
\usage{
getAllCountries(db)
}
\arguments{
\item{db}{appropriate synonyms database: squamates, birds, mammals, amphibians}
}
\value{
List of country names for each species.
}
\description{
Get a list of country names for which species lists are available.
}
\details{
There will be minor differences between taxon databases because, for example,
	there are more countries where birds occur than where squamates occur. See
	documentation for \code{link{getCountryFromSpecies}} for details on data sources.
}
\examples{

getAllCountries(db='birds')

}
\author{
Pascal Title
}
