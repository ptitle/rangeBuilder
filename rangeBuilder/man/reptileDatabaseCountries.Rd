\name{reptileDatabaseCountries}
\alias{reptileDatabaseCountries}
\alias{getRepDBcountryList}
\alias{getRepDBSpFromCountry}
\alias{getRepDBcountries}

\title{
 Squamate distribution data from the Reptile Database
}
\description{
Get the countries of occurrence for a species, or the species list for a country.
}
\usage{
getRepDBcountryList(spname)
getRepDBSpFromCountry(country)
getRepDBcountries()
}
\arguments{
	\item{spname}{
		genus and species
	}
	\item{country}{
		country name
	}
}

\details{
	These functions allow you to query country-level distribution information as per 
	the Reptile Database \url{http://reptile-database.reptarium.cz/}.

	If you see any errors that conflict with what is found online, please inform the package maintainer.
}

\value{

	\code{getRBcountryList} returns a vector of country names for the specified species.
	\code{getRepDBSpFromCountry} returns a vector of species names for the specified country.
	\code{getRepDBcountries} returns the list of countries that can be queried. 
}
\author{
	Pascal Title
}

\examples{
# return countries of occurrence for Naja naja
getRepDBcountryList('Naja_naja')

#return species that occur in New Zealand
getRepDBSpFromCountry('New Zealand')

#return the list of countries that have such data on Reptile-Database
getRepDBcountries()
}
 


