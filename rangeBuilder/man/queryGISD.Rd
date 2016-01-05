\name{queryGISD}
\alias{queryGISD}
\title{
Query the Global Invasive Species Database
}
\description{
Returns a list of countries, categorized as native and invasive range.
}
\usage{
queryGISD(species)
}
\arguments{
	\item{species}{
		genus and species
	}
}

\details{
	This function returns distribution information as found on the Distribution tab
	from the Global Invasive Species Database: \url{http://www.issg.org/database/welcome/}

	Because of how the GISD webservice is designed, it is possible to have the same country
	listed under both native and invasive distributions. This is because the species in question
	is native to one part of the country and invasive in another part of that country. See the 
	GISD website for more detailed information.

	This function queries a static version of the database acquired on January 15 2015.
	This static version will be updated periodically. 
}

\value{
	list with 3 elements
	\item{species}{
		the name of the species that was queried.
	}
	\item{native}{
		a vector of country names that comprise the native range of the species.
	}
	\item{alien}{
		a vector of country names that comprise the alien range of the species.
	}
}
\author{
	Pascal Title
}

\examples{
# find GISD information for the burmese python
queryGISD('Python_molurus')
}
 
