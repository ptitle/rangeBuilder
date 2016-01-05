\name{closestCountry}
\alias{closestCountry}
\title{
Return country from point
}
\description{
Determines which country a given point falls in.
}
\usage{
closestCountry(pt, proj = "+proj=longlat +datum=WGS84")
}
\arguments{
	\item{pt}{
		longitude and latitude, as a numeric vector
	}
	\item{proj}{
		the proj4string of the coordinate. 
	}
}

\details{
	Based on a predetermined set of global points, this function finds the country of occurrence.
	This can be useful for checking the validity of a point by comparing the returned country to 
	the country listed with the occurrence record.

	If a point falls close to the boundary between two countries, the names of the nearby countries
	are returned.

	This function will not be of much value if the point falls in the ocean, as it will return the 
	country that is closest, regardless of how far away it is. 
}

\value{
	a country name, or \code{NA}
}
\author{
	Pascal Title
}

\examples{
#point near a country border
closestCountry(c(-115.436, 32.657))
}
 
