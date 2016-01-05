\name{flipSign}
\alias{flipSign}
\title{
Flip sign of coordinates
}
\description{
Checks for coordinate sign mistakes by checking all possibilities against country occupancy.
}
\usage{
flipSign(coordVec, country, returnMultiple = FALSE, proj = "+proj=longlat +datum=WGS84")
}
\arguments{
	\item{coordVec}{
		numeric vector of length 2: longitude, latitude
	}
	\item{country}{
		the country that is associated with the record
	}
	\item{returnMultiple}{
		if multiple sign flips lead to the correct country, return all options. 
		If \code{FALSE}, returns the coords with the fewest needed sign flips.
	}
	\item{proj}{
		the proj4string of the coordinate. 
	}
}

\details{
	This function generates all possible coordinates with different signs, and 
	runs \code{\link{closestCountry}} on each, returning the coordinates that lead 
	to a country match.

	If a point falls close to the boundary between two countries, it is still considered a match.
}

\value{
	list with 2 elements
	\item{matched}{
		logical: Was the country matched
	}
	\item{newcoords}{
		matrix of coordinates that were successful.
	}
}
\author{
	Pascal Title
}

\examples{
#correct coordinates
flipSign(c(4.28, 39.98), country = 'Spain')

#mistake in coordinate sign
flipSign(c(115.436, 32.657), country = 'United States')

#incorrect sign on the latitude, but not possible to detect correct longitude
flipSign(c(4.28, -39.98), country = 'Spain')

#incorrect sign on both, but not possible to distinguish for longitude
flipSign(c(-4.28, -39.98), country = 'Spain', returnMultiple = TRUE)

#coordinates are incorrect
flipSign(c(4.28, 59.98), country = 'Spain')
}
 
