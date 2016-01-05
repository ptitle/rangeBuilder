\name{filterByLand}
\alias{filterByLand}
\title{
Filter occurrences based on land vs ocean
}
\description{
Identifies occurrence records that do not occur on land.
}
\usage{
filterByLand(coords, returnGood = TRUE, proj = '+proj=longlat +datum=WGS84')
}
\arguments{
	\item{coords}{
		coordinates in the form of a 2 column numeric matrix, data.frame, numeric vector, or SpatialPoints object. If Spatial object, proj4string must be specified. 
	}
	\item{returnGood}{
		logical: if \code{TRUE}, index of points that pass filter is returned, if \code{FALSE}, index of points that fail is returned
	}
	\item{proj}{
		proj4string of input coords. Ignored if input coords are spatial object.
	}
}

\details{
	This function uses a rasterized version of the GSHHG (global self-consistent, hierarchical, high-resolution geography database, \url{https://www.soest.hawaii.edu/pwessel/gshhg/}), that has been buffered by 2 km. 
}

\value{
	a vector of indices corresponding to the records that do or do not pass the filter, 
	depending on \code{returnGood}. 
}
\author{
	Pascal Title
}

\examples{
data(crotalus)

#identify points that fall off land
filterByLand(crotalus[,c('decimallongitude','decimallatitude')], returnGood = FALSE)
}
 
