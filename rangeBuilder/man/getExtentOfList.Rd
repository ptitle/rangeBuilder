\name{getExtentOfList}
\alias{getExtentOfList}

\title{
Get extent of list of SpatialPolygons
}
\description{
Returns the extent that encompasses all SpatialPolygons in a list
}
\usage{
getExtentOfList(shapes)
}

\arguments{
  \item{shapes}{
	a list of SpatialPolygons
}
}
\value{
an object of class extent
}
 
\author{
Pascal Title
}
 
\examples{
data(crotalus)

# create some polygons, in this case convex hulls
sp <- split(crotalus, crotalus$genSp)
sp <- lapply(sp, function(x) x[,c('decimallongitude','decimallatitude')])
sp <- lapply(sp, function(x) x[chull(x),])
poly <- lapply(sp, function(x) 
	SpatialPolygons(list(Polygons(list(Polygon(x)), ID = 1))))

getExtentOfList(poly)
}
