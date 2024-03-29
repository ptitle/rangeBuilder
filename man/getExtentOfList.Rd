% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getExtentOfList.R
\name{getExtentOfList}
\alias{getExtentOfList}
\title{Get extent of list}
\usage{
getExtentOfList(shapes)
}
\arguments{
\item{shapes}{a list of SpatialPolygons or simple features}
}
\value{
An object of class \code{bbox}.
}
\description{
Given a list of SpatialPolygons or sf objects, return 
a bounding box object that encompasses all items.
}
\examples{

data(crotalus)

# create some polygons, in this case convex hulls
sp <- split(crotalus, crotalus$genSp)
sp <- lapply(sp, function(x) x[,c('decimallongitude','decimallatitude')])
sp <- lapply(sp, function(x) x[chull(x),])
poly <- lapply(sp, function(x) 
	sf::st_convex_hull(sf::st_combine(sf::st_as_sf(x, coords = 1:2, crs = 4326))))

getExtentOfList(poly)

}
\author{
Pascal Title
}
