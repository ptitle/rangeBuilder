\name{standardizeCountry}
\alias{standardizeCountry}
\title{
Standardize country name
}
\description{
Standardizes country names to the list of countries used internally by this package.
}
\usage{
standardizeCountry(country, nthreads = 1)
}
\arguments{
	\item{country}{
		character vector of country names
	}
	\item{nthreads}{
		number of threads to use for parallelization of the function. 
		The R package \code{parallel} must be loaded for \code{nthreads > 1}.
	}
}

\details{
	This package interacts with data from the Global Invasive Species Database (GISD), the Reptile Database, as well as global maps that were used to generate the internal dataset used by \code{\link{closestCountry}}. Efforts have been made to make country names consistent across these separate datasets. This function can be used to convert the user's \code{Country} field to the same standardized set. 

	Parallelization with \code{nthreads} becomes more time-efficient only if the input vector is of multiple thousands of country names.
}

\value{
	Character vector of the standardized country names. If no match found, "" is returned. 
}
\author{
	Pascal Title
}

\examples{
standardizeCountry(c("Russian Federation", "USA", "Plurinational State of Bolivia"))
}
 
