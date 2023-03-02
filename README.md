<!-- badges: start -->
[![CRAN version](http://www.r-pkg.org/badges/version/rangeBuilder)](http://cran.rstudio.com/web/packages/rangeBuilder/index.html)
[![R-CMD-check](https://github.com/ptitle/rangeBuilder/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ptitle/rangeBuilder/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

rangeBuilder
=========

rangeBuilder provides tools for filtering occurrence records, generating alpha-hull-derived range polygons and mapping species richness. 

Installation
---------------
Stable versions of ```rangeBuilder``` are available for installation from CRAN from within R:

	install.packages("rangeBuilder")

Pre-release versions can be obtained with 

	remotes::install_github("ptitle/rangeBuilder")
	
Updates
-------
18 Nov 2022: All taxon-specific functionality (synonymy matching, etc) has 
been removed as the datasets were outdated, improvements were waiting to 
be made, and CRAN policies make it difficult to include all of the 
necessary datasets while maintaining small file sizes. 
The R package now is more exclusively focused on species occurrence record 
cleaning and mapping. I hope to resurrect the taxon-specific functionality 
in the future.
