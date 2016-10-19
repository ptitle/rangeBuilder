[![Build Status](https://travis-ci.org/ptitle/rangeBuilder.svg?branch=master)](https://travis-ci.org/ptitle/rangeBuilder)
[![CRAN version](http://www.r-pkg.org/badges/version/rangeBuilder)](http://cran.rstudio.com/web/packages/rangeBuilder/index.html)

rangeBuilder
=========

rangeBuilder provides tools for filtering occurrence records, generating alpha-hull-derived range polygons and mapping species richness. 

Installation
---------------
Stable versions of ```rangeBuilder``` are available for installation from CRAN from within R:

	install.packages("rangeBuilder");

Pre-release versions can be obtained as follows.

Install Hadley Wickham's ```devtools```:

	install.packages("devtools", dependencies=TRUE);

Then, install ```rangeBuilder```:

	require(devtools);
	install_github("ptitle/rangeBuilder/rangeBuilder");

