##' @title addRasterLegend
##'
##' @description Adds a legend to an existing raster plot, with some additional manual control

##' @param r the rasterLayer object that has been plotted
##' @param direction direction of color ramp. If omitted, then direction is automatically 
##'	 inferred, otherwise can be specified as \code{horizontal} or \code{vertical}.
##' @param side side for tick marks, see \code{\link{axis}} documentation. 
##'		Automatically inferred if omitted.
##' @param location either a location name (see \code{Details}), or coordinates for 
##'		the corners of the bar legend \code{c(xmin, xmax, ymin, ymax)}.
##' @param nTicks number of tick marks, besides min and max.
##' @param adj if location is top, left, bottom or right, use this argument to adjust the 
##'		location of the legend, defined in percent of the figure width. See Details 
##'		for additional information. 
##' @param shortFrac Percent of the plot width range that will be used as the short 
##' 	dimention of the legend. Only applies to preset location options.
##' @param longFrac Percent of the plot width range that will be used as the long dimention 
##'		of the legend. Only applies to preset location options.	
##' @param axisOffset distance from color bar for labels, as a percent of the plot width.
##' @param border logical, should the color legend have a black border
##' @param ramp either a vector of color names for defining the color ramp, 
##'		or "terrain" (default raster behavior)
##' @param isInteger If \code{auto}, automatically determines if raster is made up of integer 
##'		values, otherwise \code{TRUE} or \code{FALSE}
##' @param ncolors grain size of color ramp
##' @param breaks If a custom set of color breaks were used in plotting the raster, pass 
##'		those color breaks here. This overrides the minmax option. 
##' @param minmax min and max values from which the color ramp will be derived. If left
##' 	as \code{NULL}, the min and max of the raster will be used.
##' @param locs locations of tick marks, if \code{NULL} automatically placed
##' @param cex.axis size of axis labels
##' @param labelDist distance from axis to axis labels (passed to \code{mgp})
##' @param digits number of decimal places for labels
##' @param bigmark character used to separate thousands and millions, passed 
##' 	to \code{\link{format}}
##' @param ... additional parameters to be passed to \code{\link{axis}}.

##' @details
##'		A number of predefined locations exist in this function to make it easy to 
##'		add a legend to a raster plot. 
##'		Preset \code{locations} are: \code{topleft}, \code{topright}, \code{bottomleft}, 
##'		\code{bottomright}, \code{left}, \code{right}, \code{top} and \code{bottom}. 
##'		If more fine-tuned control is desired, then a numeric vector of length 4 can be 
##'		supplied to \code{location}, specifying the min x, max x, min y and max y 
##'		values for the legend. 
##'		Additionally, the \code{adj} argument can be used to more intuitively adjust where
##' 	the legend is placed. \code{adj} is defined as a percentage of the figure width or 
##' 	height, left to right, or bottom to top, respectively. For example, if the legend is at 
##' 	the bottom, \code{adj = 0.8} will place the legend 80% of the distance from the top of 
##' 	the figure, horizontally centered.  
##'		See examples.

##' @return Invisibly returns a list with the following components.
##' \itemize{
##'	\item{coords}{2-column matrix of xy coordinates for each color bin in the legend.}
##'	\item{width}{Coordinates for the short dimension of the legend.}
##'	\item{pal}{the color ramp}
##'	\item{tickLocs}{the tick mark locations in plotting units}
##'	\item{labels}{the values associated with those tick locations.}
##' }

##' @author Pascal Title

 
##' @export


addRasterLegend <- function(r, direction, side, location = 'right', nTicks = 2, adj = NULL, shortFrac = 0.02, longFrac = 0.3, axisOffset = 0, border = TRUE, ramp = "terrain", isInteger = 'auto', ncolors = 64, breaks = NULL, minmax = NULL, locs = NULL, cex.axis = 0.8, labelDist = 0.7, digits = 2, bigmark = '', ...) {
	
	stop('This function has been deprecated. A similar and better version now exists in the R package epm, on CRAN.')
		
}
	
	

	

