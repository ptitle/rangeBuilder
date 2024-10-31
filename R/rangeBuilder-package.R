##' @title rangeBuilder
##' @description Provides tools for filtering occurrence records, standardizing countries names, 
##' generating alpha-hull-derived range polygons and mapping species distributions. 
##' 
##' @author Pascal Title <ptitle@umich.edu>
##' 
##' @references
##' Davis Rabosky, A.R., C.L. Cox, D.L. Rabosky, P.O. Title, I.A. Holmes, A. Feldman and J.A. 
##' McGuire. 2016. Coral snakes predict the evolution of mimicry across New World snakes. Nature 
##' Communications 7:11484. 
##' 
##' @name rangeBuilder
##' 
##' 
##' @useDynLib rangeBuilder
##' @importFrom Rcpp evalCpp
##' @importFrom Rcpp sourceCpp
##'
##' @import methods
##' 
##' 
##' @importFrom Rcpp evalCpp
##' @importFrom grDevices col2rgb colorRampPalette rgb terrain.colors
##' @importFrom graphics axis grconvertX grconvertY par rect
##' @importFrom methods hasArg is slot slot<-
##' 
##' 
##' 
##' 
##' 
##' 
"_PACKAGE"