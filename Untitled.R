#' Wrapper for Histogram function
#' 
#' This function allows plotting of standardised histograms
#' with a standardisation by bin (rather than unit)
#' 
#' @param x input vector to compute histogram
#' @param ... additional arguments passed to \code{\link{hist}}
new_hist <- function(x, ...){
  newh <- hist(x, plot=F, ...)
  newh$counts <- newh$counts/length(x)
  newh
}
