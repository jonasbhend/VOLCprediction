#' Get empirical quantile
#' 
#' Get the empirical percentile of a yet unobserved value in a sample
#' 
#' @param x sample of values for empirical distribution
#' @param value value for which empirical quantile should be computed
#' 
#' @keywords utilities
#' @export
#' 
quantile.invert <- function(x, value){

  # probability of sorted values of x
  x  <- sort(x)
  if (diff(range(x)) == 0){
    out <- rep(NA, length(value))
    out[value == median(x)] <- 0.5
  } else {
    px <- (seq(along=x) -0.5) / length(x)    
    out <- approx(x=x, y=px, xout=value)$y
  }
  out[value > max(x)] <- 1
  out[value < min(x)] <- 0
  return(out)
}
