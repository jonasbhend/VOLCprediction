#' @name get_indices
#' @aliases index_wo_recovery
#' 
#' @title
#' Compute scores without eruption
#' 
#' @description
#' Function to find forecast scores without recovery from eruptions in the 
#' prediction interval
#' 
#' @param x input vector or length of forecast sequence
#' @param erup.i index of years with eruptions
#' @param k waiting period after eruptions
#' 
#' @keywords utilities
#' @export
index_wo_recovery <- function(x, erup.i, k){
  nx <- if (length(x) == 1) x else length(x)
  nerup       <- length(erup.i)
  nostart.i   <- sort(unique(as.vector(outer(erup.i, 1:k - 1, '+'))))
  start.i     <- setdiff(1:nx, nostart.i)
  return(start.i)
}

#' @rdname get_indices
#' @param n length of forecast interval
#' @export
index_with_eruption <- function(erup.i, n){  
  ind.i <- outer(erup.i, 1:n - 1, '-')
  ind.i <- sort(unique(as.vector(ind.i)))
  return(ind.i)
}

#' @rdname get_indices
#' @export
index_wo_eruption <- function(x, erup.i, n){  
  nx <- if (length(x) == 1) x else length(x)
  with.i <- index_with_eruption(x, erup.i, n)
  return(setdiff(1:nx, with.i))
}

