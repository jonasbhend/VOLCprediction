#' @name mask_recovery
#' @aliases mask_erupaod
#' 
#' @title
#' Mask n years after eruption
#' 
#' @description
#' This functions masks forecast scores for n years after the eruption, to
#' exclude years with recovery from volcanic eruption in the averaging
#' interval.
#' 
#' @param x input (output from all_scores)
#' @param after number of years after eruption to be masked
#' 
#' @keywords utilities
#' @export
mask_recovery <- function(x, after=6){
  if (!is.null(x$erup.i)){
    nseas <- median(table(floor(attr(x$opt, 'time'))))
    ei <- which(x$erup.i)
    after.i <- unique(as.vector(outer(ei, seq(0, nseas*after - 1), '+')))
    after.i <- after.i[after.i < length(attr(x$opt, 'time'))]
    x$optimistic[,,after.i] <- NA
    x$pessimistic[,,after.i] <- NA        
  } 
  
  return(x)
}

#' @rdname mask_recovery
#' @aod threshold over which to mask
#' @export
mask_climaod <- function(x, aod=0.01){
  if (!is.null(x$climaod)){
    x$optimistic[,,x$climaod[1,] > aod] <- NA
    x$pessimistic[,,x$climaod[1,] > aod] <- NA        
  } 
  return(x)
}
