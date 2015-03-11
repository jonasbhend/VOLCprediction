#' Collate scores along the time axis
#' 
#' Function to merge two objects with forecast scores (etc.) and collate
#' the contents of the objects along the time dimension.
#' 
#' @param x,y input (output of all_scores)
#' 
#' @keywords utilities
#' @export
collate_scores <- function(x,y){
  xtime <- attr(x$opt, 'time')
  ytime <- attr(y$opt, 'time')
  atime <- sort(unique(c(xtime, ytime)))
  out <- list()
  out$optimistic <- array(NA, c(dim(x$opt)[-length(dim(x$opt))], length(atime)))
  out$optimistic[,,atime %in% xtime] <- x$opt
  out$optimistic[,,atime %in% ytime] <- y$opt
  atns <- names(attributes(x$optimistic))[-grep('dim', names(attributes(x$optimistic)))]
  for (atn in setdiff(atns, 'time')) attr(out$optimistic, atn) <- attr(x$optimistic, atn)
  attr(out$optimistic, 'time') <- atime
  if (!is.null(x$pessimistic)){
    out$pessimistic <- array(NA, dim(out$optimistic))
    out$pessimistic[,,atime %in% xtime] <- x$pess
    out$pessimistic[,,atime %in% ytime] <- y$pess
    for (atn in setdiff(atns, 'time')) attr(out$pessimistic, atn) <- attr(x$pessimistic, atn)
    attr(out$pessimistic, 'time') <- atime
    out$erup.i <- rep(NA, length(atime))
    out$erup.i[atime %in% xtime] <- x$erup.i
    out$erup.i[atime %in% ytime] <- y$erup.i
    out$climaod <- out$erupaod <- t(rep(NA, length(atime)))
    out$climaod[1,atime %in% xtime] <- x$climaod[1,]
    out$climaod[1,atime %in% ytime] <- y$climaod[1,]
    out$erupaod[1,atime %in% xtime] <- x$erupaod[1,]
    out$erupaod[1,atime %in% ytime] <- y$erupaod[1,]
    class(out$climaod) <- class(out$erupaod) <- 'NetCDF'
    attr(out$climaod, 'time') <- attr(out$erupaod, 'time') <- atime
  }
  
  return(out)
}