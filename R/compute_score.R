#' @name compute_scores
#' @aliases comp_score
#' @aliases all_scores
#' 
#' @title
#' Compute forecast score
#' 
#' @description
#' This function computes the residual variance in the forecast interval.
#' This is the variance unexplained by the prediction in case the forecasting
#' system has exactly zero skill. In case the forecasting system has skill,
#' only a fraction of the residual variance is computed. Bounding assumptions 
#' on how skill plays out with an eruption in the forecast interval are
#' computed. These bounding assumptions are: i) skill is not influenced by the 
#' eruption (optimistic), ii) all skill is lost after an eruption (pessimistic).
#' 
#' @param x input object of class 'NetCDF'
#' @param aod AOD time series of volcanic eruptions
#' @param n length of prediction interval in years (defaults to 10 years)
#' @param clim length of lead-in climatology to compute the forecasts
#' @param skill vector of skills of forecast (see details)
#'
#' @details
#' The old function used a skill definition in fraction
#' of standard deviation, the new function defines skill as the fraction of
#' variance explained in the forecast period. Otherwise the two functions 
#' are identical.
#' 
#' @examples
#' ## set up a time series of pseudo-observations and plot
#' nn <- 200
#' ## dates of virtual eruptions
#' erup.i <- sort(ceiling(runif(10, min=0, max=nn)))
#' ## indices of effect of volcanoes
#' iafter <- outer(erup.i, 1:3, '+')
#' iafter <- iafter[iafter <= length(xx)]
#' xx <- rnorm(nn)
#' xx[iafter] <- rnorm(length(iafter), sd=2)
#' plot(xx, type='l', xlab='time', ylab='temperature')
#' abline(v=erup.i, lty=3)
#' 
#' ## compute the scores
#' fscore <- comp_score(xx, n=10, clim=1, erup.i=erup.i, skill=seq(0.05,0.95,0.1))
#' 
#' @return array of forecast scores
#' 
#' @keywords utilities
#' @export
comp_score <- function(x, aod=NULL, n=10, clim=10, skill=0){
  ## first put time dimension first
  xdims <- dim(x)
  nseas <- median(table(floor(attr(x, 'time'))))
  nclim <- clim*nseas
  xtmp <- as.vector(t(collapse2mat(x, first=F)))
  ## compute the base climatology
  xmn <- apply.NetCDF(x, seq(dim(x))[-length(dim(x))], filter, rep(1/clim, clim), sides=1)
  climbase <- as.vector(t(collapse2mat(xmn, first=F)))  
  ## compute indices of forecasts in forecast vector
  xind <- outer(seq(along=xtmp), 1:(n*nseas), '+')
  xind[(n + rep(attr(x, 'time'), length=length(xtmp))) > max(attr(x, 'time')), ] <- NA
  climind <- outer(seq(along=xtmp), rep(1:nseas, n) - nseas, '+')
  climind[(rep(attr(x, 'time'), length=length(xtmp)) - clim + 1 < min(attr(x, 'time')))] <- NA
  xa2 <- (array(xtmp[xind], dim(xind)) - array(climbase[climind], dim(xind)))**2
  ## compute scores without volcanic influence
  opt <- array(sqrt((1 - skill) %*% t(apply(xa2, 1, mean))), c(length(skill), xdims[length(xdims)], xdims[-length(xdims)]))
  opt <- aperm(opt, c(1, 3:(length(xdims)+1), 2))
  ## write attributes to forecast scores
  atns <- names(attributes(x))[-grep('dim', names(attributes(x)))]
  for (atn in atns) attr(opt, atn) <- attr(x, atn)
  ## compute skill if volcanic eruption is present
  if (!is.null(aod)){
    ei <- rep(c(FALSE, diff(aod[1,]) > 0), length=length(xtmp))
    eind <- t(apply(array(ei[xind], dim(xind)), 1, function(x) cumsum(x) == 0))
    pess <- array(NA, c(length(skill), xdims[length(xdims)], prod(xdims[-length(xdims)])))
    for (si in seq(along=skill)){
      pess[si,,] <- sqrt(apply(xa2 * (1 - skill[si]*eind), 1, mean))
    }
    pess <- aperm(pess, c(1,3,2))
    attributes(pess) <- attributes(opt)
    ## compute cumulative aod in climatology
    climaod <- filter(aod[1,], rep(1/nclim, nclim), sides=1)
    erupaod <- c(climaod[-(1:nclim)], rep(NA, nclim))
    attributes(climaod) <- attributes(erupaod) <- attributes(aod)
  }
    
  ## write output  
  olist <- list(optimistic=opt)
  if (!is.null(aod)){
    olist$pessimistic <- pess
    olist$erup.i <- ei[seq(climaod)]
    olist$climaod <- climaod
    olist$erupaod <- erupaod
  }
  
  return(olist)
}

#' @rdname compute_score
#' @param xin input object of class NetCDF containing climate variable
#' @param aod AOD time series of volcanic eruptions (eruptions are positive values)
#' @param ... additional arguments passed to \code{\link{comp_score}}
#' @export
#' 
all_scores <- function(xin, aod=NULL, ...){
  ## check that corresponding timesteps have been selected
  if (!is.null(aod)){
    xtime <- attr(xin, 'time')
    atime <- attr(aod, 'time')
    if (sum(xtime %in% atime) < 1){
      stop('No corresponding time steps in AOD and input time series')
    }
    if (length(xtime) != length(atime) | ! all(xtime %in% atime) | ! all(atime %in% xtime)){
      ctime <- floor(xtime[xtime %in% atime])
      xin <- select_time(xin, ctime[1], ctime[length(ctime)])
      aod <- select_time(aod, ctime[1], ctime[length(ctime)])
    }    
  }
    
  scores <- comp_score(x=xin, aod=aod, ...)
  return(scores)
}

#' @rdname compute_score
#' @export
get_eruptions <- function(aod){
  if (nrow(aod) > 1) stop('AOD is not single time series')
  ei <- which(diff(aod[1,]) > 0)
  erup.i <- ei[unique(c(1, which(diff(ei) > 1) + 1))] + 1
  return(erup.i)
}
