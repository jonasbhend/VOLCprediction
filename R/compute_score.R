#' @name compute_scores
#' @aliases comp_score
#' 
#' @title
#' Compute forecast score
#' 
#' @description
#' This function computes the residual variance in the forecast interval
#' 
#' @param x data vector, time series
#' @param n length of prediction interval (defaults to 10 years)
#' @param erup.i indices in x with eruptions
#' @param skill vector of skills of forecast (see details)
#'
#' @details
#' to be written
#' 
#' @examples
#' ## set up a time series of pseudo-observations and plot
#' xx <- rnorm(200)
#' erup.i <- sort(ceiling(runif(3, min=0, max=length(xx))))
#' xcolour <- rep(1, length(xx))
#' before.i <- outer(erup.i, 1:before, '-') ## check whether this is right
#' after.i <- outer(erup.i, 1:after, '+') ## check whether this is right
#' xcolour[after.i[after.i > 0 & after.i <= length(xx)]] <- 2
#' xcolour[before.i[before.i > 0 & before.i <= length(xx)]] <- 3
#' plot(xx, type='b', xlab='time', ylab='temperature', col=xcolour)
#' abline(v=erup.i, lty=3)
#' 
#' ## compute the scores
#' fscore <- compute_score(xx, n=10, erup.i=erup.i, skill=skill.init)
#' vol.i <- outer(erup.i, 1:n, '-')
#' vol.i <- vol.i[vol.i > 0 & vol.i <= ncol(fscore)]
#' novol.i <- setdiff(seq(along=xx), vol.i)
#' novol.i <- novol.i[novol.i > 0 & novol.i <= ncol(fscore)]
#' hist(fscore[1,novol.i,50], freq=F, col='grey', xlim=range(pretty(fscore[1,,])))
#' hist(fscore[1,vol.i,], freq=F, add=T, density=15)
#' 
#' @return array of forecast scores
#' 
#' @keywords utilities
#' @export
compute_score <- function(x,n=10, erup.i=NULL, skill=NULL){
  
  nx      <- length(x)
  if (is.null(erup.i) | is.null(skill)){
    score   <- rep(NA, (nx - n))
    for (i in 1:(nx-n)){
      score[i] <- sqrt(mean((x[i+1:n]-x[i])**2))
    }
  } else {
    ns      <- length(skill)
    score   <- array(NA, c(2,nx-n, length(skill)))
    skill   <- (1-skill)
    index   <- rep(1, length(skill))
    for (i in 1:(nx-n)){
      tmp         <- x[i+1:n]-x[i] 
      temp        <- tmp %*% t(skill)
      min.erup    <- (intersect(1:n, erup.i-i))[1]
      score[1,i,] <- sqrt(apply(temp**2, 2, mean))
      if (!is.na(min.erup)){
        temp[min.erup:n,] <- tmp[min.erup:n] %*% t(index)
      }
      score[2,i,] <- sqrt(apply(temp**2, 2, mean))
    }
    dimnames(score) <- list(c('optimistic', 'pessimistic'), names(x), skill)
  }
  if (!is.null(skill)) attr(score, 'skill') <- 1-skill
  score
}

#'@rdname compute_scores
#'@param clim number of years leading the forecast used as baseline
#'@export
comp_score <- function(x, n=10, clim=5){
  climbase <- filter(x, rep(1/clim, clim), sides=1)
  xind <- outer(seq(along=x), 1:n, '+')
  xind[apply(xind > length(x), 1, any)] <- NA
  xanom <- array(x[xind], dim(xind)) - climbase[rep(seq(along=climbase), n)]
  score <- sqrt(apply(xanom**2, 1, mean))
  return(score)
}


