\name{compute_score}
\alias{compute_score}
\title{Compute forecast score}
\usage{
compute_score(x, n = 10, erup.i = NULL, skill = NULL)
}
\arguments{
  \item{x}{data vector, time series}

  \item{n}{length of prediction interval (defaults to 10
  years)}

  \item{erup.i}{indices in x with eruptions}

  \item{skill}{vector of skills of forecast (see details)}
}
\value{
array of forecast scores
}
\description{
This function computes the residual variance in the
forecast interval
}
\details{
to be written
}
\examples{
## set up a time series of pseudo-observations and plot
xx <- rnorm(200)
erup.i <- sort(ceiling(runif(3, min=0, max=length(xx))))
xcolour <- rep(1, length(xx))
before.i <- outer(erup.i, 1:before, '-') ## check whether this is right
after.i <- outer(erup.i, 1:after, '+') ## check whether this is right
xcolour[after.i[after.i > 0 & after.i <= length(xx)]] <- 2
xcolour[before.i[before.i > 0 & before.i <= length(xx)]] <- 3
plot(xx, type='b', xlab='time', ylab='temperature', col=xcolour)
abline(v=erup.i, lty=3)

## compute the scores
fscore <- compute_score(xx, n=10, erup.i=erup.i, skill=skill.init)
vol.i <- outer(erup.i, 1:n, '-')
vol.i <- vol.i[vol.i > 0 & vol.i <= ncol(fscore)]
novol.i <- setdiff(seq(along=xx), vol.i)
novol.i <- novol.i[novol.i > 0 & novol.i <= ncol(fscore)]
hist(fscore[1,novol.i,50], freq=F, col='grey', xlim=range(pretty(fscore[1,,])))
hist(fscore[1,vol.i,], freq=F, add=T, density=15)
}
\keyword{utilities}

