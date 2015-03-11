\name{compute_scores}
\alias{comp_score}
\alias{compute_scores}
\title{Compute forecast score}
\usage{
compute_score(x, n = 10, erup.i = NULL, skill = NULL)

comp_score(x, erup.i = NULL, n = 10, clim = 10, skill = 0)
}
\arguments{
  \item{x}{data vector, time series}

  \item{n}{length of prediction interval (defaults to 10
  years)}

  \item{erup.i}{indices in x with eruptions. This is the
  time the volcano erupts. The assumption is that the
  eruption will not affect this timestep, but will start
  affecting from the following timestep (make sure to use
  the correct index for monthly or seasonal data).}

  \item{skill}{vector of skills of forecast (see details)}

  \item{clim}{number of years leading the forecast used as
  baseline}
}
\value{
array of forecast scores
}
\description{
This function computes the residual variance in the
forecast interval. This is the variance unexplained by the
prediction in case the forecasting system has exactly zero
skill. In case the forecasting system has skill, only a
fraction of the residual variance is computed. Bounding
assumptions on how skill plays out with an eruption in the
forecast interval are computed. These bounding assumptions
are: i) skill is not influenced by the eruption
(optimistic), ii) all skill is lost after an eruption
(pessimistic).
}
\details{
The old function \code{compute_score} used a skill
definition in fraction of standard deviation. Otherwise the
two functions are identical.
}
\examples{
## set up a time series of pseudo-observations and plot
nn <- 200
## dates of virtual eruptions
erup.i <- sort(ceiling(runif(10, min=0, max=nn)))
## indices of effect of volcanoes
iafter <- outer(erup.i, 1:3, '+')
iafter <- iafter[iafter <= length(xx)]
xx <- rnorm(nn)
xx[iafter] <- rnorm(length(iafter), sd=2)
plot(xx, type='l', xlab='time', ylab='temperature')
abline(v=erup.i, lty=3)

## compute the scores
fscore <- comp_score(xx, n=10, clim=1, erup.i=erup.i, skill=seq(0.05,0.95,0.1))
}
\keyword{utilities}

