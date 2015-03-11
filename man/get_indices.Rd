\name{get_indices}
\alias{get_indices}
\alias{index_with_eruption}
\alias{index_wo_eruption}
\alias{index_wo_recovery}
\title{Compute scores without eruption}
\usage{
index_wo_recovery(x, erup.i, k)

index_with_eruption(erup.i, n)

index_wo_eruption(x, erup.i, n)
}
\arguments{
  \item{x}{input vector or length of forecast sequence}

  \item{erup.i}{index of years with eruptions}

  \item{k}{waiting period after eruptions}

  \item{n}{length of forecast interval}
}
\description{
Function to find forecast scores without recovery from
eruptions in the prediction interval
}
\keyword{utilities}

