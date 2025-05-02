\name{xyplot.weightedArchetypes}
\alias{xyplot.weightedArchetypes}
\title{Plot of two-dimensional data and weighted archetypes.}
\usage{
\method{xyplot}{weightedArchetypes}(x, y, data.col = 1, data.pch = 21,
  data.bg = gray, link.col = NULL, link.lty = NULL,
  weights.type = "weights", ...)
}
\arguments{
  \item{x}{An \code{\link{archetypes}} object.}

  \item{y}{A matrix or data frame.}

  \item{data.col}{Color of data points.}

  \item{data.pch}{Type of data points.}

  \item{data.bg}{Background of data points.}

  \item{link.col}{Color of link between approximated and
  original data points.}

  \item{link.lty}{Line type of link between approximated
  and original data points.}

  \item{weights.type}{Weights to display; see
  \code{\link{weights.archetypes}}.}

  \item{...}{Arguments of \code{\link{xyplot.archetypes}}.}
}
\description{
Plot of two-dimensional data and weighted archetypes.
}

