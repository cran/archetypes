\name{pcplot.archetypes}
\alias{pcplot.archetypes}
\title{Parallel coordinates of data and archetypes.}
\usage{
\method{pcplot}{archetypes}(x, data, data.col = gray(0.7), data.lwd = 1,
  atypes.col = 2, atypes.lwd = 2, atypes.lty = 1, chull = NULL,
  chull.col = 1, chull.lwd = 2, chull.lty = 1, ...)
}
\arguments{
  \item{x}{An \code{\link{archetypes}} object.}

  \item{data}{A matrix or data frame.}

  \item{data.col}{Color of data lines.}

  \item{data.lwd}{Width of data lines.}

  \item{atypes.col}{Color of archetypes lines.}

  \item{atypes.lwd}{Width of archetypes lines.}

  \item{atypes.lty}{Type of archetypes lines.}

  \item{chull}{An integer vector giving the indices of the
  points from \code{data} lying on the convex hull.}

  \item{chull.col}{Color of convex hull lines.}

  \item{chull.lwd}{Width of convex hull lines.}

  \item{chull.lty}{Type of convex hull lines.}

  \item{...}{Passed to \code{\link{pcplot}} and
  \code{\link{lines.pcplot}}.}
}
\value{
Undefined.
}
\description{
Parallel coordinates of data and archetypes.
}

