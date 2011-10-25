\name{xyplot.stepArchetypes}
\alias{xyplot.stepArchetypes}
\title{Plot of two-dimensional data and stepArchetypes.}
\usage{
  \method{xyplot}{stepArchetypes} (x, y,
    data.col = gray(0.7), data.pch = 19,
    atypes.col = (seq_len(length(x) * length(x[[1]])) + 1),
    atypes.pch = 19, ahull.show = TRUE,
    ahull.col = atypes.col, ...)
}
\arguments{
  \item{x}{An \code{\link{stepArchetypes}} object.}

  \item{y}{A matrix or data frame.}

  \item{data.col}{Color of data points.}

  \item{data.pch}{Type of data points.}

  \item{atypes.col}{Color of archetypes points.}

  \item{atypes.pch}{Type of archetypes points.}

  \item{ahull.show}{Show approximated convex hull.}

  \item{ahull.col}{Color of approximated convex hull line.}

  \item{...}{Passed to the underlying plot functions.}
}
\value{
  Undefined.
}
\description{
  Plot of two-dimensional data and stepArchetypes.
}

