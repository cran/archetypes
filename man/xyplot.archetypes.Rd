\name{xyplot.archetypes}
\alias{xyplot.archetypes}
\title{Plot of two-dimensional data and archetypes.}
\usage{
  \method{xyplot}{archetypes} (x, y, data.col = 1,
    data.pch = 19, data.bg = NULL, atypes.col = 2,
    atypes.pch = 19, ahull.show = TRUE,
    ahull.col = atypes.col, chull = NULL,
    chull.col = gray(0.7), chull.pch = 19,
    adata.show = FALSE, adata.col = 3, adata.pch = 13,
    link.col = data.col, link.lty = 1, ...)
}
\arguments{
  \item{x}{An \code{\link{archetypes}} object.}

  \item{y}{A matrix or data frame.}

  \item{data.col}{Color of data points.}

  \item{data.pch}{Type of data points.}

  \item{data.bg}{Background of data points.}

  \item{atypes.col}{Color of archetypes points.}

  \item{atypes.pch}{Type of archetypes points.}

  \item{ahull.show}{Show approximated convex hull.}

  \item{ahull.col}{Color of approximated convex hull line.}

  \item{chull}{An integer vector giving the indices of the
  points from \code{data} lying on the convex hull.}

  \item{chull.col}{Color of convex hull points.}

  \item{chull.pch}{Type of convex hull points.}

  \item{adata.show}{Show approximated data with link to the
  original data.}

  \item{adata.col}{Color of approximated data points.}

  \item{adata.pch}{Type of approximated data points.}

  \item{link.col}{Color of link between approximated and
  original data points.}

  \item{link.lty}{Line type of link between approximated
  and original data points.}

  \item{...}{Passed to the underlying plot functions.}
}
\value{
  Undefined.
}
\description{
  Plot of two-dimensional data and archetypes.
}
\note{
  The link between approximated and original data is based
  on an idea and Matlab source code of Bernard Pailthorpe.
}

