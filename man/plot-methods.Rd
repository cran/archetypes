\name{plot-methods}
\alias{plot.archetypes}
\alias{plot.stepArchetypes}
\title{Plot of data and archetypes.}
\usage{
\method{plot}{archetypes} (x, y, data.col=gray(0.7), data.pch=19,
    atypes.col=2, atypes.pch=19, ahull.show=TRUE, ahull.col=atypes.col,
    chull, chull.col=1, chull.pch=19, adata.show=FALSE, adata.col=3,
    adata.pch=13, link.col=data.col, ...)
\method{plot}{stepArchetypes} (x, y, data.col=gray(0.7), data.pch=19,
    atypes.col=(seq_len(length(x) * length(x[[1]])) + 1),
    atypes.pch=19, ahull.show=TRUE, ahull.col=atypes.col, ...)
}
\arguments{
  \item{x}{An \code{\link{archetypes}} or
    \code{\link{stepArchetypes}} object.}
  \item{y}{Data matrix or data frame.}
  \item{data.col}{Color of data points.}
  \item{data.pch}{Type of data points.}
  \item{atypes.col}{Color of archetypes points.}
  \item{atypes.pch}{Type of archetypes points.}
  \item{ahull.show}{Show approximated convex hull.}
  \item{ahull.col}{Color of approximated convex hull line.}
  \item{chull}{An integer vector giving the indices of the points from
    \code{data} lying on the convex hull.}
  \item{chull.col}{Color of convex hull points.}
  \item{chull.pch}{Type of convex hull points.}
  \item{adata.show}{Show approximated data with link to the original
    data.}
  \item{adata.col}{Color of approximated data points.}
  \item{adata.pch}{Type of approximated data points.}
  \item{link.col}{Color of link between approximated and original data
    points.}
  \item{...}{Passed to the underlying plot functions.}
}
\description{
  Plot for two-dimensional data and archetypes.
}
\note{The link between approximated and original data is based on an
  idea and Matlab source code of Bernard Pailthorpe.}
\value{Undefined.}
