\name{pcplot-methods}
\alias{pcplot}
\alias{pcplot.default}
\alias{pcplot.archetypes}
\title{Parallel coordinates plot.}
\usage{
pcplot(x, ...)
\method{pcplot}{default} (x, col=gray(0.7), lty=1, var.label=TRUE,
    rx=NULL, ...)
\method{pcplot}{archetypes} (x, data, data.col=gray(0.7), data.lwd=1,
    atypes.col=2, atypes.lwd=2, atypes.lty=1, chull, chull.col=1,
    chull.lwd=2, chull.lty=1, ...)
}
\description{Parallel coordinates plot.}
\arguments{
  \item{x}{An data.frame or a \code{\link{archetypes}} object.}
  \item{col}{Line color.}
  \item{lty}{Line type.}
  \item{var.label}{Axes labels.}
  \item{rx}{A $2 \times m$ matrix with ranges for each dimension.}
  \item{data}{A matrix or data frame.}
  \item{data.col}{Color of data lines.}
  \item{data.lwd}{Width of data lines.}
  \item{atypes.col}{Color of archetypes lines.}
  \item{atypes.lwd}{Width of archetypes lines.}
  \item{atypes.lty}{Type of archetypes lines.}
  \item{chull}{An integer vector giving the indices of the points from
    \code{data} lying on the convex hull.}
  \item{chull.col}{Color of convex hull lines.}
  \item{chull.lwd}{Width of convex hull lines.}
  \item{chull.lty}{Type of convex hull lines.}
  \item{...}{Passed to the underlying plot functions.}
}
\details{Code copied from function \code{\link[MASS]{parcoord}} of
  package \code{MASS} to simply play arround with the visualization of
  archetypes. At a later date, when it is clear which visualisation is
  the best, the functionality is probabibly merged with the original
  function or it is possible with parallel coordinate plots which are
  available et all.
}
\value{Undefined.}
