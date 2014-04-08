\name{panorama.archetypes}
\alias{panorama.archetypes}
\title{Panorma plot for archetypes.}
\usage{
\method{panorama}{archetypes}(object, data, distfn = distEuclidean,
  xlab = "Index", ylab = "Distance", order = TRUE, col = 1, pch = 1,
  cex = 1, atypes.col = (seq(length = nparameters(object)) + 1),
  atypes.pch = rep(19, nparameters(object)), atypes.cex = rep(1,
  nparameters(object)), ylim = NULL, ...)
}
\arguments{
  \item{object}{An \code{\link{archetypes}}-related
  object.}

  \item{data}{A matrix or data frame.}

  \item{distfn}{Distance function.}

  \item{xlab}{Label of xaxis.}

  \item{ylab}{Label of yaxis.}

  \item{order}{Order the distances.}

  \item{col}{Color of distances.}

  \item{pch}{Plot character of distances.}

  \item{cex}{magnification of the distances.}

  \item{atypes.col}{Color of archetype distances.}

  \item{atypes.pch}{Plot character of archetype distances.}

  \item{atypes.cex}{Magnification of the archetype
  distances.}

  \item{ylim}{The y limits of the plot.}

  \item{...}{Passed to the underlying \code{plot} call.}
}
\description{
Panorma plot for archetypes.
}
\examples{
\dontrun{
  data(toy)
  a <- archetypes(toy, 3)
  panorama(a, toy)

  ## See demo(robust-ozone).
  }
}

