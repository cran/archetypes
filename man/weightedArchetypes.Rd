\name{weightedArchetypes}
\alias{weightedArchetypes}
\title{Weighted archetypes}
\usage{
  weightedArchetypes(data, k, weights = NULL,
    familyBlocks = list(), ...)
}
\arguments{
  \item{weights}{Data weights matrix.}

  \item{familyBlocks}{Exchange predefined family blocks;
  see \code{\link{archetypesFamily}}.}

  \item{data}{A numeric \eqn{n \times m} data matrix.}

  \item{k}{The number of archetypes.}

  \item{...}{Additional arguments for family blocks.}
}
\value{
  An object of class \code{weightedArchetypes} and
  \code{\link{as.archetypes}}.
}
\description{
  Weighted archetypes
}
\seealso{
  Other archetypes: \code{\link{archetypes}},
  \code{\link{archetypesFamily}},
  \code{\link{as.archetypes}},
  \code{\link{robustArchetypes}}
}

