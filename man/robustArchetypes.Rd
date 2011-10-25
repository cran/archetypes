\name{robustArchetypes}
\alias{robustArchetypes}
\title{Robust archetypes}
\usage{
  robustArchetypes(data, k, familyBlocks = list(), ...)
}
\arguments{
  \item{familyBlocks}{Exchange predefined family blocks;
  see \code{\link{archetypesFamily}}.}

  \item{data}{A numeric \eqn{n \times m} data matrix.}

  \item{k}{The number of archetypes.}

  \item{...}{Additional arguments for family blocks.}
}
\value{
  An object of class \code{robustArchetypes} and
  \code{\link{as.archetypes}}.
}
\description{
  Robust archetypes
}
\seealso{
  Other archetypes: \code{\link{archetypes}},
  \code{\link{archetypesFamily}},
  \code{\link{as.archetypes}},
  \code{\link{weightedArchetypes}}
}

