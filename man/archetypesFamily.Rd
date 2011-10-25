\name{archetypesFamily}
\alias{archetypesFamily}
\title{Archetypes family constructor}
\usage{
  archetypesFamily(which = c("original", "weighted", "robust"),
    ...)
}
\arguments{
  \item{which}{The kind of archetypes family.}

  \item{...}{Exchange predefined family blocks with
  self-defined functions.}
}
\value{
  A list containing a function for each of the different
  parts.
}
\description{
  This function returns a problem solving block for each of
  the different conceptual parts of the algorithm.
}
\seealso{
  Other archetypes: \code{\link{archetypes}},
  \code{\link{as.archetypes}},
  \code{\link{robustArchetypes}},
  \code{\link{weightedArchetypes}}
}

