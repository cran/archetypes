\name{simplex_projection}
\alias{archmap_projections}
\alias{atypes_projection}
\alias{simplex_projection}
\alias{tspsimplex_projection}
\title{Archetypal map projections}
\usage{
simplex_projection(x, r = 10)

tspsimplex_projection(x, r = 10, equidist = FALSE, ...)

atypes_projection(x)
}
\arguments{
  \item{x}{Archetypes matrix}

  \item{r}{Radius of the simplex projection}

  \item{equidist}{Arrange archetypes equidistantly or in
  relation to their distance}

  \item{...}{Parameters for the
  \code{\link[TSP]{solve_TSP}} function}
}
\value{
Matrix with the projected archetypes
}
\description{
Archetypal map projections
}
\seealso{
Other archmap: \code{\link{archmap}}
}

