\name{archmap}
\alias{archmap}
\title{Archetypal maps}
\usage{
  archmap(object, projection = simplex_projection,
    projection_args = list(), rotate = 0, cex = 1.5,
    col = 1, pch = 1, xlab = "", ylab = "", axes = FALSE,
    asp = TRUE, ...)
}
\arguments{
  \item{object}{An \code{\link{archetypes}} object}

  \item{projection}{Projection function; see
  \code{\link{archmap_projections}}}

  \item{projection_args}{Arguments passed to the projection
  function; see \code{\link{archmap_projections}}}

  \item{rotate}{Rotation angle to rotate the projection}

  \item{cex}{Character expansion of archetypes}

  \item{col}{Color of observations}

  \item{pch}{Point character of observations}

  \item{xlab}{A label for the x-axis}

  \item{ylab}{A label for the y-axis}

  \item{axes}{Logical value to draw axes or not}

  \item{asp}{The y/x aspect ratio}

  \item{...}{Arguments passed to the underlying plot
  function}
}
\value{
  Invisible matrix with the projected archetypes
}
\description{
  Two-dimensional projection of the observations based on
  the alpha coefficients into a space spanned by the
  (projected) archetypes.
}
\examples{
data("skel", package = "archetypes")
  skel2 <- subset(skel, select = -Gender)

  set.seed(1981)
  a <- archetypes(skel2, k = 5)

  ## Simplex projection:
  archmap(a, col = skel$Gender)

  ## Simplex projection with archetypes arranged according to their
  ## distances:
  archmap(a, col = skel$Gender,
          projection = tspsimplex_projection)
  archmap(a, col = skel$Gender,
          projection = tspsimplex_projection,
          projection_args = list(equidist = TRUE))

  ## MDS projection:
  archmap(a, col = skel$Gender,
          projection = atypes_projection)
}
\seealso{
  Other archmap: \code{\link{atypes_projection}},
  \code{\link{simplex_projection}},
  \code{\link{tspsimplex_projection}}
}

