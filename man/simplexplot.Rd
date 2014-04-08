\name{simplexplot}
\alias{simplexplot}
\title{Simplex visualization}
\usage{
simplexplot(object, radius = 10, order = NULL, labels_cex = 1,
  labels = NULL, show_labels = TRUE, points_col = "#00000044",
  points_pch = 19, points_cex = 1, projection = simplex_projection,
  show_points = TRUE, show_circle = TRUE, circle_col = "lightgray",
  show_edges = TRUE, edges_col = "lightgray", show_direction = FALSE,
  direction_length = 1, directions_col = points_col, ...)
}
\arguments{
  \item{object}{An \code{\link{archetypes}} object}

  \item{radius}{Radius of the projection}

  \item{order}{Order of the archetypes}

  \item{labels_cex}{Label expansion}

  \item{labels}{Labels}

  \item{show_labels}{Show labels}

  \item{points_col}{Color of the points}

  \item{points_pch}{Plot character of the points}

  \item{points_cex}{Character expansion of the points}

  \item{projection}{Projection function; see
  \code{\link{archmap_projections}}}

  \item{show_points}{Show the points}

  \item{show_circle}{Show the circle}

  \item{circle_col}{Color of the circle}

  \item{show_edges}{Show the edges}

  \item{edges_col}{Color of the edges}

  \item{direction_length}{Expansion of the direction
  pointers}

  \item{directions_col}{Color of the direction pointers}

  \item{show_direction}{Show direction pointers}

  \item{...}{Additional arguments; currently ignored}
}
\value{
Invisible list of all computed components needed for the
simplex visualization.
}
\description{
The stochastic nature of the alpha coefficients implies
that they exist on a standard (K-1)-simplex with the K
archetypes Z as the corners, and the coefficients as the
coordinate with respect to these corners. A standard
simplex can be projected to two dimensions via a skew
orthogonal projection, where all the vertices of the
simplex are shown on a circle connected by edges. The
individual alpha coefficients can be then projected into
this circle.
}
\examples{
### This example reproduces parts of the Figure 7 shown in
  ### "Probabilistic Archetypal Analysis" by Seth and Eugster (2014)

  data("toy", package = "archetypes")

  set.seed(1234); a3 <- archetypes(toy, k = 3)
  set.seed(1237); a4 <- archetypes(toy, k = 4)
  set.seed(1238); a5 <- archetypes(toy, k = 5)

  simplexplot(a3)
  simplexplot(a3, show_direction = TRUE, show_points = FALSE)
  simplexplot(a4, projection = tspsimplex_projection)
  simplexplot(a5, show_direction = TRUE, show_points = FALSE,
    direction_length = 2, directions_col = "black")
}
\references{
See Section 6 in "Probabilistic Archetypal Analysis" by
Seth and Eugster (2014), http://arxiv.org/abs/1312.7604.
}

