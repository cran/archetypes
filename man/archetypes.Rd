\name{archetypes}
\alias{archetypes}
\title{Perform archetypal analysis on a data matrix.}
\usage{
  archetypes(data, k, weights = NULL, maxIterations = 100,
    minImprovement = sqrt(.Machine$double.eps),
    maxKappa = 1000, verbose = FALSE, saveHistory = TRUE,
    family = archetypesFamily("original"), ...)
}
\arguments{
  \item{data}{A numeric \eqn{n \times m} data matrix.}

  \item{k}{The number of archetypes.}

  \item{weights}{Data weights matrix or vector (used as
  elements of the diagonal weights matrix).}

  \item{maxIterations}{The maximum number of iterations.}

  \item{minImprovement}{The minimal value of improvement
  between two iterations.}

  \item{maxKappa}{The limit of kappa to report an ill-ness
  warning.}

  \item{verbose}{Print some details during execution.}

  \item{saveHistory}{Save each execution step in an
  environment for further analyses.}

  \item{family}{Blocks defining the underlying problem
  solving mechanisms; see \code{\link{archetypesFamily}}.}

  \item{...}{Additional arguments for family blocks.}
}
\value{
  An object of class \code{archetypes}, see
  \code{\link{as.archetypes}}.
}
\description{
  Perform archetypal analysis on a data matrix.
}
\examples{
data(toy)
  a <- archetypes(toy, 3)
}
\references{
  Cutler and Breiman. Archetypal Analysis. Technometrics,
  36(4), 1994. 338-348.
}
\seealso{
  Other archetypes: \code{\link{archetypesFamily}},
  \code{\link{as.archetypes}},
  \code{\link{robustArchetypes}},
  \code{\link{weightedArchetypes}}
}

