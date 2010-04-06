\name{archetypes}
\alias{archetypes}
\title{Perform archetypal analysis on a data matrix.}
\usage{archetypes(data, k, weights, maxIterations=100,
    minImprovement=sqrt(.Machine$double.eps), maxKappa=1000,
    verbose=FALSE, saveHistory=TRUE,
    family=archetypesFamily("original"), ...)
robustArchetypes(data, k, familyBlocks=list(), ...)
weightedArchetypes(data, k, weights, familyBlocks=list(), ...)
}
\description{Perform archetypal analysis on a data matrix.}
\value{\code{archetypes}: An object of class \code{archetypes}, see
\code{\link{archetypes-class}}.

\code{robustArchetypes}: An object of class \code{robustArchetypes} and
\code{\link{archetypes-class}}.


\code{weightedArchetypes}: An object of class \code{weightedArchetypes} and
\code{\link{archetypes-class}}.


}
\seealso{\code{\link{stepArchetypes}}, \code{\link{archetypes-class}}}
\references{Cutler and Breiman. Archetypal Analysis. Technometrics,
36(4), 1994. 338-348.}
\note{Please see the vignette for a detailed explanation!}
\arguments{\item{data}{A numeric \eqn{n \times m} data matrix.}
\item{k}{The number of archetypes.}
\item{weights}{Data weights matrix or vector (used as elements of
the diagonal weights matrix).}
\item{maxIterations}{The maximum number of iterations.}
\item{minImprovement}{The minimal value of improvement between two
iterations.}
\item{maxKappa}{The limit of kappa to report an ill-ness warning.}
\item{verbose}{Print some details during execution.}
\item{saveHistory}{Save each execution step in an environment for
further analyses.}
\item{family}{Blocks defining the underlying problem solving mechanisms;
see \code{\link{archetypesFamily}}.}
\item{...}{Additional arguments for family blocks.}
\item{familyBlocks}{Exchange predefined family blocks.}
}
\examples{data(toy)
a <- archetypes(toy, 3)}
\alias{robustArchetypes}
\alias{weightedArchetypes}

