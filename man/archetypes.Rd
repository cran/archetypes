\name{archetypes}
\alias{archetypes}
\title{Perform archetypal analysis on a data matrix.}
\usage{archetypes(data, k, maxIterations=100,
    minImprovement=sqrt(.Machine$double.eps), maxKappa=1000,
    verbose=TRUE, saveHistory=TRUE, family=archetypesFamily("default"))}
\description{Perform archetypal analysis on a data matrix.}
\value{An object of class \code{\link{archetypes}}, see
\code{\link{as.archetypes}}.}
\seealso{\code{\link{stepArchetypes}}}
\references{Cutler and Breiman. Archetypal Analysis. Technometrics,
36(4), 1994. 338-348.}
\note{Please see the vignette for a detailed explanation!}
\arguments{\item{data}{A numeric $n \times m$ data matrix.}
\item{k}{The number of archetypes.}
\item{maxIterations}{The maximum number of iterations.}
\item{minImprovement}{The minimal value of improvement between two
iterations.}
\item{maxKappa}{The limit of kappa to report an ill-ness warning.}
\item{verbose}{Print some details during execution.}
\item{saveHistory}{Save each execution step in an environment for
further analyses.}
\item{family}{Blocks defining the underlying problem solving mechanisms;
see \code{\link{archetypesFamily}}.}}
\examples{data(toy)
a <- archetypes(toy, 3)}
