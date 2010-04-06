\name{stepArchetypes}
\alias{stepArchetypes}
\title{Runs archetypes algorithm repeatedly.}
\usage{stepArchetypes(..., k, nrep=3, method=archetypes, verbose=TRUE)
\method{[}{stepArchetypes}(x, i)
\method{summary}{stepArchetypes}(object, ...)
\method{nparameters}{stepArchetypes}(object, ...)
\method{rss}{stepArchetypes}(object, ...)
\method{bestModel}{stepArchetypes}(object, ...)
}
\description{Runs archetypes algorithm repeatedly.}
\value{\code{stepArchetypes}: A list with \code{k} elements and class attribute
\code{stepArchetypes}. Each element is a list of class
\code{repArchetypes} with \code{nrep} elements; only for internal
usage.

\code{[.stepArchetypes}: A \code{stepArchetypes} object containing only the parts
defined in \code{i}.


\code{summary.stepArchetypes}: Undefined.


\code{nparameters.stepArchetypes}: Vector of numbers of archetypes.


\code{rss.stepArchetypes}: A vector of residual sum of squares.


\code{bestModel.stepArchetypes}: A list of length \code{k} of best models.


}
\seealso{\code{\link{archetypes}}}
\note{Please see the vignette for a detailed explanation!}
\arguments{\item{...}{Passed to \code{\link{archetypes}} function.}
\item{k}{A vector of integers passed in turn to the k argument of
\code{\link{archetypes}}.}
\item{nrep}{For each value of \code{k} run \code{\link{archetypes}}
\code{nrep} times.}
\item{method}{Archetypes function to use, typically
\code{\link{archetypes}}, \code{\link{weightedArchetypes}} or
\code{\link{robustArchetypes}},}
\item{verbose}{Show progress during exection.}
\item{x}{A \code{stepArchetypes} object.}
\item{i}{The indizes to extract.}
\item{object}{A \code{stepArchetypes} object.}
}
\examples{\dontrun{
data(skel)
skel2 <- subset(skel, select=-Gender)
as <- stepArchetypes(skel2, k=1:5, verbose=FALSE)

## Residual sum of squares curve:
screeplot(as)

## Select three archetypes and from that the best
## recurrence:
a3 <- bestModel(as[[3]])
}}
\alias{[.stepArchetypes}
\details{\code{[.stepArchetypes}: An extraction on a \code{stepArchetypes} object returns again a
\code{stepArchetypes} object.

}
\alias{summary.stepArchetypes}
\alias{nparameters.stepArchetypes}
\alias{rss.stepArchetypes}
\alias{bestModel.stepArchetypes}

