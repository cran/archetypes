\name{stepArchetypes}
\alias{stepArchetypes}
\title{Runs archetypes algorithm repeatedly.}
\usage{stepArchetypes(..., k, nrep=3, verbose=TRUE)}
\description{Runs archetypes algorithm repeatedly.}
\value{A list with \code{k} elements and class attribute
\code{stepArchetypes}. Each element is a list of class
\code{repArchetypes} with \code{nrep} elements; only for internal
usage.}
\seealso{\code{\link{atypes}}, \code{\link{ntypes}},
\code{\link{rss}}, \code{\link{adata}}, \code{\link{alphas}},
\code{\link{ahistory}}, \code{\link{nhistory}}}
\note{Please see the vignette for a detailed explanation!}
\arguments{\item{...}{Passed to \code{\link{archetypes}} function.}
\item{k}{A vector of integers passed in turn to the k argument of
\code{\link{archetypes}}.}
\item{nrep}{For each value of \code{k} run \code{\link{archetypes}}
\code{nrep} times.}
\item{verbose}{Show progress during exection.}}
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
