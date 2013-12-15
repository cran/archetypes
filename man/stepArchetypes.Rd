\name{stepArchetypes}
\alias{stepArchetypes}
\title{Run archetypes algorithm repeatedly}
\usage{
  stepArchetypes(..., k, nrep = 3, method = archetypes,
    verbose = TRUE)
}
\arguments{
  \item{...}{Passed to the specific archetype function.}

  \item{k}{A vector of integers passed in turn to the k
  argument of \code{\link{archetypes}}.}

  \item{nrep}{For each value of \code{k} run
  \code{\link{archetypes}} \code{nrep} times.}

  \item{method}{Archetypes function to use, typically
  \code{\link{archetypes}},
  \code{\link{weightedArchetypes}} or
  \code{\link{robustArchetypes}},}

  \item{verbose}{Show progress during exection.}
}
\value{
  A list with \code{k} elements and class attribute
  \code{stepArchetypes}. Each element is a list of class
  \code{repArchetypes} with \code{nrep} elements; only for
  internal usage.
}
\description{
  Run archetypes algorithm repeatedly
}
\examples{
\dontrun{
  data(skel)
  skel2 <- subset(skel, select=-Gender)
  as <- stepArchetypes(skel2, k=1:5, verbose=FALSE)

  ## Residual sum of squares curve:
  screeplot(as)

  ## Select three archetypes and from that the best
  ## recurrence:
  a3 <- bestModel(as[[3]])
  }
}
\seealso{
  \code{\link{archetypes}}
}

