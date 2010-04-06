\name{archetypesFamily}
\alias{archetypesFamily}
\title{Archetypes family constructor.}
\usage{archetypesFamily(which=c("original", "weighted", "robust"), ...)
}
\description{Archetypes family constructor.}
\details{This function returns a problem solving block for each of the
different conceptual parts of the algorithm.}
\value{A list containing a function for each of the different parts.}
\seealso{\code{\link{archetypes}}}
\arguments{\item{which}{The kind of archetypes family.}
\item{...}{Exchange predefined family blocks with self-defined
functions.}
}

