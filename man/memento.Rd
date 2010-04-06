\name{memento}
\alias{new.memento}
\title{Memento environment.}
\description{Memento environment.}
\details{Simple implementation of the 'Memento' design pattern.}
\value{Memento environment.}
\alias{memento}
\arguments{\item{i}{The number of the state.}
\item{state}{The state to save.}
}
\examples{\dontrun{
m <- new.memento()
m$save(i, state)
m$states()
m$get(i)
}}

