\name{movieplot}
\title{Archetypes plot movie.}
\usage{movieplot(zs, data, show=c('atypes', 'adata'),
ssleep=0, bsleep=0, postfn=function(iter){}, ...)}
\description{Archetypes plot movie.}
\value{Undefined.}
\alias{movieplot}
\arguments{\item{zs}{An \code{\link{archetypes}} object.}
\item{data}{The data matrix.}
\item{show}{Show archetypes or approximated data.}
\item{ssleep}{Seconds to sleep before start.}
\item{bsleep}{Seconds to sleep between each plot.}
\item{postfn}{Post plot function; is called in each
iteration after the plot call.}
\item{...}{Passed to underlying plot functions.}}
