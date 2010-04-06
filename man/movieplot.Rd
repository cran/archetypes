\name{movieplot}
\alias{movieplot}
\title{Archetypes movies.}
\usage{movieplot(zs, data, show=c("atypes", "adata", "rwdata"), ssleep=0, bsleep=0,
    postfn=function(iter) { }, rwdata.col1=gray(0.7), rwdata.col2=2,
    ...)
movieplot2(zs, data, show="atypes", ssleep=0, bsleep=0, zas.col=2, zas.pch=13,
    old.col=rgb(1, 0.5, 0.5), ...)
moviepcplot(zs, data, show=c("atypes", "adata"), ssleep=0, bsleep=0, ...)
}
\description{Archetypes movies.}
\value{\code{movieplot}: Undefined.

\code{movieplot2}: Undefined.


\code{moviepcplot}: Undefined.


}
\alias{movieplot}
\arguments{\item{zs}{An \code{\link{archetypes}} object.}
\item{data}{The data matrix.}
\item{show}{Show archetypes or approximated data.}
\item{ssleep}{Seconds to sleep before start.}
\item{bsleep}{Seconds to sleep between each plot.}
\item{postfn}{Post plot function; is called in each
iteration after the plot call.}
\item{rwdata.col1}{If \code{show = 'rwdata'}: color of base data set.}
\item{rwdata.col2}{If \code{show = 'rwdata'}: color of weighted data set.}
\item{...}{Passed to underlying plot functions.}
\item{zas.col}{Color of the intermediate archetypes.}
\item{zas.pch}{Type of the intermediate archetypes points.}
\item{old.col}{Color of the archetypes on step further.}
}
\alias{movieplot2}
\details{\code{movieplot2}: Shows the intermediate steps of the algorithm;

}
\alias{moviepcplot}

