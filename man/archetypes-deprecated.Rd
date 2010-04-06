\name{archetypes-deprecated}
\title{Deprecated functions in archetypes package}
\description{These functions are provided for compatibility with older
  versions of archetypes only, and may be defunct as soon as the next
  release.}
\alias{atypes}
\usage{atypes(zs, ...)
ntypes(zs, ...)
adata(zs, ...)
alphas(zs, ...)
betas(zs, ...)
iters(zs, ...)
ahistory(zs, ...)
nhistory(zs, ...)
}
\details{\code{atypes}: replaced by \code{\link{parameters}}.

\code{ntypes}: replaced by \code{\link{nparameters}}.


\code{adata}: replaced by \code{\link{fitted}}.


\code{alphas}: replaced by \code{\link{coef}}.


\code{betas}: replaced by \code{\link{coef}}.


\code{iters}: removed.


\code{ahistory}: removed; see \code{\link{memento}}.


\code{nhistory}: removed; see \code{\link{memento}}.


}
\value{\code{atypes}: Archetypes matrix.

\code{ntypes}: Number of archetypes.


\code{adata}: Approximated data matrix.


\code{alphas}: Alpha matrix.


\code{betas}: Beta matrix.


\code{iters}: Number of iterations.


\code{ahistory}: The \code{archetypes} object of the requested step.


\code{nhistory}: The number of history steps available.


}
\arguments{\item{zs}{An \code{archetypes}-related object.}
\item{...}{Further arguments.}
}
\alias{ntypes}
\alias{adata}
\alias{alphas}
\alias{betas}
\alias{iters}
\alias{ahistory}
\alias{nhistory}

