\name{as.archetypes}
\alias{as.archetypes}
\title{Archetypes object constructor.}
\usage{as.archetypes(archetypes, k, alphas, rss, iters, call, history, kappas)}
\description{Archetypes object constructor.}
\value{A list with an element for each parameter and class attribute
\code{archetypes}.}
\seealso{\code{\link{archetypes}}, \code{\link{atypes}}, \code{\link{ntypes}},
\code{\link{rss}}, \code{\link{adata}}, \code{\link{alphas}},
\code{\link{ahistory}}, \code{\link{nhistory}}}
\arguments{\item{archetypes}{The archetypes; a $p \times m$ matrix, see
\code{\link{atypes}}.}
\item{k}{The number of archetypes;}
\item{alphas}{The coefficients; a $n \times p$ matrix, see
\code{\link{alphas}}.}
\item{rss}{The residual sum of squares; see \link{rss}.}
\item{iters}{The number of iterations to the convergence.}
\item{call}{The call of the \code{\link{archetypes}} function.}
\item{history}{If \code{saveHistory} set then an environment with the
archetypes object for each execution step;}
\item{kappas}{The kappas for each system of linear equations.}}
