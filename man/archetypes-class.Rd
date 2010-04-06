\name{archetypes-class}
\alias{as.archetypes}
\title{Archetypes object constructor and methods.}
\usage{as.archetypes(archetypes, k, alphas, rss, iters, call, history, kappas, betas, zas,
    family, familyArgs, residuals, weights, reweights)
\method{fitted}{archetypes}(object, ...)
\method{coef}{archetypes}(object, type=c("alphas", "betas"), ...)
\method{residuals}{archetypes}(object, ...)
\method{rss}{archetypes}(object, type=c("scaled", "single", "global"), ...)
\method{weights}{archetypes}(object, type=c("weights", "reweights"), ...)
\method{kappa}{archetypes}(z, ...)
\method{nparameters}{archetypes}(object, ...)
}
\description{Archetypes object constructor and methods.}
\value{\code{as.archetypes}: A list with an element for each parameter and class attribute
\code{archetypes}.

\code{fitted.archetypes}: Matrix with approximated data.


\code{parameters,archetypes-method}: Matrix with \eqn{k} archetypes.


\code{coef.archetypes}: Coefficient matrix.


\code{residuals.archetypes}: Matrix with residuals.


\code{rss.archetypes}: Residual sum of squares.


\code{weights.archetypes}: Vector of weights.


\code{kappa.archetypes}: A vector of kappas.


\code{nparameters.archetypes}: Number of archetypes.


}
\seealso{\code{\link{archetypes}}}
\alias{archetypes-class}
\arguments{\item{archetypes}{The archetypes; a \eqn{p \times m} matrix, see
\code{\link{atypes}}.}
\item{k}{The number of archetypes;}
\item{alphas}{The coefficients; a \eqn{n \times p} matrix, see
\code{\link{alphas}}.}
\item{rss}{The residual sum of squares; see \link{rss}.}
\item{iters}{The number of iterations to the convergence.}
\item{call}{The call of the \code{\link{archetypes}} function.}
\item{history}{If \code{saveHistory} set then an environment with the
archetypes object for each execution step;}
\item{kappas}{The kappas for each system of linear equations.}
\item{betas}{The data coefficients; a \eqn{p \times n} matrix.}
\item{zas}{The temporary archetypes.}
\item{family}{The archetypes family.}
\item{familyArgs}{Additional arguments for family blocks.}
\item{residuals}{The residuals.}
\item{weights}{The data weights.}
\item{reweights}{The data reweights.}
\item{object}{An \code{archetypes}-related object.}
\item{...}{Ignored.}
\item{type}{Return alphas or betas.}
\item{z}{An \code{archetypes} object.}
}
\alias{fitted.archetypes}
\alias{parameters,archetypes-method}
\alias{parameters}
\alias{coef.archetypes}
\alias{residuals.archetypes}
\alias{rss.archetypes}
\alias{weights.archetypes}
\alias{kappa.archetypes}
\alias{nparameters.archetypes}

