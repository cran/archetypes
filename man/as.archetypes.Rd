\name{as.archetypes}
\alias{as.archetypes}
\title{Archetypes object constructor}
\usage{
as.archetypes(object, k, alphas, rss, iters = NULL, call = NULL,
  history = NULL, kappas = NULL, betas = NULL, zas = NULL,
  family = NULL, familyArgs = NULL, residuals = NULL, weights = NULL,
  reweights = NULL, scaling = NULL)
}
\arguments{
  \item{object}{The archetypes; a \eqn{p \times m} matrix,
  see \code{\link{parameters}}.}

  \item{k}{The number of archetypes;}

  \item{alphas}{The coefficients; a \eqn{n \times p}
  matrix, see \code{\link{coef}}.}

  \item{rss}{The residual sum of squares; see
  \code{\link{rss.archetypes}}.}

  \item{iters}{The number of iterations to the
  convergence.}

  \item{call}{The call of the \code{\link{archetypes}}
  function.}

  \item{history}{If \code{saveHistory} set then an
  environment with the archetypes object for each execution
  step;}

  \item{kappas}{The kappas for each system of linear
  equations.}

  \item{betas}{The data coefficients; a \eqn{p \times n}
  matrix.}

  \item{zas}{The temporary archetypes.}

  \item{family}{The archetypes family.}

  \item{familyArgs}{Additional arguments for family
  blocks.}

  \item{residuals}{The residuals.}

  \item{weights}{The data weights.}

  \item{reweights}{The data reweights.}

  \item{scaling}{The scaling parameters of the data.}
}
\value{
A list with an element for each parameter and class
attribute \code{archetypes}.
}
\description{
Archetypes object constructor
}
\seealso{
Other archetypes: \code{\link{archetypesFamily}};
\code{\link{archetypes}}; \code{\link{robustArchetypes}};
\code{\link{weightedArchetypes}}
}

