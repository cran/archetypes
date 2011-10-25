\name{pcplot.default}
\alias{pcplot.default}
\title{Default parallel coordinates plot.}
\usage{
  \method{pcplot}{default} (x, col = gray(0.7), lty = 1,
    var.label = TRUE, rx = NULL, ...)
}
\arguments{
  \item{x}{A \eqn{n \times m} matrix or data frame who
  columns represent variables. Missing values are allowed.}

  \item{col}{Line color.}

  \item{lty}{Line type.}

  \item{var.label}{Axes labels.}

  \item{rx}{A \eqn{2 \times m} matrix with ranges for each
  dimension.}

  \item{...}{Passed to the underlying
  \code{\link[graphics]{matplot}} function.}
}
\value{
  Undefined.
}
\description{
  Code copied from function \code{\link[MASS]{parcoord}} of
  package \code{MASS} to simply play arround with the
  visualization of archetypes. At a later date, when it is
  clear which visualisation is the best, the functionality
  is probabibly merged with the original function or it is
  possible with parallel coordinate plots which are
  available et all.
}

