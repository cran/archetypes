\name{screeplot.stepArchetypes}
\alias{screeplot.stepArchetypes}
\title{Screeplot of stepArchetypes.}
\usage{
  \method{screeplot}{stepArchetypes} (x,
    type = c("lines", "barplot"), ...)
}
\arguments{
  \item{x}{A \code{\link{stepArchetypes}} object.}

  \item{type}{Draw lines or a barplot.}

  \item{...}{Passed to underlying plot functions.}
}
\value{
  Undefined.
}
\description{
  Screeplot draws the residual sum of square curve based on
  the best model of each step.
}

