\name{predict.archetypes}
\alias{predict.archetypes}
\title{Predict method for archetypal analysis fits}
\usage{
  \method{predict}{archetypes} (object, newdata, ...)
}
\arguments{
  \item{object}{An \code{archetypes} object; currently only
  \code{\link[=archetypesFamily]{original}}-family
  objects.}

  \item{newdata}{A data frame with data for which to
  predict the alpha coefficients.}

  \item{...}{Ignored.}
}
\value{
  The predict alpha coefficients.
}
\description{
  This method produces predicted alpha coefficients for new
  data.
}

