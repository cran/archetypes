\name{lines.pcplot}
\alias{lines.pcplot}
\title{Add lines to an existing parallel coordinates plot.}
\usage{
\method{lines}{pcplot}(x, data, col = 1, lty = 1, ...)
}
\arguments{
  \item{x}{A matrix or data frame containing the additional
  data.}

  \item{data}{The data of the existing parallel coordinates
  plot.}

  \item{col}{Line colors.}

  \item{lty}{Line types.}

  \item{...}{Passed to underlying
  \code{\link[graphics]{matlines}}.}
}
\value{
Undefined.
}
\description{
Add lines to an existing parallel coordinates plot.
}

