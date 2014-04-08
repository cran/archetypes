\name{barplot.archetypes}
\alias{barplot.archetypes}
\title{Barplot of archetypes.}
\usage{
\method{barplot}{archetypes}(height, data, which = c("below", "beside"),
  which.beside = c("atypes", "variables"), which.below = c("compressed",
  "default"), percentiles = FALSE, below.compressed.height = 0.1,
  below.compressed.srt = 0, col.atypes = NULL, ...)
}
\arguments{
  \item{height}{An \code{\link{archetypes}} object.}

  \item{data}{The original data matrix.}

  \item{which}{\code{below} creates a barplot for each
  archetype, \code{beside} creates one barplot with bars
  side by side.}

  \item{which.beside}{Barplot according to \code{atypes} or
  \code{variables}.}

  \item{which.below}{\code{compressed} plots the labels
  only once.}

  \item{percentiles}{Show real values or percentile
  profiles.}

  \item{below.compressed.height}{Height of additional tail
  subplot.}

  \item{below.compressed.srt}{Rotations of the x-labels.}

  \item{col.atypes}{Color of archetypes; only used in
  \code{below.compressed}.}

  \item{...}{Passed to the underlying \code{\link{barplot}}
  call.}
}
\value{
Undefined.
}
\description{
Barplot of archetypes.
}

