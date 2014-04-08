\name{skeletonplot}
\alias{jd}
\alias{skeletonplot}
\title{Skeleton plot.}
\usage{
skeletonplot(x, skel.width = 100, skel.height = 200, ylab = "Height (cm)",
  base.radius = 2, xlab = "", xlim = (nrow(x) * c(0, skel.width)),
  ylim = c(0, skel.height), col = NULL, mtext = TRUE, skel.lwd = 1, ...)

jd()
}
\arguments{
  \item{x}{Matrix or data.frame of skeleton data.}

  \item{skel.width}{Reference width for instance
  calculation.}

  \item{skel.height}{Reference height for instance
  calculation.}

  \item{base.radius}{Base radius for points.}

  \item{xlab}{The x label of the plot.}

  \item{ylab}{The y label of the plot.}

  \item{xlim}{Numeric of length 2 giving the x limits for
  the plot.}

  \item{ylim}{Numeric of length 2 giving the y limits for
  the plot.}

  \item{col}{Color of the different parts of the skeleton.}

  \item{mtext}{Label archetypes.}

  \item{skel.lwd}{Line width of skeleton.}

  \item{...}{Passed to underlying canvas plot function.}
}
\value{
List of skeleton instances.

Generic skeleton instance.
}
\description{
Displays a schematic representation of skeleton data as
available in dataset \code{\link{skel}}.

Displays a generic skeleton with annotations explaining the
measurements available in data set \code{\link{skel}}.
}
\seealso{
\code{\link{skel}}
}

