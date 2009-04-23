\name{skeletonplot}
\title{Skeleton plot.}
\usage{skeletonplot(x, skel.width=100, skel.height=200, base.radius=2, xlab='',
ylab='Height (cm)', xlim=(nrow(x) * c(0, skel.width)), ylim=c(0,
skel.height), col=c(hipbase = 1, hip = 1, shoulderbase = 1,
shoulder = 1, head = 1, elbow=2, wrist=3, knee=4, ankle=5,
chest='purple1', pelvis=6), mtext=TRUE, skel.lwd=1, ...)}
\description{Skeleton plot.}
\details{Displays a schematic representation of skeleton data as available
in dataset \code{\link{skel}}.}
\value{List of skeleton instances.}
\seealso{\code{\link{skel}}, \code{\link{jd}}}
\alias{skeletonplot}
\arguments{\item{x}{Matrix or data.frame of skeleton data.}
\item{skel.width}{Reference width for instance calculation.}
\item{skel.height}{Reference height for instance calculation.}
\item{base.radius}{Base radius for points.}
\item{xlab}{The x label of the plot.}
\item{ylab}{The y label of the plot.}
\item{xlim}{Numeric of length 2 giving the x limits for the plot.}
\item{ylim}{Numeric of length 2 giving the y limits for the plot.}
\item{col}{Color of the different parts of the skeleton.}
\item{mtext}{Label archetypes.}
\item{skel.lwd}{Line width of skeleton.}
\item{...}{Passed to underlying canvas plot function.}}
