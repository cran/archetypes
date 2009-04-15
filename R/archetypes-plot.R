

#' Helper function to calculate the approximated convex hull.
#' @param zs An \code{archetypes} object.
#' @return Matrix with the points.
ahull <- function(zs) {
  a <- rbind(atypes(zs), atypes(zs)[1,])
  xc <- a[,1]; xm <- mean(xc)
  yc <- a[,2]; ym <- mean(yc)
  
  real <- xc - xm
  imag <- yc - ym
  angle <- atan2(imag, real)
  
  index <- order(angle)
  
  return(a[c(index, index[1]),])
}



#' Plot of data and archetypes.
#' @param x An \code{\link{archetypes}} object.
#' @param y A matrix or data frame.
#' @param data.col Color of data points.
#' @param data.pch Type of data points.
#' @param atypes.col Color of archetypes points.
#' @param atypes.pch Type of archetypes points.
#' @param ahull.show Show approximated convex hull.
#' @param ahull.col Color of approximated convex hull line.
#' @param chull An integer vector giving the indices of the points from
#'   \code{data} lying on the convex hull.
#' @param chull.col Color of convex hull points.
#' @param chull.pch Type of convex hull points.
#' @param adata.show Show approximated data with link to the original
#'   data.
#' @param adata.col Color of approximated data points.
#' @param adata.pch Type of approximated data points.
#' @param link.col Color of link between approximated and original data
#'   points.
#' @param ... Passed to the underlying plot functions.
#' @return Undefined.
#' @note The link between approximated and original data is based on an
#'   idea and Matlab source code of Bernard Pailthorpe.
#' @method plot archetypes
#' @export
#' @noRd
plot.archetypes <- function(x, y,
                            data.col=gray(0.7), data.pch=19,
                            atypes.col=2, atypes.pch=19,
                            ahull.show=TRUE, ahull.col=atypes.col,
                            chull=NULL, chull.col=1, chull.pch=19,
                            adata.show=FALSE, adata.col=3, adata.pch=13,
                            link.col=data.col, ...) {

  zs <- x; data <- y;

  plot(data, col=data.col, pch=data.pch, ...)
  points(atypes(zs), col=atypes.col, pch=atypes.pch, ...)

  if ( !is.null(chull) ) {
    points(data[chull,], col=chull.col, pch=chull.pch, ...)
    lines(data[c(chull, chull[1]),], col=chull.col, ...)
  }

  if ( ahull.show )
    lines(ahull(zs), col=ahull.col)


  if ( adata.show ) {
    ### Based on an idea of Bernard Pailthorpe.
    adata <- adata(zs)
    
    points(adata, col=adata.col, pch=adata.pch, ...)
    for ( i in seq_len(nrow(data)) )
      lines(rbind(data[i,], adata[i,]), col=link.col, ...)
  }
}



#' Plot of data and stepArchetypes.
#' @param x An \code{\link{stepArchetypes}} object.
#' @param y A matrix or data frame.
#' @param data.col Color of data points.
#' @param data.pch Type of data points.
#' @param atypes.col Color of archetypes points.
#' @param atypes.pch Type of archetypes points.
#' @param ahull.show Show approximated convex hull.
#' @param ahull.col Color of approximated convex hull line.
#' @param ... Passed to the underlying plot functions.
#' @return Undefined.
#' @method plot stepArchetypes
#' @export
#' @noRd
plot.stepArchetypes <- function(x, y,
                                data.col=gray(0.7), data.pch=19,
                                atypes.col=(seq_len(length(x) * length(x[[1]]))+1),
                                atypes.pch=19, ahull.show=TRUE, ahull.col=atypes.col, ...) {
  
  zs <- x; data <- y;
  
  flatzs <- unlist(zs, recursive=FALSE)
  
  plot(data, col=data.col, pch=data.pch, ...)
  for ( i in seq_along(flatzs) ) {
    a <- flatzs[[i]]
    points(atypes(a), col=atypes.col[i], pch=atypes.pch, ...)

    if ( ahull.show )
      lines(ahull(a), col=ahull.col[i])
  }
}


