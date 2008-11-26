

#' Barplot of archetypes.
#' @param height An \code{\link{archetypes}} object.
#' @param data The original data matrix.
#' @param beside Portray the archetypes as stacked bars.
#' @param percentage Show real values or percentages according to the
#'    original data.
#' @param ... Passed to the underlying \code{\link{barplot}} call.
#' @return Undefined.
#' @method barplot archetypes
#' @importFrom graphics barplot
#' @export
barplot.archetypes <- function(height, data, beside=TRUE, percentage=FALSE, ...) {
  atypes <- atypes(height)

  if ( !percentage ) {
    ylab <- 'Value'
    ylim <- NULL
  }
  else {
    m <- sapply(data, max)
    atypes <- t(t(atypes) / m * 100)
    ylab <- 'Percentage'
    ylim <- c(0,100)
  }  

  if ( beside ) {
    barplot(atypes, ylab=ylab, beside=TRUE, ylim=ylim, ...)
  }
  else {
    p <- nrow(atypes)
    
    par(mfrow=c(p,1))
    for ( i in 1:p ) 
      barplot(atypes[i,], main=paste('Archetype', i),
              ylab=ylab, ylim=ylim, ...)
  }  
}

