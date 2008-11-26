

#' Archetypes plot movie.
#' @param zs An \code{\link{archetypes}} object.
#' @param data The data matrix.
#' @param show Show archetypes or approximated data.
#' @param ssleep Seconds to sleep before start.
#' @param bsleep Seconds to sleep between each plot.
#' @param ... Passed to underlying plot functions.
#' @return Undefined.
#' @export
movieplot <- function(zs, data, show=c('atypes', 'adata'),
                      ssleep=0, bsleep=0, ...) {
  
  steps <- length(zs$history)
  atypesmovie <- ifelse(show[1] == 'atypes', TRUE, FALSE)

  Sys.sleep(ssleep)

  # ... and play:
  for ( i in seq_len(steps)-1 ) {
    a <- ahistory(zs, step=i)
    if ( atypesmovie )
      plot(a, data, ...)
    else
      plot(adata(a), ...)
    
    Sys.sleep(bsleep)
  }
}



#' Archetypes parallel coordinates plot movie.
#' @param zs An \code{\link{archetypes}} object.
#' @param data The data matrix.
#' @param show Show archetypes or approximated data.
#' @param ssleep Seconds to sleep before start.
#' @param bsleep Seconds to sleep between each plot.
#' @param ... Passed to underlying pcplot functions.
#' @return Undefined.
#' @export
moviepcplot <- function(zs, data, show=c('atypes', 'adata'),
                        ssleep=0, bsleep=0, ...) {

  steps <- length(zs$history)
  atypesmovie <- ifelse(show[1] == 'atypes', TRUE, FALSE)
  rx <- apply(data, 2, range, na.rm=TRUE)

  Sys.sleep(ssleep)

  # ... and play:
  for ( i in seq_len(steps)-1 ) {
    a <- ahistory(zs, step=i)

    if ( atypesmovie )
      pcplot(a, data, ...)
    else
      pcplot(adata(a), rx=rx, ...)
    
    Sys.sleep(bsleep)
  }
}
