

#' Archetypes getter.
#'
#' replaced by \code{\link{parameters}}.
#'
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Archetypes matrix.
#' @export
#' @rdname archetypes-deprecated
atypes <- function(zs, ...) {
  .Deprecated('parameters')
  UseMethod('atypes')
}

#' @S3method atypes archetypes
#' @nord
atypes.archetypes <- function(zs, ...) {
  return(zs$archetypes)
}

#' @S3method atypes stepArchetypes
#' @nord
atypes.stepArchetypes <- function(zs, ...) {
  return(lapply(zs, atypes))
}

#' @S3method atypes repArchetypes
#' @nord
atypes.repArchetypes <- function(zs, ...) {
  lapply(zs, atypes)
}


#' Number of archetypes getter.
#'
#' replaced by \code{\link{nparameters}}.
#'
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Number of archetypes.
#' @export
#' @rdname archetypes-deprecated
ntypes <- function(zs, ...) {
  .Deprecated('nparameters')
  UseMethod('ntypes')
}

#' @S3method atypes archetypes
#' @nord
ntypes.archetypes <- function(zs, ...) {
  return(zs$k)
}

#' @S3method ntypes stepArchetypes
#' @nord
ntypes.stepArchetypes <- function(zs, ...) {
  return(sapply(zs, ntypes))
}

#' @S3method ntypes repArchetypes
#' @nord
ntypes.repArchetypes <- function(zs, ...) {
  ntypes(zs[[1]])
}



#' Archetypes data approximation.
#'
#' replaced by \code{\link{fitted}}.
#'
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Approximated data matrix.
#' @export
#' @rdname archetypes-deprecated
adata <- function(zs, ...) {
  .Deprecated('fitted')
  UseMethod('adata')
}

#' @S3method adata archetypes
#' @nord
adata.archetypes <- function(zs, ...) {
  return(t(t(zs$archetypes) %*% t(zs$alphas)))
}



#' Alpha getter.
#'
#' replaced by \code{\link{coef}}.
#'
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Alpha matrix.
#' @export
#' @rdname archetypes-deprecated
alphas <- function(zs, ...) {
  .Deprecated('coef')
  UseMethod('alphas')
}

#' @S3method alphas archetypes
#' @nord
alphas.archetypes <- function(zs, ...) {
  return(zs$alphas)
}



#' Beta getter.
#'
#' replaced by \code{\link{coef}}.
#'
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Beta matrix.
#' @export
#' @rdname archetypes-deprecated
betas <- function(zs, ...) {
  .Deprecated('coef')
  UseMethod('betas')
}

#' @S3method betas archetypes
#' @nord
betas.archetypes <- function(zs, ...) {
  return(zs$betas)
}



#' Iteration getter.
#'
#' removed.
#'
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return Number of iterations.
#' @export
#' @rdname archetypes-deprecated
iters <- function(zs, ...) {
  .Deprecated()
  UseMethod('iters')
}

#' @S3method iters archetypes
#' @nord
iters.archetypes <- function(zs, ...) {
  return(zs$iters)
}



#' Archetypes history getter.
#'
#' removed; see \code{\link{memento}}.
#'
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return The \code{archetypes} object of the requested step.
#' @export
#' @rdname archetypes-deprecated
ahistory <- function(zs, ...) {
  .Deprecated('memento')
  UseMethod('ahistory')
}


#' @S3method ahistory archetypes
#' @nord
ahistory.archetypes <- function(zs, step, ...) {
  if ( is.null(zs$history) )
    stop('No history available')

  if ( step >= 0 )
    s <- paste('s', step, sep='')
  else
    s <- paste('s', nhistory(zs) + step - 1, sep='')

  return(zs$history[[s]][[1]])
}



#' Number of history steps getter.
#'
#' removed; see \code{\link{memento}}.
#'
#' @param zs An \code{archetypes}-related object.
#' @param ... Further arguments.
#' @return The number of history steps available.
#' @export
#' @rdname archetypes-deprecated
nhistory <- function(zs, ...) {
  .Deprecated('memento')
  UseMethod('nhistory')
}


#' @S3method nhistory archetypes
#' @nord
nhistory.archetypes <- function(zs, ...) {
  if ( is.null(zs$history) )
    stop('No history available')

  return(length(zs$history))
}

