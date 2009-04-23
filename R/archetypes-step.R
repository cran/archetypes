

#' Runs archetypes algorithm repeatedly.
#' @param ... Passed to \code{\link{archetypes}} function.
#' @param k A vector of integers passed in turn to the k argument of
#'   \code{\link{archetypes}}.
#' @param nrep For each value of \code{k} run \code{\link{archetypes}}
#'   \code{nrep} times.
#' @param verbose Show progress during exection.
#' @return A list with \code{k} elements and class attribute
#'   \code{stepArchetypes}. Each element is a list of class
#'   \code{repArchetypes} with \code{nrep} elements; only for internal
#'   usage.
#' @seealso \code{\link{atypes}}, \code{\link{ntypes}},
#'   \code{\link{rss}}, \code{\link{adata}}, \code{\link{alphas}},
#'   \code{\link{ahistory}}, \code{\link{nhistory}}
#' @export
#' @examples
#'   \dontrun{
#'   data(skel)
#'   skel2 <- subset(skel, select=-Gender)
#'   as <- stepArchetypes(skel2, k=1:5, verbose=FALSE)
#'
#'   ## Residual sum of squares curve:
#'   screeplot(as)
#'
#'   ## Select three archetypes and from that the best
#'   ## recurrence:
#'   a3 <- bestModel(as[[3]])
#'   }
#' @note Please see the vignette for a detailed explanation!
stepArchetypes <- function(..., k, nrep=3, verbose=TRUE) {

  mycall <- match.call()
  as <- list()
  
  for ( i in 1:length(k) ) { 
    as[[i]] <- list()
    class(as[[i]]) <- 'repArchetypes'
    
    for ( j in seq_len(nrep) ) {
      if ( verbose )
        cat('\n*** k=', k[i], ', rep=', j, ':\n', sep='')
      
      as[[i]][[j]] <- archetypes(..., k=k[i], verbose=verbose)
    }
  }

  return(structure(as, class='stepArchetypes', call=mycall))
}



#' Extract method.
#'
#' An extraction on a \code{stepArchetypes} object returns again a
#' \code{stepArchetypes} object.
#'
#' @param x A \code{stepArchetypes} object.
#' @param i The indizes to extract.
#' @return A \code{stepArchetypes} object containing only the parts
#'   defined in \code{i}.
#' @S3method "[" stepArchetypes
`[.stepArchetypes` <- function(x, i) {
  y <- unclass(x)[i]
  attributes(y) <- attributes(x)
  
  return(y)
}



#' Print method for stepArchetypes object.
#' @param x A \code{stepArchetypes} object.
#' @param ... Pass to underlying print function.
#' @return Undefined.
#' @method print stepArchetypes
#' @S3method print stepArchetypes
print.stepArchetypes <- function(x, ...) {
  cat('StepArchetypes object\n\n')
  cat(deparse(attr(x, 'call')), '\n')
}



#' Summary method for stepArchetypes object.
#' @param object A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return Undefined.
#' @method summary stepArchetypes
#' @S3method summary stepArchetypes
summary.stepArchetypes <- function(object, ...) {
  print(object)
  
  ps <- ntypes(object)

  for ( i in seq_along(object) ) {
    cat('\nk=', ps[i], ':\n', sep='')
    print(object[[i]], full=FALSE)
  }
}



#' Archetypes getter.
#' @param zs A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return A list of archetypes matrices.
#' @method atypes stepArchetypes
#' @S3method atypes stepArchetypes
atypes.stepArchetypes <- function(zs, ...) {
  return(lapply(zs, atypes))
}



#' Number of archetypes getter.
#' @param zs A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return Vector of numbers of archetypes.
#' @method ntypes stepArchetypes
#' @S3method ntypes stepArchetypes
ntypes.stepArchetypes <- function(zs, ...) {
  return(sapply(zs, ntypes))
}



#' Archetypes residual sum of squares getter.
#' @param zs A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return A vector of residual sum of squares.
#' @method rss stepArchetypes
#' @S3method rss stepArchetypes
rss.stepArchetypes <- function(zs, ...) {
  ret <- t(sapply(zs, rss))
  rownames(ret) <- paste('k', ntypes(zs), sep='')
  return(ret)
}


#' Iteration getter.
#' @param zs A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return A matrix of iterations.
#' @method iters stepArchetypes
#' @S3method iters stepArchetypes
iters.stepArchetypes <- function(zs, ...) {
  ret <- t(sapply(zs, iters))
  rownames(ret) <- paste('k', ntypes(zs), sep='')

  return(ret)
}



#' Best model getter.
#' @param zs An \code{\link{stepArchetypes}} object.
#' @param ... Further arguments.
#' @return A list of length \code{k} of best models.
#' @export
bestModel <- function(zs, ...) {
  UseMethod('bestModel')
}

#' \code{stepArchetypes} best model getter.
#' @param zs A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return A list of length \code{k} of best models.
#' @method bestModel stepArchetypes
#' @S3method bestModel stepArchetypes
bestModel.stepArchetypes <- function(zs, ...) {
  zsmin <- lapply(zs, bestModel)

  if ( length(zsmin) == 1 )
    return(zsmin[[1]])
  else
    return(zsmin)
}



#' Print method for repArchetypes object.
#' @param x A \code{repArchetypes} object.
#' @param ... Pass to underlying print function.
#' @return Undefined.
#' @method print repArchetypes
#' @S3method print repArchetypes
print.repArchetypes <- function(x, ...) {
  for ( i in seq_along(x) )
    print(x[[i]], ...)
}



#' Archetypes getter.
#' @param zs A \code{repArchetypes} object.
#' @param ... Ignored.
#' @return A list of archetypes matrices.
#' @method atypes repArchetypes
#' @S3method atypes repArchetypes
atypes.repArchetypes <- function(zs, ...) {
  ret <- lapply(zs, atypes)
  return(ret)
}



#' Archetypes residual sum of squares getter.
#' @param zs A \code{repArchetypes} object.
#' @param ... Ignored.
#' @return A vector of residual sum of squares.
#' @method rss repArchetypes
#' @S3method rss repArchetypes
rss.repArchetypes <- function(zs, ...) {
  ret <- sapply(zs, rss)
  names(ret) <- paste('r', seq_along(ret), sep='')

  return(ret)
}


#' Number of archetypes getter.
#' @param zs A \code{repArchetypes} object.
#' @param ... Ignored.
#' @return Vector of numbers of archetypes.
#' @method ntypes repArchetypes
#' @S3method ntypes repArchetypes
ntypes.repArchetypes <- function(zs, ...) {
  return(ntypes(zs[[1]]))
}


#' Iteration getter.
#' @param zs A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return A matrix of iterations.
#' @method iters repArchetypes
#' @S3method iters repArchetypes
iters.repArchetypes <- function(zs, ...) {
  ret <- sapply(zs, iters)
  names(ret) <- paste('r', seq_along(ret), sep='')
  
  return(ret)
}



#' \code{repArchetypes} best model getter.
#' @param zs A \code{repArchetypes} object.
#' @param ... Ignored.
#' @return The best model.
#' @method bestModel repArchetypes
#' @S3method bestModel repArchetypes
bestModel.repArchetypes <- function(zs, ...) {
  m <- which.min(rss(zs))

  if ( length(m) == 0 )
    return(zs[[1]])
  else
    return(zs[[m]])
}


