#' @include archetypes-kit.R
{}


#' Runs archetypes algorithm repeatedly.
#' @param ... Passed to \code{\link{archetypes}} function.
#' @param k A vector of integers passed in turn to the k argument of
#'   \code{\link{archetypes}}.
#' @param nrep For each value of \code{k} run \code{\link{archetypes}}
#'   \code{nrep} times.
#' @param method Archetypes function to use, typically
#'   \code{\link{archetypes}}, \code{\link{weightedArchetypes}} or
#'   \code{\link{robustArchetypes}},
#' @param verbose Show progress during exection.
#' @return A list with \code{k} elements and class attribute
#'   \code{stepArchetypes}. Each element is a list of class
#'   \code{repArchetypes} with \code{nrep} elements; only for internal
#'   usage.
#' @seealso \code{\link{archetypes}}
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
stepArchetypes <- function(..., k, nrep = 3, method = archetypes, verbose = TRUE) {

  mycall <- match.call()
  as <- list()

  for ( i in 1:length(k) ) {
    as[[i]] <- list()
    class(as[[i]]) <- 'repArchetypes'

    for ( j in seq_len(nrep) ) {
      if ( verbose )
        cat('\n*** k=', k[i], ', rep=', j, ':\n', sep='')

      as[[i]][[j]] <- method(..., k=k[i])
    }
  }

  return(structure(as, class='stepArchetypes', call=mycall))
}



setOldClass('repArchetypes')
setOldClass('stepArchetypes')



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
#' @method [ stepArchetypes
#' @rdname stepArchetypes
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
#' @nord
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
#' @rdname stepArchetypes
summary.stepArchetypes <- function(object, ...) {
  print(object)

  ps <- nparameters(object)

  for ( i in seq_along(object) ) {
    cat('\nk=', ps[i], ':\n', sep='')
    print(object[[i]], full=FALSE)
  }
}



#' Return fitted archetypes.
#' @param object A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return A list of archetypes matrices.
#' @nord
.parameters.stepArchetypes <- function(object, ...) {
  return(lapply(object, parameters))
}

#' Return fitted archetypes.
#' @param object An \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return List of archetypes.
#' @importFrom modeltools parameters
#' @nord
setMethod('parameters',
          signature = signature(object = 'stepArchetypes'),
          .parameters.stepArchetypes)



#' Number of parameters.
#' @param object A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return Vector of numbers of archetypes.
#' @method nparameters stepArchetypes
#' @S3method nparameters stepArchetypes
#' @rdname stepArchetypes
nparameters.stepArchetypes <- function(object, ...) {
  return(sapply(object, nparameters))
}



#' Archetypes residual sum of squares getter.
#' @param object A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return A vector of residual sum of squares.
#' @method rss stepArchetypes
#' @S3method rss stepArchetypes
#' @rdname stepArchetypes
rss.stepArchetypes <- function(object, ...) {
  ret <- t(sapply(object, rss))
  rownames(ret) <- paste('k', nparameters(object), sep='')
  return(ret)
}



#' Best model getter.
#' @param object An object.
#' @param ... Further arguments.
#' @return The best models.
#' @export
#' @rdname archetypes-generics
bestModel <- function(object, ...) {
  UseMethod('bestModel')
}

#' \code{stepArchetypes} best model getter.
#' @param object A \code{stepArchetypes} object.
#' @param ... Ignored.
#' @return A list of length \code{k} of best models.
#' @method bestModel stepArchetypes
#' @S3method bestModel stepArchetypes
#' @rdname stepArchetypes
bestModel.stepArchetypes <- function(object, ...) {
  zsmin <- lapply(object, bestModel)

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
#' @nord
print.repArchetypes <- function(x, ...) {
  for ( i in seq_along(x) )
    print(x[[i]], ...)

  invisible(x)
}



#' Return fitted archetypes.
#' @param object A \code{repArchetypes} object.
#' @param ... Ignored.
#' @return A list of archetypes matrices.
#' @nord
.parameters.repArchetypes <- function(object, ...) {
  lapply(object, parameters)
}

#' Return fitted archetypes.
#' @param object An \code{repArchetypes} object.
#' @param ... Ignored.
#' @return List of archetypes.
#' @importFrom modeltools parameters
#' @nord
setMethod('parameters',
          signature = signature(object = 'repArchetypes'),
          .parameters.repArchetypes)



#' Archetypes residual sum of squares getter.
#' @param object A \code{repArchetypes} object.
#' @param ... Ignored.
#' @return A vector of residual sum of squares.
#' @method rss repArchetypes
#' @S3method rss repArchetypes
#' @nord
rss.repArchetypes <- function(object, ...) {
  ret <- sapply(object, rss)
  names(ret) <- paste('r', seq_along(ret), sep='')

  return(ret)
}


#' Number of archetypes.
#' @param object A \code{repArchetypes} object.
#' @param ... Ignored.
#' @return Vector of numbers of archetypes.
#' @method nparameters repArchetypes
#' @S3method nparameters repArchetypes
#' @nord
nparameters.repArchetypes <- function(object, ...) {
  nparameters(object[[1]])
}



#' \code{repArchetypes} best model getter.
#' @param object A \code{repArchetypes} object.
#' @param ... Ignored.
#' @return The best model.
#' @method bestModel repArchetypes
#' @S3method bestModel repArchetypes
#' @nord
bestModel.repArchetypes <- function(object, ...) {
  m <- which.min(rss(object))

  if ( length(m) == 0 )
    return(object[[1]])
  else
    return(object[[m]])
}


